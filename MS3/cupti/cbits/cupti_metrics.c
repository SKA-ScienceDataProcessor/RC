
#include <cuda.h>

#include <cupti_events.h>
#include <cupti_metrics.h>
#include <cupti_callbacks.h>
#include <cupti_driver_cbid.h>

#include <stdio.h>

#include <pthread.h>

// The collected metrics
#define MAX_METRICS 10
char *metricNames[MAX_METRICS];
int metricCount = 0;
uint64_t metrics[MAX_METRICS];

// Callback user data. None for now.
struct context_counter_data {
    CUcontext context;
    CUpti_EventGroupSets *eventGroupSets;
    CUpti_MetricID metricIds[MAX_METRICS];
    CUpti_MetricValueKind metricKinds[MAX_METRICS];
    struct context_counter_data *next;
};
struct context_counter_data *allCounterData;
pthread_mutex_t mutex;

// The hooked callbacks
typedef void cupti_user_t;
static void cupti_callback(cupti_user_t *user, CUpti_CallbackDomain domain,
                           CUpti_CallbackId cbid, void *cbdata);
CUpti_SubscriberHandle subscriber = NULL;

#define CUPTI_CALL(call)                            \
    if (CUPTI_SUCCESS != (res = call)) return res;

int initialised = 0;

void cupti_metrics_init(const char *metricList)
{
    if (initialised) return;

    // Split and copy metric names
    int count = 0;
    const char *start = metricList;
    while(count < MAX_METRICS) {
        const char *end = strchr(start, ',');
        if (!end) break;
        int len = end - start;
        metricNames[count] = malloc(len + 1);
        memcpy(metricNames[count], start, len);
        metricNames[count][len] = '\0';
        start = end + 1;
        count++;
    }
    if (count < MAX_METRICS) {
        int len = strlen(start);
        metricNames[count] = malloc(len + 1);
        strcpy(metricNames[count], start);
        metricCount = count + 1;
    } else {
        fprintf(stderr, "CUPTI warning: Too many metrics enabled. If you are sure that this is what you want, increase MAX_METRICS to make this work.");
    }

    // Initialise mutex
    pthread_mutex_init(&mutex, NULL);
    initialised = 1;

}

int cupti_metrics_enable()
{
    CUptiResult res;

    if (!initialised || !metricCount) {
        return 0;
    }

    // We cannot initialise anything here - we only hook CUDA context
    // creation in order to do the actual work later.
    if (subscriber == NULL) {

        // Create subscription
        CUPTI_CALL(cuptiSubscribe(&subscriber, (CUpti_CallbackFunc)cupti_callback,
                                  NULL));

        // Bind callbacks
        CUPTI_CALL(cuptiEnableCallback(1, subscriber, CUPTI_CB_DOMAIN_RESOURCE,
                                       CUPTI_CBID_RESOURCE_CONTEXT_CREATED));
        CUPTI_CALL(cuptiEnableCallback(1, subscriber, CUPTI_CB_DOMAIN_RESOURCE,
                                       CUPTI_CBID_RESOURCE_CONTEXT_DESTROY_STARTING));

        // Hook kernel launch. This feels hacky, but is the approach
        // callback_metric.cu takes.
        CUPTI_CALL(cuptiEnableCallback(1, subscriber, CUPTI_CB_DOMAIN_DRIVER_API, 
                                       CUPTI_DRIVER_TRACE_CBID_cuLaunchKernel));
    }

}

void cupti_assert(int loc, CUptiResult res)
{
    if (CUPTI_SUCCESS == res) return;

    // Get error message
    const char *err = "Unknown error";
    cuptiGetResultString(res, &err);

    // Output. Don't do anything else - we expect this to not happen,
    // and it's certainly not worth crashing the program for.
    fprintf(stderr, "Unexpected CUPTI error at cupti_metrics.c:%d: %s\n", loc, err);
}

#define CUPTI_ASSERT(res) cupti_assert(__LINE__, res)

void cupti_metrics_disable()
{
    if (subscriber != NULL) {
        CUPTI_ASSERT(cuptiUnsubscribe(subscriber)); subscriber = NULL;
    }
}

int cupti_get_metrics_count()
{
    return metricCount;
}

uint64_t cupti_get_metric(int i)
{
    if (i < 0 || i >= metricCount) {
        fprintf(stderr, "CUPTI warning: Invalid metric number %d\n", i);
        return 0;
    }
    return metrics[i];
}

static CUdevice get_device_from_ctx(CUcontext ctx)
{

    // Strangely, there does not seem to be a way to get this from the
    // context without making it current. Feels hacky, possibly
    // subject to future change.
    CUcontext curCtx = 0;
    CUdevice device = 0;
    cuCtxGetCurrent(&curCtx);
    if (curCtx != ctx) {
        cuCtxPushCurrent(ctx);
    }
    cuCtxGetDevice(&device);
    if (curCtx != ctx) {
        cuCtxPopCurrent(NULL);
    }

    return device;
}

static void cupti_callback_context_created(cupti_user_t *user,
                                           CUpti_ResourceData *cbdata)
{
    CUdevice device = get_device_from_ctx(cbdata->context);

    // Translate metric names to IDs
    struct context_counter_data *counter_data =
        malloc(sizeof(struct context_counter_data));
    int i;
    for (i = 0; i < metricCount; i++) {

        // Look up metric ID
        counter_data->metricIds[i] = 0;
        CUPTI_ASSERT(cuptiMetricGetIdFromName(device, metricNames[i],
                                              counter_data->metricIds + i));

        // Query metric kind, we will need that later
        size_t metricKindSize = sizeof(CUpti_MetricID);
        CUPTI_ASSERT(cuptiMetricGetAttribute(counter_data->metricIds[i],
                                             CUPTI_METRIC_ATTR_VALUE_KIND,
                                             &metricKindSize,
                                             counter_data->metricKinds + i));
        switch (counter_data->metricKinds[i]) {
        case CUPTI_METRIC_VALUE_KIND_UINT64:
        case CUPTI_METRIC_VALUE_KIND_INT64:
            break;
        default:
            fprintf(stderr, "CUPTI warning: Metric '%s' has kind '%d', inaccurate results to be expected!\n", metricNames[i], counter_data->metricKinds[i]);
        }
    }

    // Create event group sets
    CUpti_EventGroupSets *eventGroupPasses = NULL;
    CUPTI_ASSERT(cuptiMetricCreateEventGroupSets(cbdata->context,
                                                 metricCount * sizeof(CUpti_MetricID),
                                                 counter_data->metricIds,
                                                 &eventGroupPasses));

    // Make sure we only have one pass. Warn otherwise.
    if (eventGroupPasses->numSets > 1) {
        fprintf(stderr, "CUPTI warning: Calculating metrics would require %d passes! We cannot do that!\n",
                eventGroupPasses->numSets);
    }
    if (!eventGroupPasses->numSets)
        return;

    // Make sure we are not using instrumentation.
    for (i = 0; i < eventGroupPasses->sets->numEventGroups; i++) {
        CUpti_EventDomainID domainId = 0;
        size_t domainIdSize = sizeof(domainId);
        CUPTI_ASSERT(cuptiEventGroupGetAttribute(eventGroupPasses->sets->eventGroups[i],
                                                 CUPTI_EVENT_GROUP_ATTR_EVENT_DOMAIN_ID,
                                                 &domainIdSize, &domainId));
        CUpti_EventCollectionMethod collection;
        size_t collectionSize = sizeof(collection);
        CUPTI_ASSERT(cuptiDeviceGetEventDomainAttribute(device, domainId,
                                                        CUPTI_EVENT_DOMAIN_ATTR_COLLECTION_METHOD,
                                                        &collectionSize, &collection));

        if (collection == CUPTI_EVENT_COLLECTION_METHOD_INSTRUMENTED) {
            fprintf(stderr, "CUPTI warning: Metrics use instrumented events, performance degradation likely!\n");
        }
    }

    // Save back
    pthread_mutex_lock(&mutex);
    counter_data->context = cbdata->context;
    counter_data->eventGroupSets = eventGroupPasses;
    counter_data->next = allCounterData;
    allCounterData = counter_data;
    pthread_mutex_unlock(&mutex);
}

static void cupti_callback_context_destroy_starting(cupti_user_t *user,
                                                    CUpti_ResourceData *cbdata)
{

    // Find associated counter data
    pthread_mutex_lock(&mutex);
    struct context_counter_data *counter_data, *prev = NULL;
    for (counter_data = allCounterData; counter_data; counter_data = counter_data->next) {
        if (counter_data->context == cbdata->context) {
            break;
        }
        prev = counter_data;
    }
    if (!counter_data) {
        // This can happen if a context gets destroyed that existed
        // before our callbacks got established (FIXME?). Furthermore,
        // the docs sound like it's unreliable to depend on context
        // identity - this might also cause this.
        fprintf(stderr, "CUPTI warning: Unmatched destroyed context!\n");
        pthread_mutex_unlock(&mutex);
        return;
    }

    // Disable counters
    int i;
    CUpti_EventGroupSets *eventGroupPasses = counter_data->eventGroupSets;
    for (i = 0; i < eventGroupPasses->sets->numEventGroups; i++) {
        CUPTI_ASSERT(cuptiEventGroupDisable(eventGroupPasses->sets->eventGroups[i]));
    }

    // Free event group
    CUPTI_ASSERT(cuptiEventGroupSetsDestroy(eventGroupPasses));

    // Free
    if (prev) {
        prev->next = counter_data->next;
    } else {
        allCounterData = counter_data->next;
    }
    free(counter_data);
    pthread_mutex_unlock(&mutex);
}

static void cupti_callback_launch_kernel(cupti_user_t *user, CUpti_CallbackData *cbdata)
{

    // Find associated counter data
    pthread_mutex_lock(&mutex);
    struct context_counter_data *counter_data;
    for (counter_data = allCounterData; counter_data; counter_data = counter_data->next) {
        if (counter_data->context == cbdata->context) {
            break;
        }
    }
    pthread_mutex_unlock(&mutex);
    if (!counter_data) {
        if (cbdata->callbackSite == CUPTI_API_ENTER) {
            fprintf(stderr, "CUPTI warning: Could not find context for kernel start!\n");
            // Simply generate it. Use user data as a cheap way to
            // prevent an infinite loop.
            if (user) {
                cupti_callback_context_created(user, cbdata);
                cupti_callback_launch_kernel(NULL, cbdata);
            }
        }
        return;
    }
    CUpti_EventGroupSets *eventGroupPasses = counter_data->eventGroupSets;

    if (cbdata->callbackSite == CUPTI_API_ENTER) {
        //cudaDeviceSynchronize();

        // Set collection mode. Kernel mode is the only one that is
        // guaranteed to work, even if it forces us to sum up metrics
        // manually.
        CUPTI_ASSERT(cuptiSetEventCollectionMode(cbdata->context,
                                                 CUPTI_EVENT_COLLECTION_MODE_KERNEL));

        // Enable the counters!
        int i;
        for (i = 0; i < eventGroupPasses->sets->numEventGroups; i++) {
            uint32_t all = 1;
            CUPTI_ASSERT(cuptiEventGroupSetAttribute(eventGroupPasses->sets->eventGroups[i],
                                                     CUPTI_EVENT_GROUP_ATTR_PROFILE_ALL_DOMAIN_INSTANCES,
                                                     sizeof(all), &all));
            CUPTI_ASSERT(cuptiEventGroupEnable(eventGroupPasses->sets->eventGroups[i]));
        }
    }

    else if (cbdata->callbackSite == CUPTI_API_EXIT) {
        CUdevice device = get_device_from_ctx(cbdata->context);

        // Find out how many events we have in total. Note that
        // cuptiMetricGetNumEvents wouldn't help us here, as we are
        // collecting multiple metrics, which *might* have overlapping
        // events.
        uint32_t numEvents = 0; int i;
        for (i = 0; i < eventGroupPasses->sets->numEventGroups; i++) {
            uint32_t num = 0;
            size_t numSize = sizeof(num);
            CUPTI_ASSERT(cuptiEventGroupGetAttribute(eventGroupPasses->sets->eventGroups[i],
                                                     CUPTI_EVENT_GROUP_ATTR_NUM_EVENTS,
                                                     &numSize, &num));
            numEvents += num;
        }

        // Allocate arrays for event IDs & values
        size_t eventIdsSize = sizeof(CUpti_EventID) * numEvents;
        CUpti_EventID *eventIds = (CUpti_EventID *)alloca(eventIdsSize);
        size_t eventValuesSize = sizeof(uint64_t) * numEvents;
        uint64_t *eventValues = (uint64_t *)alloca(eventValuesSize);
        memset(eventValues, 0, sizeof(uint64_t) * numEvents);

        // Now read all events, per group
        int eventIx = 0;
        for (i = 0; i < eventGroupPasses->sets->numEventGroups; i++) {
            CUpti_EventGroup eventGroup = eventGroupPasses->sets->eventGroups[i];

            // Get event IDs
            uint32_t num = 0;
            size_t numSize = sizeof(num);
            CUPTI_ASSERT(cuptiEventGroupGetAttribute(eventGroup,
                                                     CUPTI_EVENT_GROUP_ATTR_NUM_EVENTS,
                                                     &numSize, &num));

            // Get how many domain instances were actually counting
            uint32_t domInstNum = 0;
            size_t domInstNumSize = sizeof(domInstNum);
            CUPTI_ASSERT(cuptiEventGroupGetAttribute(eventGroup,
                                                     CUPTI_EVENT_GROUP_ATTR_INSTANCE_COUNT,
                                                     &domInstNumSize, &domInstNum));

            // Get counter values from all instances
            size_t idsSize = sizeof(CUpti_EventID) * num;
            size_t valsSize = sizeof(uint64_t) * num * domInstNum;
            uint64_t *vals = (uint64_t *)alloca(valsSize);
            size_t numRead = 0;
            CUPTI_ASSERT(cuptiEventGroupReadAllEvents(eventGroup,
                                                      CUPTI_EVENT_READ_FLAG_NONE,
                                                      &valsSize,
                                                      vals,
                                                      &idsSize,
                                                      eventIds + eventIx,
                                                      &numRead));
            if (numRead != num) {
                fprintf(stderr, "CUPTI warning: ReadAllEvents returned unexpected number of values (expected %u, got %u)!\n", (unsigned)num, (unsigned)numRead);
            }

            // For normalisation we need the *total* number of domain
            // instances (not only the ones that were available for counting)
            CUpti_EventDomainID domainId = 0;
            size_t domainIdSize = sizeof(domainId);
            CUPTI_ASSERT(cuptiEventGroupGetAttribute(eventGroup,
                                                     CUPTI_EVENT_GROUP_ATTR_EVENT_DOMAIN_ID,
                                                     &domainIdSize, &domainId));
            uint32_t totalDomInstNum = 0;
            size_t totalDomInstNumSize = sizeof(totalDomInstNum);
            CUPTI_ASSERT(cuptiDeviceGetEventDomainAttribute(device, domainId,
                                                            CUPTI_EVENT_DOMAIN_ATTR_TOTAL_INSTANCE_COUNT,
                                                            &totalDomInstNumSize,
                                                            &totalDomInstNum));

            // Determine true counter values
            int j;
            for (j = 0; j < numRead; j++) {

                // First, sum up across instances
                uint64_t val = 0; int k;
                for (k = 0; k < domInstNum; k++) {
                    val += vals[j+k*num];
                }

                // Then normalise and add to proper event count
                eventValues[eventIx + j] = (val * totalDomInstNum) / domInstNum;
            }

            // Progress!
            eventIx += num;
        }

        // Now calculate metrics.
        for (i = 0; i < metricCount; i++) {

            // This only works if the metric does not depend on kernel
            // time (because we set it to zero here - use
            // cupti_activity facilities to measure kernel time
            // separately).
            CUpti_MetricValue metric;
            CUPTI_ASSERT(cuptiMetricGetValue(device, counter_data->metricIds[i],
                                             eventIdsSize, eventIds,
                                             eventValuesSize, eventValues,
                                             0, &metric));

            // Sum up metrics. Note that this might not actually make
            // sense for all of them, we warn about that before.
            switch (counter_data->metricKinds[i]) {
            case CUPTI_METRIC_VALUE_KIND_DOUBLE:
                metrics[i] += metric.metricValueDouble;
                break;
            case CUPTI_METRIC_VALUE_KIND_UINT64:
                metrics[i] += metric.metricValueUint64;
                break;
            case CUPTI_METRIC_VALUE_KIND_INT64:
                metrics[i] += metric.metricValueInt64;
                break;
            case CUPTI_METRIC_VALUE_KIND_PERCENT:
                metrics[i] += metric.metricValuePercent;
                break;
            case CUPTI_METRIC_VALUE_KIND_THROUGHPUT:
                metrics[i] += metric.metricValueThroughput;
                break;
            case CUPTI_METRIC_VALUE_KIND_UTILIZATION_LEVEL:
                metrics[i] += metric.metricValueUtilizationLevel;
                break;
            }
        }
    }
}

static void cupti_callback(cupti_user_t *user, CUpti_CallbackDomain domain,
                           CUpti_CallbackId cbid, void *cbdata)
{
    switch (domain) {
    case CUPTI_CB_DOMAIN_RESOURCE:
        switch (cbid) {
        case CUPTI_CBID_RESOURCE_CONTEXT_CREATED:
            cupti_callback_context_created(user, (CUpti_ResourceData *)cbdata);
            break;
        case CUPTI_CBID_RESOURCE_CONTEXT_DESTROY_STARTING:
            cupti_callback_context_destroy_starting(user, (CUpti_ResourceData *)cbdata);
            break;
        default:
            fprintf(stderr, "CUPTI metrics: Unexpected resource callback received!\n");
            break;
        }
        break;
    case CUPTI_CB_DOMAIN_DRIVER_API:
        switch (cbid) {
        case CUPTI_DRIVER_TRACE_CBID_cuLaunchKernel:
            cupti_callback_launch_kernel(user, (CUpti_CallbackData *)cbdata);
            break;
        default:
            fprintf(stderr, "CUPTI metrics: Unexpected driver API callback received!\n");
            break;
        }
        break;
    default:
        fprintf(stderr, "CUPTI metrics: Unexpected callback domain!\n");
        break;
    }
}
