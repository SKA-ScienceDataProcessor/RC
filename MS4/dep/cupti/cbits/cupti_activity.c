
#include <cupti_activity.h>

#include <stdio.h>

// Statistics about the activity we've seen. This will most likely
// require extension at some point.
volatile uint64_t memsetTime = 0;
volatile uint64_t memcpyTime[3][3] = {{0,0,0},{0,0,0},{0,0,0}};
volatile uint64_t kernelTime = 0;
volatile uint64_t overheadTime = 0;
volatile uint64_t dropped = 0;

volatile uint64_t memsetBytes = 0;
volatile uint64_t memcpyBytes[3][3] = {{0,0,0},{0,0,0},{0,0,0}};

const int HOST = 0;
const int DEVICE = 1;
const int ARRAY = 2;

uint64_t cupti_getMemsetTime() { return memsetTime; }
uint64_t cupti_getMemcpyTime(int from, int to) { return memcpyTime[from][to]; }
uint64_t cupti_getKernelTime() { return kernelTime; }
uint64_t cupti_getOverheadTime() { return overheadTime; }
uint64_t cupti_getDroppedRecords() { return dropped; }
uint64_t cupti_getMemsetBytes() { return memsetBytes; }
uint64_t cupti_getMemcpyBytes(int from, int to) { return memcpyBytes[from][to]; }

// Stolen from activity_trace_async example - they ought to know
#define BUF_SIZE (32 * 1024)
#define ALIGN_SIZE (8)
#define ALIGN_BUFFER(buffer, align)                                            \
  (((uintptr_t) (buffer) & ((align)-1)) ? ((buffer) + (align) - ((uintptr_t) (buffer) & ((align)-1))) : (buffer))

static void CUPTIAPI bufferRequested(uint8_t **buffer, size_t *size, size_t *maxNumRecords)
{
    uint8_t *data = (uint8_t *) malloc(BUF_SIZE + ALIGN_SIZE);
    if (data == NULL) exit(-1);

    *size = BUF_SIZE;
    *buffer = ALIGN_BUFFER(data, ALIGN_SIZE);
    *maxNumRecords = 0;
}

static void CUPTIAPI bufferCompleted(CUcontext ctx, uint32_t streamId, uint8_t *buffer, size_t size, size_t validSize)
{
    CUptiResult res;
    CUpti_Activity *record = NULL;
    size_t drops;

    // Check dropped records
    if (CUPTI_SUCCESS == cuptiActivityGetNumDroppedRecords(ctx, streamId, &drops)) {
        dropped += drops;
    }

    // Read records
    if (validSize <= 0) return;
    while(1) {
        // Get a record
        if (CUPTI_SUCCESS != cuptiActivityGetNextRecord(buffer, validSize, &record))
            break;

        // Identify
        switch (record->kind) {
        case CUPTI_ACTIVITY_KIND_MEMSET:
            {
                CUpti_ActivityMemset *activity = (CUpti_ActivityMemset *)record;
                memsetTime += activity->end - activity->start;
                memsetBytes += activity->bytes;
            }
            break;
        case CUPTI_ACTIVITY_KIND_MEMCPY:
            {
                CUpti_ActivityMemcpy *activity = (CUpti_ActivityMemcpy *)record;
                int from = HOST, to = HOST;
                switch (activity->copyKind) {
                case CUPTI_ACTIVITY_MEMCPY_KIND_HTOD: from = HOST; to = DEVICE; break;
                case CUPTI_ACTIVITY_MEMCPY_KIND_DTOH: from = DEVICE; to = HOST; break;
                case CUPTI_ACTIVITY_MEMCPY_KIND_HTOA: from = HOST; to = ARRAY; break;
                case CUPTI_ACTIVITY_MEMCPY_KIND_ATOH: from = ARRAY; to = HOST; break;
                case CUPTI_ACTIVITY_MEMCPY_KIND_ATOA: from = ARRAY; to = ARRAY; break;
                case CUPTI_ACTIVITY_MEMCPY_KIND_ATOD: from = ARRAY; to = DEVICE; break;
                case CUPTI_ACTIVITY_MEMCPY_KIND_DTOA: from = DEVICE; to = ARRAY; break;
                case CUPTI_ACTIVITY_MEMCPY_KIND_DTOD: from = DEVICE; to = DEVICE; break;
                case CUPTI_ACTIVITY_MEMCPY_KIND_HTOH: from = HOST; to = HOST; break;
                    // Need a separate counter?
                case CUPTI_ACTIVITY_MEMCPY_KIND_PTOP: from = DEVICE; to = DEVICE;
                    static int warned = 0;
                    if (!warned) {
                        fputs("CUPTI warning: We currently count P2P memcpy as"
                              "device-to-device. This *might* not be what you want.",
                              stderr);
                        warned = 1;
                    }
                    break;
                }
                memcpyTime[from][to] += activity->end - activity->start;
                memcpyBytes[from][to] += activity->bytes;
            }
            break;
        case CUPTI_ACTIVITY_KIND_KERNEL:
            {
                CUpti_ActivityKernel *activity = (CUpti_ActivityKernel *)record;
                kernelTime += activity->end - activity->start;
            }
            break;
        case CUPTI_ACTIVITY_KIND_OVERHEAD:
            {
                CUpti_ActivityOverhead *activity = (CUpti_ActivityOverhead *)record;
                overheadTime += activity->end - activity->start;
            }
            break;
        }
    }

    free(buffer);
}

#define CUPTI_CALL(call)                            \
    if (CUPTI_SUCCESS != (res = call)) return res;

int cupti_enable()
{
    CUptiResult res;

    // Announce our interest in certain activity
    CUPTI_CALL(cuptiActivityEnable(CUPTI_ACTIVITY_KIND_MEMSET));
    CUPTI_CALL(cuptiActivityEnable(CUPTI_ACTIVITY_KIND_MEMCPY));
    CUPTI_CALL(cuptiActivityEnable(CUPTI_ACTIVITY_KIND_KERNEL));
    CUPTI_CALL(cuptiActivityEnable(CUPTI_ACTIVITY_KIND_OVERHEAD));

    // Register callbacks
    CUPTI_CALL(cuptiActivityRegisterCallbacks(bufferRequested, bufferCompleted));

    // Okay
    return 0;
}

int cupti_disable()
{
    cuptiActivityDisable(CUPTI_ACTIVITY_KIND_MEMSET);
    cuptiActivityDisable(CUPTI_ACTIVITY_KIND_MEMCPY);
    cuptiActivityDisable(CUPTI_ACTIVITY_KIND_KERNEL);
    cuptiActivityDisable(CUPTI_ACTIVITY_KIND_OVERHEAD);
}

