
#include <unistd.h>
#include <asm/unistd.h>
#include <sys/types.h>
#include <sys/syscall.h>
#include <linux/perf_event.h>
#include <stdint.h>
#include <memory.h>

#ifdef USE_LIBPFM
#define _LINUX_PERF_EVENT_H
#define perf_event_open __perf_event_open
#include <perfmon/pfmlib_perf_event.h>
#undef perf_event_open
#endif

static inline int
sys_perf_event_open(struct perf_event_attr *attr,
                    pid_t pid, int cpu, int group_fd,
                    unsigned long flags)
{
    return syscall(__NR_perf_event_open, attr, pid, cpu, group_fd, flags);
}

static int perf_event_open_gen(struct perf_event_attr *attr,
                               int group_fd)
{

    // Group leader: Request time values for reference (so we can
    // detect multiplexing). Also ask for the whole group to return
    // results at the same time.
    if (group_fd == -1) {
        attr->read_format = PERF_FORMAT_TOTAL_TIME_ENABLED
                          | PERF_FORMAT_TOTAL_TIME_RUNNING
                          | PERF_FORMAT_GROUP;
    }

    // Exclude kernel and hypervisor
    attr->exclude_kernel = 1;
    attr->exclude_hv = 1;

    // Create the performance counter
    return sys_perf_event_open(attr, 0, -1, group_fd, 0);
}

int perf_event_open(uint32_t type,
                    uint64_t config,
                    int group_fd)
{
    struct perf_event_attr attr;

    // Basic counter set-up
    memset(&attr, 0, sizeof(attr));
    attr.size = sizeof(attr);
    attr.type = type;
    attr.config = config;

    return perf_event_open_gen(&attr, group_fd);
}

#ifdef USE_LIBPFM
static int pfm_initialized;
int perf_event_open_pfm(const char *str,
                        int group_fd)
{
    struct perf_event_attr attr;
    pfm_perf_encode_arg_t arg;

    // Clear
    memset(&attr, 0, sizeof(attr));
    attr.size = sizeof(attr);
    memset(&arg, 0, sizeof(arg));
    arg.size = sizeof(arg);
    arg.attr = &attr;

    // Use pfm to populate "attr"
    if (!pfm_initialized) {
        if (pfm_initialize() != PFM_SUCCESS)
            return -2;
        pfm_initialized = 1;
    }
    if (pfm_get_os_event_encoding(str, PFM_PLM0 | PFM_PLM3, PFM_OS_PERF_EVENT, &arg)
          != PFM_SUCCESS)
        return -2;

    // Generate event
    return perf_event_open_gen(&attr, group_fd);
}
#endif