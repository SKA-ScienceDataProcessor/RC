
#ifndef __STATIC_SCHEDULING_SUPPORT_H
#define	__STATIC_SCHEDULING_SUPPORT_H

#ifdef __cplusplus
extern "C" {
#endif

/* put message into logging system. Please note that you should avoid making multiline log entries and end line with \n */
void node_log(char*fmt, ...);

/* register mapper - also sets global variables with various parameters */
void register_mappers();

/* return current SLURM task index */
int current_slurm_task();

/* return index of current SLURM node */
int current_slurm_node();

void register_mappers();

#ifdef __cplusplus
}
#endif

#endif  /*  __STATIC_SCHEDULING_SUPPORT_H */
