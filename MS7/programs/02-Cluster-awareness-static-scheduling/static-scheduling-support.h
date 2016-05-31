
#ifndef __STATIC_SCHEDULING_SUPPORT_H
#define	__STATIC_SCHEDULING_SUPPORT_H

/* put message into logging system. Please note that you should avoid making multiline log entries and end line with \n */
void node_log(char*fmt, ...);

/* get number of slurm nodes */
int num_slurm_nodes();

/* current SLURM node */
int current_slurm_node();

/* current SLURM task */
int current_slurm_task();

/* get branching factor from command line */
int branching_factor();

/* register mapper - also sets global variables with various parameters */
void register_mappers();

#endif  /*  __STATIC_SCHEDULING_SUPPORT_H */
