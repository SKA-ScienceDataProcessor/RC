
#ifndef __STATIC_SCHEDULING_SUPPORT_H
#define	__STATIC_SCHEDULING_SUPPORT_H

#ifdef __cplusplus
extern "C" {
#endif

/* put message into logging system. Please note that you should avoid making multiline log entries and end line with \n */
void node_log(char*fmt, ...);

/* return (runtime instance initialization) relative time in microseconds (good enough for our benchmark) */
long current_time_microseconds();

/* register mapper - also sets global variables with various parameters */
void register_mappers();

#ifdef __cplusplus
}
#endif

#endif  /*  __STATIC_SCHEDULING_SUPPORT_H */
