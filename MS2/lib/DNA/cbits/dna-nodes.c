#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef SLURM
#include <slurm/slurm.h>

char *dna_hosts()
{
        hostlist_t my_hostlist;
        char *hostnames, *host, *result;
        int length = 0, off = 0;
        hostnames = getenv("SLURM_NODELIST");
        if (hostnames == NULL) {
                fprintf(stderr, "No slurm host list\n");
                exit(1);
        }
        my_hostlist = slurm_hostlist_create(hostnames);
        if (my_hostlist == NULL) {
                fprintf(stderr, "No memory\n");
                exit(1);
        }
        while ( host = slurm_hostlist_shift(my_hostlist)) {
                length += strlen(host) + 1;
                free(host);
        }
        slurm_hostlist_destroy(my_hostlist);
        result = malloc(length);
        if (!result) {
                fprintf(stderr, "No memory\n");
                exit(1);
        }
        bzero(result, length);
        my_hostlist = slurm_hostlist_create(hostnames);
        if (my_hostlist == NULL) {
                fprintf(stderr, "No memory\n");
                exit(1);
        }
        while ((host = slurm_hostlist_shift(my_hostlist))) {
                sprintf(result + off, "%s ", host);
                off += strlen(host) + 1;
                free(host);
        }
        result[length] = '\0';
        slurm_hostlist_destroy(my_hostlist);
        return result;
}

#else

char *dna_hosts()
{
        char *r = getenv("SLURM_NODELIST");
        if (!r) {
                fprintf(stderr, "ERROR cannot get DNA hosts\n");
                exit(-1);
        }
        return r;
}

#endif
#if 0
int main(int argc, char **argv)
{
        printf("DNA hosts: %s\n", dna_hosts());
        return 0;
}
#endif
