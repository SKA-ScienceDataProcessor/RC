#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <slurm/slurm.h>

int main(int argc, char *argv[])
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
	printf("Result:\n%s\n", result);
	slurm_hostlist_destroy(my_hostlist);
	exit(0);
}
