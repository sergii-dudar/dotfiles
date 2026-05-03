#include "output.h"

#include <stdio.h>

output_connection *output_connection_open(void) {
    return (output_connection *)stdout;
}

void output_connection_close(output_connection *const connection) {
    (void)connection;
}

int output_set_status(output_connection *const connection, const char *name) {
    (void)connection;
    if (fprintf(stdout, "%s\n", name) < 0) {
        (void)fprintf(stderr, "error: could not write status to stdout\n");
        return 1;
    }
    if (fflush(stdout) != 0) {
        (void)fprintf(stderr, "error: could not flush stdout\n");
        return 1;
    }
    return 0;
}
