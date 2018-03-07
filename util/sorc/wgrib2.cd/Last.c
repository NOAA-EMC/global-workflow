#include <stdio.h>
#include <stdlib.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

extern int file_append;
extern char *last_inv_out;

/*
 * HEADER:100:last:inv_output:1:write last inv item to file X
 */
int f_last(ARG1) {
    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1, file_append ? "a" : "w")) == NULL) {
            fatal_error("last: Could not open %s", arg1);
        }
    }
    else if (mode == -2) {
        ffclose((FILE *) *local);
    }
    else if (mode >= 0) {
        fprintf((FILE *) *local, "%s\n", last_inv_out);
    }
    return 0;
}
