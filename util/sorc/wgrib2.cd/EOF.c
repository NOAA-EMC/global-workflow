#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

static FILE *err_file = NULL;
static int err_int;

static FILE *err_str_file = NULL;
static char *err_str;

extern int file_append;

/*
 * HEADER:100:err_bin:setup:2:send (binary) integer to file upon err exit: X=file Y=integer
 */

int f_err_bin(ARG2) {
    if (mode == -1) {
        if ((err_file = (void *) ffopen(arg1, file_append ? "ab" : "wb")) == NULL) {
            fatal_error("Could not open %s", arg1);
        }
	err_int = atoi(arg2);
    }
    return 0;
}
void err_bin(int error) {
    int i;
    /* this routine may called by fatal error and end of processing */
    if (error && err_file != NULL) {
        i = fwrite(&err_int, sizeof(int), 1, err_file);
        if (i != 1) fprintf(stderr,"ERROR err_bin: write error\n");
    }
    if (err_file != NULL) ffclose(err_file);
    
    return;
}

/*
 * HEADER:100:err_string:setup:2:send string to file upon err exit: X=file Y=string
 */

int f_err_string(ARG2) {
    if (mode == -1) {
        if ((err_str_file = (void *) ffopen(arg1, file_append ? "ab" : "wb")) == NULL) {
            fatal_error("Could not open %s", arg1);
        }
	err_str = malloc(strlen(arg2) + 1);
	if (err_str == NULL) fatal_error("err_string: memory allocation","");
	strncpy(err_str, arg2, strlen(arg2)+1);
    }
    return 0;
}

void err_string(int error) {
    int i;
    /* this routine may called by fatal error and end of processing */
    if (error && err_str_file != NULL) {
	i = fwrite(err_str, strlen(err_str), 1, err_str_file);
	if (i != 1) fprintf(stderr,"ERROR err_string: write error\n");
    }
    if (err_file != NULL) ffclose(err_file);
    return;
}



/*
 * HEADER:100:eof_bin:setup:2:send (binary) integer to file upon EOF: X=file Y=integer
 */

int f_eof_bin(ARG2) {
    int i,j;
    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1, file_append ? "ab" : "wb")) == NULL) {
            fatal_error("Could not open %s", arg1);
        }
    }
    else if (mode == -2) {
	j = atoi(arg2);
	i = fwrite(&j, sizeof(int), 1, (FILE *) *local);
	if (i != 1) fatal_error("eof_bin: write file %s", arg1);
	ffclose((FILE *) *local);
    }
    return 0;
}

/*
 * HEADER:100:eof_string:setup:2:send string to file upon EOF: X=file Y=string
 */

int f_eof_string(ARG2) {
    int i;
    if (mode == -1) {
        if ((*local = (void *) ffopen(arg1, file_append ? "ab" : "wb")) == NULL) {
            fatal_error("Could not open %s", arg1);
        }
    }
    else if (mode == -2) {
        i = fwrite(arg2, strlen(arg2), 1, (FILE *) *local);
        if (i != 1) fatal_error("eof_string: write error %s", arg1);
	ffclose((FILE *) *local);
    }
    return 0;
}
