#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "wgrib2.h"

/*
 * a simple extension to fopen
 *
 * if file is already open, just return the pointer to the file handler
 *
 * public domain 2/2008 Wesley Ebisuzaki
 *  v1.1 WNE: upgrade: add read/write detection
 *		- can be stdin and stdout (in previous version)
 *		if pipe - set flush_mode, error checking
 *  v1.2 10/2014 John Howard .. add ffclose
 *               needs to keep track of number of ffopens of a file
 *               when number ffopens and ffclose are the same .. close file
 *
 *               adding ffcloses is needed to make wgrib2 a callable subroutine
 *                otherwise run out of file handles
 */

extern int flush_mode;
struct opened_file
{
    char *name;
    FILE *handle;
    int is_read_file;
    int usage_count;
    int do_not_close_flag;
    struct opened_file *next;
};

/* do not initialize opened_file_start in init_globals */
static struct opened_file *opened_file_start = NULL;
/* do not initialize opened_file_start in init_globals */

FILE *ffopen(const char *filename, const char *mode)
{
    struct opened_file *ptr;
    struct stat stat_buf;  /* test for pipes */
    int is_read_file;
    const char *p;

    /* see if is a read/write file */
    is_read_file = 0;
    p = mode;
    while (*p) {
	if (*p++ == 'r') is_read_file = 1;
    }

    if (strcmp(filename,"-") == 0) {
	if (is_read_file) return stdin;
	flush_mode = 1;
	return stdout;
    }

    ptr = opened_file_start;
    while (ptr != NULL) {
	if (strcmp(filename,ptr->name) == 0) {
	    if (is_read_file != ptr->is_read_file) 
			fatal_error("ffopen: file can only read or write not both: %s", ptr->name);
	    (ptr->usage_count)++;
	    return ptr->handle;
	}
	ptr = ptr->next;
    }

    ptr = (struct opened_file *) malloc( sizeof(struct opened_file) );
    ptr->handle = fopen(filename,mode);
    if (ptr->handle == NULL) fatal_error("ffopen: could not open %s", filename);
    ptr->name = (char *) malloc(strlen(filename) + 1);
    strncpy(ptr->name, filename, strlen(filename)+1);
    ptr->is_read_file = is_read_file;
    ptr->usage_count = 1;
    ptr->do_not_close_flag = 0;
    ptr->next = opened_file_start;
    opened_file_start = ptr;

    /* check if output is to a pipe */
    if (is_read_file == 0) {
	if (stat(filename, &stat_buf) == -1)
		fatal_error("ffopen: could not stat file: %s", filename);
        if (S_ISFIFO(stat_buf.st_mode)) {
	    flush_mode  = 1;
	}
    }

    return ptr->handle;
}

int ffclose(FILE *flocal)
{
    struct opened_file *ptr, *last_ptr;

    if (flocal == stdin || flocal == stdout) return 0;		// do not close stdin or stdout

    last_ptr = NULL;
    ptr = opened_file_start;
    while (ptr != NULL) {
	if (ptr->handle == flocal) {
	    if (ptr->usage_count <= 0) fatal_error_i("ffclose: usage_count = %d <= 0, %d", ptr->usage_count);
	    if (ptr->do_not_close_flag == 1) return 0;
	    if (--ptr->usage_count == 0) {   // usage_count is zero, can close file name
		fclose(ptr->handle);
		free(ptr->name);
		if (last_ptr == NULL)
		    opened_file_start = ptr->next;
		else {
		    last_ptr->next = ptr->next;
		}
		free(ptr);
	    }
	    return 0;
	}
	last_ptr = ptr;
	ptr = ptr->next;
    }
    return 1;	/* not found */
}

/*
 * make a file that has been opened with ffopen persistent
 *
 * with a callable wgrib2, may not want to close files for performance
 *
 */
int mk_file_persistent(const char *filename) {
    struct opened_file *ptr;

    ptr = opened_file_start;
    while (ptr != NULL) {
	if (strcmp(filename,ptr->name) == 0) {
	    ptr->do_not_close_flag = 1;
	    return 0;
	}
	ptr = ptr->next;
    }
    fprintf(stderr,"Warning persistant flag not set %s\n", filename);
    return 1;
}

/*
 * make a persistent file transient (remove persistant flag)
 */

int mk_file_transient(const char *filename) {
    struct opened_file *ptr;

    ptr = opened_file_start;
    while (ptr != NULL) {
	if (strcmp(filename,ptr->name) == 0) {
	    ptr->do_not_close_flag = 0;
	    return 0;
	}
	ptr = ptr->next;
    }
    fprintf(stderr,"Warning persistant not cleared %s\n", filename);
    return 1;
}

/*
 * rewind filename
 */

int rewind_file(const char *filename) {
    struct opened_file *ptr;
    ptr = opened_file_start;
    while (ptr != NULL) {
	if (strcmp(filename,ptr->name) == 0) {
	    rewind(ptr->handle);	    
	    return 0;
	}
	ptr = ptr->next;
    }
    return 1;
}

/*
 * this routine checks to see that all files opened using ffopen are closed 
 */

int ffclose_finished(void) {
    struct opened_file *ptr;
    ptr = opened_file_start;
    while (ptr != NULL) {
        fprintf(stderr, "ffclose_finished .. found file %s open\n", ptr->name);
	ptr = ptr->next;
    }
    return 0;
}
