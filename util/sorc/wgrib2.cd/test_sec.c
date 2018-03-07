#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"


/* some routines to check whether two fields are the same */


int same_sec0(unsigned char **sec_a, unsigned char **sec_b) {

    unsigned char *a, *b;
    int i;
    a = sec_a[0];
    b = sec_b[0];
    for (i = 0; i < 8; i++) {
	if (*a++ != *b++) return 0;
    }
    return 1;
}

int same_sec1(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i;
    i = GB2_Sec1_size(sec_a);
    if (GB2_Sec1_size(sec_b) != i) return 0;
    a = sec_a[1];
    b = sec_b[1];
    while (i--) {
	if (*a++ != *b++) return 0;
    }
    return 1;
}

// test to see if same sec1 but don't do time stamp

int same_sec1_not_time(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i, j;
    i = GB2_Sec1_size(sec_a);
    if (GB2_Sec1_size(sec_b) != i) return 0;
    a = sec_a[1];
    b = sec_b[1];
    for (j = 0; j < 12; j++) {
	if (a[j] != b[j]) return 0;
    }
    for (j = 19; j < i; j++) {
	if (a[j] != b[j]) return 0;
    }
    return 1;
}

int same_sec2(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i;
    i = GB2_Sec2_size(sec_a);
    if (GB2_Sec2_size(sec_b) != i) return 0;
    a = sec_a[2];
    b = sec_b[2];
    while (i--) {
        if (*a++ != *b++) return 0;
    }
    return 1;
}


int same_sec3(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i;
    i = GB2_Sec3_size(sec_a);
    if (GB2_Sec3_size(sec_b) != i) return 0;
    a = sec_a[3];
    b = sec_b[3];
    while (i--) {
        if (*a++ != *b++) return 0;
    }
    return 1;
}

int same_sec4(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int i;

    i = GB2_Sec4_size(sec_a);
    if (GB2_Sec4_size(sec_b) != i) return 0;
    a = sec_a[4];
    b = sec_b[4];
    while (i--) {
        if (*a++ != *b++) return 0;
    }
    return 1;
}

/*
   check to see if the two section 4 are the same
   this version ignores time code (fcst hour and the time code in the stat processing)
   returns 1 if the same.
   modified 8/2013
 */

int same_sec4_not_time(unsigned char **sec_a, unsigned char **sec_b) {
 
    unsigned char *a, *b, *p;
    unsigned int i, j;
    int pdt;
    static int warning = 0; 
    int code_4_4, stat_time;

    i = GB2_Sec4_size(sec_a);
    if (GB2_Sec4_size(sec_b) != i) return 0;
    pdt = GB2_ProdDefTemplateNo(sec_a);
    if (GB2_ProdDefTemplateNo(sec_b) != pdt) return 0;

    a = sec_a[4];
    b = sec_b[4];

    if (pdt >= 0 && pdt <= 15) {

        p = code_table_4_4_location(sec_a);
	if (p == NULL) fatal_error_i("same_sec4_not_time, prog error pdt=%d", pdt);
	code_4_4 = p - a;
	p = stat_proc_verf_time_location(sec_a);
	stat_time = p ? p - a : 0;

// printf(">>> code_4_4_%d stat_time %d pdt=%d\n",code_4_4, stat_time, pdt);

	for (j = 0; j < code_4_4; j++) {
	    if (a[j] != b[j]) return 0;
	}

	if (stat_time == 0) {
	    for (j = code_4_4 + 5; j < i; j++) {
	        if (a[j] != b[j]) return 0;
	    }
	}
	else {
	    for (j = code_4_4 + 5; j < stat_time; j++) {
	        if (a[j] != b[j]) return 0;
	    }
	    for (j = stat_time + 7; j < i; j++) {
	        if (a[j] != b[j]) return 0;
	    }
	}
	return 1;
    }

    if (warning == 0) {
	warning = 1;
	fprintf(stderr,"same_sec4_not_time does not handle pdt=%d",pdt);
    }
    return 0;
}

/*
 *  check to see if sec4 is the same except allowing for different ave/acc period
 *   if different, return 0
 *   if not statistically processed PDT, return 0
 *   if same, return 1
 */

int same_sec4_diff_ave_period(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int size, j;
    int idx;

    if (GB2_Sec4_size(sec_b) != (size = GB2_Sec4_size(sec_a)) ) return 0;
    if (GB2_ProdDefTemplateNo(sec_a) != GB2_ProdDefTemplateNo(sec_b) ) return 0;
    if ((idx = stat_proc_n_time_ranges_index(sec_a)) < 0) return 0;
    a = sec_a[4];
    b = sec_b[4];

    // check for data up to year of end of overall time interval
    for (j = 0; j < idx - 7 ; j++) {
	if (a[j] != b[j]) return 0;
    }

    //  skip time of end of overall time interval * 7 bytes

    // check n time ranges to secoded code table 4.4
    for (j = idx; j < idx+8;  j++) {
	if (a[j] != b[j]) return 0;
    }

    //  skip length of time range

    for (j = idx+12; j < size; j++) {
	if (a[j] != b[j]) return 0;
    }
    return 1;
}

// test for same sec4 merge

int same_sec4_for_merge(unsigned char **sec_a, unsigned char **sec_b) {
    unsigned char *a, *b;
    unsigned int j, size, code_4_4;
    int idx;

    if (GB2_Sec4_size(sec_b) != (size = GB2_Sec4_size(sec_a)) ) return 0;
    if (GB2_ProdDefTemplateNo(sec_a) != GB2_ProdDefTemplateNo(sec_b) ) return 0;
    if ((idx = stat_proc_n_time_ranges_index(sec_a)) < 0) return 0;

    a = sec_a[4];
    b = sec_b[4];

    code_4_4 = code_table_4_4_location(sec_a) - a;

    // up to and including code table 4.4
    for (j = 0; j <= code_4_4; j++) {
	if (a[j] != b[j]) return 0;
    }

    // ignore forecast hour
    for (j = code_4_4+5; j < idx + 35 - 42; j++) {
	if (a[j] != b[j]) return 0;
    }

    // ignore time of end of overall time interval

    for (j = idx; j < size;  j++) {
	if (a[j] != b[j]) return 0;
    }

    return 1;
}
