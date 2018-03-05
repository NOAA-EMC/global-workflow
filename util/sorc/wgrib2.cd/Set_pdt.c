#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/*
 * the set options
 *
 * routines make a generic PDT
 *
 * 9/2008 Public Domain by Wesley Ebisuzaki
 * 10/2013 use update_sec4()
 * 7/2014 added copy more metadata when making new pdt
 *        if copying metadata, check for number of time ranges, make larger if needed
 */

/*
 * HEADER:100:set_pdt:misc:1:makes new (clean) pdt, X=PDT_number or X=PDT_number:size of PDT in octets
 */


int f_set_pdt(ARG1) {

    int pdt, i, len, copy_metadata, extra_time_range, k;
    unsigned char new_sec4[SET_PDT_SIZE];
    unsigned char *new_sec[9];
    unsigned char *p_old, *p_new;

    if (mode < 0) return 0;

    i = sscanf(arg1,"%d:%d", &pdt, &len);
    if (i == 0) fatal_error("set_pdt: X=PDT_number[:PDT_SIZE]","");

    copy_metadata = arg1[0] == '+';

    /* number of bytes for extra time ranges */
    extra_time_range = 0;
    if (copy_metadata) {
       k = stat_proc_n_time_ranges_index(sec);
       if (k >= 0) {
           extra_time_range = sec[4][k];
	   if (extra_time_range == 255) fatal_error("set_pdt: missing n_time_ranges","");
           extra_time_range = extra_time_range >= 2 ? 12*(extra_time_range - 1) : 0;
       }
    }
       
    if (i == 1) {	// use default PDT size

        switch(pdt) {

        case 0: len = 34; break;
        case 1: len = 37; break;
        case 2: len = 36; break;
        case 3: len = 68; break;
        case 4: len = 64; break;
        case 5: len = 47; break;
        case 6: len = 35; break;
        case 7: len = 34; break;
        case 8: len = 58; break;
        case 9: len = 71; break;
        case 10: len = 59; break;
        case 11: len = 61; break;
        case 12: len = 60; break;
        case 13: len = 92; break;
        case 14: len = 88; break;
        case 15: len = 37; break;
        case 20: len = 43; break;
        case 30: len = 14; break;
        case 31: len = 14; break;
        case 40: len = 36; break;
        case 41: len = 39; break;
        case 42: len = 60; break;
        case 43: len = 63; break;
        case 44: len = 45; break;
        case 45: len = 50; break;
        case 46: len = 71; break;
        case 47: len = 74; break;
        case 48: len = 58; break;
        case 52: len = 31; break;		// validation
        case 60: len = 44; break;
        case 61: len = 68; break;
        case 91: len = 35; break;
        case 254: len = 15; break;
	case 1000: len = 22; break;
	case 1001: len = 38; break;
	case 1100: len = 34; break;
	case 1101: len = 50; break;
        default: fatal_error_i("set_pdt: unsupported pdt=%d",pdt); break;
        }
	len += extra_time_range;
    }

    if (len > SET_PDT_SIZE) fatal_error_ii("set_pdt: maximum pdt len is %d wanted %d", SET_PDT_SIZE, len);

    uint_char(len, new_sec4);		    // size of sec[4];
    new_sec4[4] = 4;                        // section number
    uint2_char(0, new_sec4+5);              // no extra coordinates
    uint2_char(pdt, new_sec4+7);            // pdt
    for (i = 9; i < len; i++) new_sec4[i] = 255;

    if (copy_metadata) {
	for (i = 0; i < 9; i++) new_sec[i] = sec[i];
        new_sec[4] = new_sec4;
	new_sec4[9] = sec[4][9];		// parmmeter category 4.1
	new_sec4[10] = sec[4][10];		// parameter number   4.2

	p_old = code_table_4_3_location(sec);
	p_new = code_table_4_3_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = code_table_4_4_location(sec);
	p_new = code_table_4_4_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    for (i = 0; i < 5; i++) p_new[i] = p_old[i];
	}

	p_old = code_table_4_5a_location(sec);
	p_new = code_table_4_5a_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    for (i = 0; i < 6; i++) p_new[i] = p_old[i];
	}

	p_old = code_table_4_5b_location(sec);
	p_new = code_table_4_5b_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    for (i = 0; i < 6; i++) p_new[i] = p_old[i];
	}

	p_old = code_table_4_6_location(sec);
	p_new = code_table_4_6_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = perturbation_number_location(sec);
	p_new = perturbation_number_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = number_of_forecasts_in_the_ensemble_location(sec);
	p_new = number_of_forecasts_in_the_ensemble_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = code_table_4_7_location(sec);
	p_new = code_table_4_7_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = code_table_4_9_location(sec);
	p_new = code_table_4_9_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    for (i = 0; i < 11; i++) p_new[i] = p_old[i];
	}

	p_old = background_generating_process_identifier_location(sec);
	p_new = background_generating_process_identifier_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = analysis_or_forecast_generating_process_identifier_location(sec);
	p_new = analysis_or_forecast_generating_process_identifier_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	p_old = hours_of_observational_data_cutoff_after_reference_time_location(sec);
	p_new = hours_of_observational_data_cutoff_after_reference_time_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
	    *p_new++ = *p_old++;
	    *p_new = *p_old;
	}

	p_old = observation_generating_process_identifier_location(sec);
	p_new = observation_generating_process_identifier_location(new_sec);
	if (p_old != NULL && p_new != NULL) *p_new = *p_old;

	/* time ranges for stat processing */
	p_old = stat_proc_verf_time_location(sec);
	p_new = stat_proc_verf_time_location(new_sec);
	if (p_old != NULL && p_new != NULL) {
            k = (58-34) + extra_time_range;
	    for (i = 0; i < k; i++) p_new[i] = p_old[i];
	}

    }
    update_sec4(sec, new_sec4);
    return 0;
}

