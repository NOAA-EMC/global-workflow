#include <stdio.h>
#include <string.h>
#include "grb2.h"
#include "wgrib2.h"

/*
 * time range: ascii to int, int to ascii
 */

static struct tr_table_struct {
	const char *name; const int val;
} tr_table [] = {
	{ "min", 0 },
	{ "hour", 1 }, 
	{ "day", 2 },
	{ "month", 3 },
	{ "year", 4 },
	{ "decade", 5 },
	{ "normal", 6 },
	{ "century", 7 },
	{ "3-hours", 10 },
	{ "6-hours", 11 },
	{ "12-hours", 12 },
	{ "sec", 13 },
	{ "missing", 255 },
};

int a2time_range(const char * string) {
    int i;
    for (i = 0; i < sizeof (tr_table) / sizeof(struct tr_table_struct); i++) {
	if (strcmp(string,tr_table[i].name) == 0) return tr_table[i].val;
    }
    return -1;
}

const char *time_range2a(int tr) {
    int i;
    for (i = 0; i < sizeof (tr_table) / sizeof(struct tr_table_struct); i++) {
	if (tr_table[i].val == tr) return tr_table[i].name;
    }
    return "?";
}

/*
 * converts units to single digit units
 * i.e. 3 hour -> hours
 *
 */

int normalize_time_range(int *tr, int *val) {
    switch(*tr) {
	case 10:		// 3 hours
	    *tr = 1;
	    *val *= 3;
	    break;
	case 11:		// 6 hours
	    *tr = 1;
	    *val *= 6;
	    break;
	case 12:		// 12 hours
	    *tr = 1;
	    *val *= 12;
	    break;
    }
    return 0;
}
