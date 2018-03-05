#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "grb2.h"
#include "wgrib2.h"
#include "fnlist.h"

/* Levels.c
 *   2006: public domain wesley ebisuzaki
 *   1/2007: cleanup M. Schwarb
 *   1/2007: Caser Tejeda Hernandez found error in meter underground
 *   2/2007: level 11
 *   2/2007: spelling error fixed
 *   9/2008: type 241 added Nick Lott 
 */


/*
 * HEADER:200:lev:inv:0:level (code table 4.5)
 */

/* code table 4.5 */

const char *level_table[192] = {
/* 0 */ "reserved",
/* 1 */ "surface",
/* 2 */ "cloud base",
/* 3 */ "cloud top",
/* 4 */ "0C isotherm",
/* 5 */ "level of adiabatic condensation from sfc",
/* 6 */ "max wind",
/* 7 */ "tropopause",
/* 8 */ "top of atmosphere",
/* 9 */ "sea bottom",
/* 10 */ "entire atmosphere",
/* 11 */ "cumulonimbus base",
/* 12 */ "cumulonimbus top",
/* 13 */ "reserved",
/* 14 */ "reserved",
/* 15 */ "reserved",
/* 16 */ "reserved",
/* 17 */ "reserved",
/* 18 */ "reserved",
/* 19 */ "reserved",
/* 20 */ "%g K level",
/* 21 */ "reserved",
/* 22 */ "reserved",
/* 23 */ "reserved",
/* 24 */ "reserved",
/* 25 */ "reserved",
/* 26 */ "reserved",
/* 27 */ "reserved",
/* 28 */ "reserved",
/* 29 */ "reserved",
/* 30 */ "reserved",
/* 31 */ "reserved",
/* 32 */ "reserved",
/* 33 */ "reserved",
/* 34 */ "reserved",
/* 35 */ "reserved",
/* 36 */ "reserved",
/* 37 */ "reserved",
/* 38 */ "reserved",
/* 39 */ "reserved",
/* 40 */ "reserved",
/* 41 */ "reserved",
/* 42 */ "reserved",
/* 43 */ "reserved",
/* 44 */ "reserved",
/* 45 */ "reserved",
/* 46 */ "reserved",
/* 47 */ "reserved",
/* 48 */ "reserved",
/* 49 */ "reserved",
/* 50 */ "reserved",
/* 51 */ "reserved",
/* 52 */ "reserved",
/* 53 */ "reserved",
/* 54 */ "reserved",
/* 55 */ "reserved",
/* 56 */ "reserved",
/* 57 */ "reserved",
/* 58 */ "reserved",
/* 59 */ "reserved",
/* 60 */ "reserved",
/* 61 */ "reserved",
/* 62 */ "reserved",
/* 63 */ "reserved",
/* 64 */ "reserved",
/* 65 */ "reserved",
/* 66 */ "reserved",
/* 67 */ "reserved",
/* 68 */ "reserved",
/* 69 */ "reserved",
/* 70 */ "reserved",
/* 71 */ "reserved",
/* 72 */ "reserved",
/* 73 */ "reserved",
/* 74 */ "reserved",
/* 75 */ "reserved",
/* 76 */ "reserved",
/* 77 */ "reserved",
/* 78 */ "reserved",
/* 79 */ "reserved",
/* 80 */ "reserved",
/* 81 */ "reserved",
/* 82 */ "reserved",
/* 83 */ "reserved",
/* 84 */ "reserved",
/* 85 */ "reserved",
/* 86 */ "reserved",
/* 87 */ "reserved",
/* 88 */ "reserved",
/* 89 */ "reserved",
/* 90 */ "reserved",
/* 91 */ "reserved",
/* 92 */ "reserved",
/* 93 */ "reserved",
/* 94 */ "reserved",
/* 95 */ "reserved",
/* 96 */ "reserved",
/* 97 */ "reserved",
/* 98 */ "reserved",
/* 99 */ "reserved",
/* 100 */ "%g mb",
/* 101 */ "mean sea level",
/* 102 */ "%g m above mean sea level",
/* 103 */ "%g m above ground",
/* 104 */ "%g sigma level",
/* 105 */ "%g hybrid level",
/* 106 */ "%g m underground",
/* 107 */ "%g K isentropic level",
/* 108 */ "%g mb above ground",
/* 109 */ "PV=%g (Km^2/kg/s) surface",
/* 110 */ "reserved",
/* 111 */ "%g Eta level",
/* 112 */ "reserved",
/* 113 */ "logarithmic hybrid level",
/* 114 */ "snow level",
/* 115 */ "reserved",
/* 116 */ "reserved",
/* 117 */ "mixed layer depth",
/* 118 */ "hybrid height level",
/* 119 */ "hybrid pressure level",
/* 120 */ "reserved",
/* 121 */ "reserved",
/* 122 */ "reserved",
/* 123 */ "reserved",
/* 124 */ "reserved",
/* 125 */ "reserved",
/* 126 */ "reserved",
/* 127 */ "reserved",
/* 128 */ "reserved",
/* 129 */ "reserved",
/* 130 */ "reserved",
/* 131 */ "reserved",
/* 132 */ "reserved",
/* 133 */ "reserved",
/* 134 */ "reserved",
/* 135 */ "reserved",
/* 136 */ "reserved",
/* 137 */ "reserved",
/* 138 */ "reserved",
/* 139 */ "reserved",
/* 140 */ "reserved",
/* 141 */ "reserved",
/* 142 */ "reserved",
/* 143 */ "reserved",
/* 144 */ "reserved",
/* 145 */ "reserved",
/* 146 */ "reserved",
/* 147 */ "reserved",
/* 148 */ "reserved",
/* 149 */ "reserved",
/* 150 */ "%g generalized vertical height coordinate",
/* 151 */ "reserved",
/* 152 */ "reserved",
/* 153 */ "reserved",
/* 154 */ "reserved",
/* 155 */ "reserved",
/* 156 */ "reserved",
/* 157 */ "reserved",
/* 158 */ "reserved",
/* 159 */ "reserved",
/* 160 */ "%g m below sea level",
/* 161 */ "%g m below water surface",
/* 162 */ "lake or river bottom",
/* 163 */ "bottom of sediment layer",
/* 164 */ "bottom of thermally active sediment layer",
/* 165 */ "bottom of sediment layer penetrated by thermal wave",
/* 166 */ "maxing layer",
/* 167 */ "bottom of root zone",
/* 168 */ "reserved",
/* 169 */ "reserved",
/* 170 */ "reserved",
/* 171 */ "reserved",
/* 172 */ "reserved",
/* 173 */ "reserved",
/* 174 */ "reserved",
/* 175 */ "reserved",
/* 176 */ "reserved",
/* 177 */ "reserved",
/* 178 */ "reserved",
/* 179 */ "reserved",
/* 180 */ "reserved",
/* 181 */ "reserved",
/* 182 */ "reserved",
/* 183 */ "reserved",
/* 184 */ "reserved",
/* 185 */ "reserved",
/* 186 */ "reserved",
/* 187 */ "reserved",
/* 188 */ "reserved",
/* 189 */ "reserved",
/* 190 */ "reserved",
/* 191 */ "reserved"
};


int level1(int mode, int type, int undef_val, float value, int center, int subcenter, char *inv_out);
int level2(int mode, int type1, int undef_val1, float value1, int type2, int undef_val2, 
   float value2, int center, int subcenter, char *inv_out);

int f_lev(ARG0) {

    int level_type1, level_type2;
    float val1, val2;
    int undef_val1, undef_val2;
    int center, subcenter;

    if (mode < 0) return 0;

    center = GB2_Center(sec);
    subcenter = GB2_Subcenter(sec);

    fixed_surfaces(sec, &level_type1, &val1, &undef_val1, &level_type2, &val2, &undef_val2);

    if (mode > 1) {
	if (undef_val1 == 0) sprintf(inv_out,"lvl1=(%d,%lg) ",level_type1,val1);
	else sprintf(inv_out,"lvl1=(%d,missing) ",level_type1);
	inv_out += strlen(inv_out);

	if (undef_val2 == 0) sprintf(inv_out,"lvl2=(%d,%lg):",level_type2,val2);
	else sprintf(inv_out,"lvl2=(%d,missing):",level_type2);
	inv_out += strlen(inv_out);
    }

    level2(mode, level_type1, undef_val1, val1, level_type2, undef_val2, val2, center, subcenter, inv_out);
    return 0;
}

/*
 * level2 is for layers
 */

int level2(int mode, int type1, int undef_val1, float value1, int type2, int undef_val2, float value2, int center, int subcenter,
	char *inv_out) {

    if (type1 == 100 && type2 == 100) {
	sprintf(inv_out,"%g-%g mb",value1/100,value2/100);
    }
    else if (type1 == 102 && type2 == 102) {
	sprintf(inv_out,"%g-%g m above mean sea level",value1,value2);
    }
    else if (type1 == 103 && type2 == 103) {
	sprintf(inv_out,"%g-%g m above ground",value1,value2);
    }
    else if (type1 == 104 && type2 == 104) {
	sprintf(inv_out,"%g-%g sigma layer",value1,value2);
    }
    else if (type1 == 105 && type2 == 105) {
	sprintf(inv_out,"%g-%g hybrid layer",value1,value2);
    }
    else if (type1 == 106 && type2 == 106) {
	/* sprintf(inv_out,"%g-%g m below ground",value1/100,value2/100); removed 1/2007 */
	sprintf(inv_out,"%g-%g m below ground",value1,value2);
    }
    else if (type1 == 107 && type2 == 107) {
	sprintf(inv_out,"%g-%g K isentropic layer",value1,value2);
    }
    else if (type1 == 108 && type2 == 108) {
	sprintf(inv_out,"%g-%g mb above ground",value1/100,value2/100);
    }
    else if (type1 == 160 && type2 == 160) {
	sprintf(inv_out,"%g-%g m below sea level",value1,value2);
    }
    else if (type1 == 1 && type2 == 8) {
	sprintf(inv_out,"atmos col");		// compatible with wgrib
    }
    else if (type1 == 9 && type2 == 1) {
	sprintf(inv_out,"ocean column");
    }
    else if (center == NCEP && type1 == 235 && type2 == 235) {
	    sprintf(inv_out,"%g-%gC ocean isotherm layer", value1/10,value2/10);
    }
    else if (center == NCEP && type1 == 236 && type2 == 236) {	// obsolete
	    sprintf(inv_out,"%g-%g m ocean layer", value1*10,value2*10);
    }
    else if (type1 == 255 && type2 == 255) {
	    sprintf(inv_out,"no_level");
    }
    else {
        level1(mode, type1, undef_val1, value1, center, subcenter,inv_out);
	inv_out += strlen(inv_out);
        if (type2 != 255) {
	    sprintf(inv_out," - ");
	    inv_out += strlen(inv_out);
	    level1(mode, type2, undef_val2, value2, center, subcenter,inv_out);
        }
    }
    return 0;
}

/*
 * level1 is for a single level (not a layer)
 */

int level1(int mode, int type, int undef_val, float val, int center, int subcenter,char *inv_out) {

    const char *string;

    /* local table for NCEP */
    if (center == NCEP && type >= 192 && type <= 254) {
	if (type == 235) {
	    sprintf(inv_out,"%gC ocean isotherm", val/10);
	    return 0;
	}
	if (type == 241) {
	    sprintf(inv_out,"%g in sequence", val);
	    return 0;
	}

	string = NULL;
        switch (type) {
#include "CodeTable_4.5_ncep.dat"
	}
	if (string != NULL) {
	    sprintf(inv_out,string, val);
	    return 0;
	}
    }

    if (type == 100 || type == 108) val = val * 0.01;  // Pa -> mb

    // no numeric information
    if (type == 255) return 8;

    if (type < 192) {
	sprintf(inv_out,level_table[type], val);
    }
    else if (center == NCEP){
	if (undef_val == 0) sprintf(inv_out,"NCEP level type %d %g", type, val);
	else sprintf(inv_out,"NCEP level type %d", type);
    }
    else {
        if (undef_val == 0) sprintf(inv_out,"local level type %d %g", type, val);
        else sprintf(inv_out,"local level type %d", type);
    }

    return 0;
}
