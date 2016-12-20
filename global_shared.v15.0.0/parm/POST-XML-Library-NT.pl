#!/usr/bin/perl
# This is the XML library for POST XML implementation.
# The entry point "constract_ctrl_elements" take POST control and available table XML file to:
#    1. Query the XML files for matching component elements
#    2. Store control elements and matching parameters into array for return
#
# Detailed processing logic is as follow:
#    -  Find  how many type of paramsets using datset (i.e GFSPRS)
#    -  Constract and push 'name data' into $xml_query_array
#    -  Each array elememnt will be a line in final txt file.
#	 -  In each dataset get main txt body section elements
#    -  Apply to default value if not found
#    -  Constract and push 'name data' into @output_resultset_list which is used as final output array
#    -  If the data is array (i.e. level), also remember the size	
#	 -  Loop through all existing "param" section of control file
#    -  Find shortname and look in available table for a match
#    -  If no match ignore this shortname and keep looking
#    -  if found matching on shortname get all sub elements from ctrl file; set as default if found
#    -  If not found, looking in available file. 
#    -  If still not found in available file, use array [*][2] as default
#    -  Constract and push 'name data' into @output_resultset_list
#    -  Found result count for each XML query will be stored in array $xml_query_array[*][3]
#    -  $xml_query_array[0][3] is the counter of datset
#    -  Only write out a shortname entry when the short name is found in both control and available XML.
#    -  If parameter element is found in control XML, it will be set as default.
#    -  If parameter element is not found in control XML, than no default value will be set for that parameter.
#    -  If parameter element is found in control XML and same parameter element is also found in available XML, use value from control XML file.
#    -  If parameter element is found in control XML and same parameter element is not found in available XML, use value from control XML file.
#    -  If parameter element is not found in control XML and same parameter element is found in available XML, use value from available XML file.
#    -  If parameter element is not found in control XML and same parameter element is not found in available XML, ignore the value.
#    -  To see the n-th individual param in final flat file:
#    -    ((location the line of target shortname)-20)/37)+1 
# The final array of output will be:
# @output_resultset_list [*] [*]
#                        name (i.e. datset or bit_map_flag) from $xml_query_array[*][0]
#                            Result data (i.e. GFSPRS, NAM ...)
#
# Usage:             my @tmp_t1=constract_ctrl_elements();
#
# Input parameter:   none
#
# Input file:        POST control XML file and POST available table XML file.
#
# output:            @output_resultset_list
#
# 
# Lin Gan 201502 - Initial programming as version 1.0
# Lin Gan 201506 - Increase logic for reading real array on level entry version 1.1
#

  use strict;
  use warnings;
  
  use constant {

#--------------------------------------------------------------
#  Query string for main section of the Control XML file
#--------------------------------------------------------------

    data_set_query  => '/postxml/paramset/datset/text()',
    grid_num_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../grid_num/text()',
    sub_center_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../sub_center/text()',
    version_no_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../version_no/text()',
    local_table_vers_no_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../local_table_vers_no/text()',
    sigreftime_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../sigreftime/text()',
    prod_status_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../prod_status/text()',
    data_type_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../data_type/text()',
    gen_proc_type_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../gen_proc_type/text()',
    time_range_unit_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../time_range_unit/text()',
    orig_center_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../orig_center/text()',
    gen_proc_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../gen_proc/text()',
    packing_method_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../packing_method/text()',
    order_of_sptdiff_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../order_of_sptdiff/text()',
    field_datatype_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../field_datatype/text()',
    comprs_type_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../comprs_type/text()',
    type_ens_fcst_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../type_ens_fcst/text()',

#--------------------------------------------------------------	
#  Query string for param part of control file
#--------------------------------------------------------------

  	post_avblfldidx_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../post_avblfldidx/text()',
	longname_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../longname/text()',
	mass_windpoint_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../mass_windpoint/text()',
	pdstmpl_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../pdstmpl/text()',
	pname_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../pname/text()',
	table_info_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../table_info/text()',
	stats_proc_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../stats_proc/text()',
	fixed_sfc1_type_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../fixed_sfc1_type/text()',
	scale_fact_fixed_sfc1_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_fact_fixed_sfc1/text()',
	level_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../level/text()',
	fixed_sfc2_type_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../fixed_sfc2_type/text()',
	scale_fact_fixed_sfc2_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_fact_fixed_sfc2/text()',
	level2_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../level2/text()',
	aerosol_type_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../aerosol_type/text()',
	typ_intvl_size_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../typ_intvl_size/text()',
	scale_fact_1st_size_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_fact_1st_size/text()',
	scale_val_1st_size_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_val_1st_size/text()',
	scale_fact_2nd_size_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_fact_2nd_size/text()',
	scale_val_2nd_size_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_val_2nd_size/text()',
	typ_intvl_wvlen_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../typ_intvl_wvlen/text()',
	scale_fact_1st_wvlen_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_fact_1st_wvlen/text()',
	scale_val_1st_wvlen_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_val_1st_wvlen/text()',
	scale_fact_2nd_wvlen_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_fact_2nd_wvlen/text()',
	scale_val_2nd_wvlen_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale_val_2nd_wvlen/text()',
	scale_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../scale/text()',
	stat_miss_val_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../stat_miss_val/text()',
	leng_time_range_prev_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../leng_time_range_prev/text()',
	time_inc_betwn_succ_fld_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../time_inc_betwn_succ_fld/text()',
	type_of_time_inc_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../type_of_time_inc/text()',
	stat_unit_time_key_succ_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../stat_unit_time_key_succ/text()',
	bit_map_flag_query  => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname[text()=\'$inptwo\']/../bit_map_flag/text()',

#--------------------------------------------------------------
#  Query string for param part of available file
#--------------------------------------------------------------

  	post_avblfldidx_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../post_avblfldidx/text()',
	longname_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../longname/text()',
	mass_windpoint_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../mass_windpoint/text()',
	pdstmpl_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../pdstmpl/text()',
	pname_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../pname/text()',
	table_info_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../table_info/text()',
	stats_proc_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../stats_proc/text()',
	fixed_sfc1_type_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../fixed_sfc1_type/text()',
	scale_fact_fixed_sfc1_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_fact_fixed_sfc1/text()',
	level_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../level/text()',
	fixed_sfc2_type_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../fixed_sfc2_type/text()',
	scale_fact_fixed_sfc2_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_fact_fixed_sfc2/text()',
	level2_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../level2/text()',
	aerosol_type_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../aerosol_type/text()',
	typ_intvl_size_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../typ_intvl_size/text()',
	scale_fact_1st_size_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_fact_1st_size/text()',
	scale_val_1st_size_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_val_1st_size/text()',
	scale_fact_2nd_size_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_fact_2nd_size/text()',
	scale_val_2nd_size_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_val_2nd_size/text()',
	typ_intvl_wvlen_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../typ_intvl_wvlen/text()',
	scale_fact_1st_wvlen_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_fact_1st_wvlen/text()',
	scale_val_1st_wvlen_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_val_1st_wvlen/text()',
	scale_fact_2nd_wvlen_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_fact_2nd_wvlen/text()',
	scale_val_2nd_wvlen_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale_val_2nd_wvlen/text()',
	scale_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../scale/text()',
	stat_miss_val_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../stat_miss_val/text()',
	leng_time_range_prev_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../leng_time_range_prev/text()',
	time_inc_betwn_succ_fld_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../time_inc_betwn_succ_fld/text()',
	type_of_time_inc_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../type_of_time_inc/text()',
	stat_unit_time_key_succ_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../stat_unit_time_key_succ/text()',
	bit_map_flag_avil_query  => '/postxml/post_avblflds/param/shortname[text()=\'$inpone\']/../bit_map_flag/text()',
	
#--------------------------------------------------------------
# Find param using shortname matching in ctrl file
#--------------------------------------------------------------

	ctrl_param_shortname_query => '/postxml/paramset/datset[text()=\'$inpone\']/../param/shortname/text()',

#--------------------------------------------------------------
# Find param using shortname matching in avil file
#--------------------------------------------------------------

	avil_param_shortname_query => '/postxml/post_avblflds/param/shortname/text()',
  };


#--------------------------------------------------------------
# Set array of query where [*] [0]=txt file tag
#							   [1]=XML QL
#							   [2]=default value if not found 
#							   [3]=found counter
#							   [4]=found string data
#--------------------------------------------------------------

  my @xml_query_array;

#--------------------------------------------------------------
# Control XML  
#--------------------------------------------------------------

  $xml_query_array[0][0] = "datset";
  $xml_query_array[0][1] = data_set_query;
  $xml_query_array[0][2] = "?";
  $xml_query_array[1][0] = "grid_num";
  $xml_query_array[1][1] = grid_num_query;
  $xml_query_array[1][2] = "255";
  $xml_query_array[2][0] = "sub_center";
  $xml_query_array[2][1] = sub_center_query;
  $xml_query_array[2][2] = "?";
  $xml_query_array[3][0] = "version_no";
  $xml_query_array[3][1] = version_no_query;
  $xml_query_array[3][2] = "?";
  $xml_query_array[4][0] = "local_table_vers_no";
  $xml_query_array[4][1] = local_table_vers_no_query;
  $xml_query_array[4][2] = "?";
  $xml_query_array[5][0] = "sigreftime";
  $xml_query_array[5][1] = sigreftime_query;
  $xml_query_array[5][2] = "?";
  $xml_query_array[6][0] = "prod_status";
  $xml_query_array[6][1] = prod_status_query;
  $xml_query_array[6][2] = "?";
  $xml_query_array[7][0] = "data_type";
  $xml_query_array[7][1] = data_type_query;
  $xml_query_array[7][2] = "?";
  $xml_query_array[8][0] = "gen_proc_type";
  $xml_query_array[8][1] = gen_proc_type_query;
  $xml_query_array[8][2] = "?";
  $xml_query_array[9][0] = "time_range_unit";
  $xml_query_array[9][1] = time_range_unit_query;
  $xml_query_array[9][2] = "?";
  $xml_query_array[10][0] = "orig_center";
  $xml_query_array[10][1] = orig_center_query;
  $xml_query_array[10][2] = "?";
  $xml_query_array[11][0] = "gen_proc";
  $xml_query_array[11][1] = gen_proc_query;
  $xml_query_array[11][2] = "?";
  $xml_query_array[12][0] = "packing_method";
  $xml_query_array[12][1] = packing_method_query;
  $xml_query_array[12][2] = "?";
  $xml_query_array[13][0] = "order_of_sptdiff";
  $xml_query_array[13][1] = order_of_sptdiff_query;
  $xml_query_array[13][2] = "1st_ord_sptdiff";
  $xml_query_array[14][0] = "field_datatype";
  $xml_query_array[14][1] = field_datatype_query;
  $xml_query_array[14][2] = "?";
  $xml_query_array[15][0] = "comprs_type";
  $xml_query_array[15][1] = comprs_type_query;
  $xml_query_array[15][2] = "?";
  $xml_query_array[16][0] = "type_ens_fcst";
  $xml_query_array[16][1] = type_ens_fcst_query;
  $xml_query_array[16][2] = "?";

#--------------------------------------------------------------
# param section in control table 
#--------------------------------------------------------------

  $xml_query_array[26][0] = "post_avblfldidx";
  $xml_query_array[26][1] = post_avblfldidx_query;
  $xml_query_array[26][2] = "-9999";
  $xml_query_array[27][0] = "shortname";
  $xml_query_array[28][0] = "longname";
  $xml_query_array[28][1] = longname_query;
  $xml_query_array[28][2] = "?";
  $xml_query_array[29][0] = "mass_windpoint";
  $xml_query_array[29][1] = mass_windpoint_query;
  $xml_query_array[29][2] = "1";
  $xml_query_array[30][0] = "pdstmpl";
  $xml_query_array[30][1] = pdstmpl_query;
  $xml_query_array[30][2] = "tmpl4_0";
  $xml_query_array[31][0] = "pname";
  $xml_query_array[31][1] = pname_query;
  $xml_query_array[31][2] = "?";
  $xml_query_array[32][0] = "table_info";
  $xml_query_array[32][1] = table_info_query;
  $xml_query_array[32][2] = "?";
  $xml_query_array[33][0] = "stats_proc";
  $xml_query_array[33][1] = stats_proc_query;
  $xml_query_array[33][2] = "?";
  $xml_query_array[34][0] = "fixed_sfc1_type";
  $xml_query_array[34][1] = fixed_sfc1_type_query;
  $xml_query_array[34][2] = "?";
  $xml_query_array[35][0] = "scale_fact_fixed_sfc1";
  $xml_query_array[35][1] = scale_fact_fixed_sfc1_query;
  $xml_query_array[35][2] = "?";
  $xml_query_array[36][0] = "level";
  $xml_query_array[36][1] = level_query;
  $xml_query_array[36][2] = "?";
  $xml_query_array[37][0] = "fixed_sfc2_type";
  $xml_query_array[37][1] = fixed_sfc2_type_query;
  $xml_query_array[37][2] = "?";
  $xml_query_array[38][0] = "scale_fact_fixed_sfc2";
  $xml_query_array[38][1] = scale_fact_fixed_sfc2_query;
  $xml_query_array[38][2] = "?";
  $xml_query_array[39][0] = "level2";
  $xml_query_array[39][1] = level2_query;
  $xml_query_array[39][2] = "?";
  $xml_query_array[40][0] = "aerosol_type";
  $xml_query_array[40][1] = aerosol_type_query;
  $xml_query_array[40][2] = "?";
  $xml_query_array[41][0] = "typ_intvl_size";
  $xml_query_array[41][1] = typ_intvl_size_query;
  $xml_query_array[41][2] = "?";
  $xml_query_array[42][0] = "scale_fact_1st_size";
  $xml_query_array[42][1] = scale_fact_1st_size_query;
  $xml_query_array[42][2] = "0";
  $xml_query_array[43][0] = "scale_val_1st_size";
  $xml_query_array[43][1] = scale_val_1st_size_query;
  $xml_query_array[43][2] = "0.0";
  $xml_query_array[44][0] = "scale_fact_2nd_size";
  $xml_query_array[44][1] = scale_fact_2nd_size_query;
  $xml_query_array[44][2] = "0";
  $xml_query_array[45][0] = "scale_val_2nd_size";
  $xml_query_array[45][1] = scale_val_2nd_size_query;
  $xml_query_array[45][2] = "0.0";
  $xml_query_array[46][0] = "typ_intvl_wvlen";
  $xml_query_array[46][1] = typ_intvl_wvlen_query;
  $xml_query_array[46][2] = "?";
  $xml_query_array[47][0] = "scale_fact_1st_wvlen";
  $xml_query_array[47][1] = scale_fact_1st_wvlen_query;
  $xml_query_array[47][2] = "0";
  $xml_query_array[48][0] = "scale_val_1st_wvlen";
  $xml_query_array[48][1] = scale_val_1st_wvlen_query;
  $xml_query_array[48][2] = "0.0";
  $xml_query_array[49][0] = "scale_fact_2nd_wvlen";
  $xml_query_array[49][1] = scale_fact_2nd_wvlen_query;
  $xml_query_array[49][2] = "0";
  $xml_query_array[50][0] = "scale_val_2nd_wvlen";
  $xml_query_array[50][1] = scale_val_2nd_wvlen_query;
  $xml_query_array[50][2] = "0.0";
  $xml_query_array[51][0] = "scale";
  $xml_query_array[51][1] = scale_query;
  $xml_query_array[51][2] = "?";
  $xml_query_array[52][0] = "stat_miss_val";
  $xml_query_array[52][1] = stat_miss_val_query;
  $xml_query_array[52][2] = "0";
  $xml_query_array[53][0] = "leng_time_range_prev";
  $xml_query_array[53][1] = leng_time_range_prev_query;
  $xml_query_array[53][2] = "0";
  $xml_query_array[54][0] = "time_inc_betwn_succ_fld";
  $xml_query_array[54][1] = time_inc_betwn_succ_fld_query;
  $xml_query_array[54][2] = "0";
  $xml_query_array[55][0] = "type_of_time_inc";
  $xml_query_array[55][1] = type_of_time_inc_query;
  $xml_query_array[55][2] = "?";
  $xml_query_array[56][0] = "stat_unit_time_key_succ";
  $xml_query_array[56][1] = stat_unit_time_key_succ_query;
  $xml_query_array[56][2] = "?";
  $xml_query_array[57][0] = "bit_map_flag";
  $xml_query_array[57][1] = bit_map_flag_query;
  $xml_query_array[57][2] = "?";

#--------------------------------------------------------------    
# param section in available table 
#--------------------------------------------------------------

  $xml_query_array[66][0] = "post_avblfldidx";
  $xml_query_array[66][1] = post_avblfldidx_avil_query;
  $xml_query_array[66][2] = "-9999";
  $xml_query_array[67][0] = "shortname";
  $xml_query_array[68][0] = "longname";
  $xml_query_array[68][1] = longname_avil_query;
  $xml_query_array[68][2] = "?";
  $xml_query_array[69][0] = "mass_windpoint";
  $xml_query_array[69][1] = mass_windpoint_avil_query;
  $xml_query_array[69][2] = "1";
  $xml_query_array[70][0] = "pdstmpl";
  $xml_query_array[70][1] = pdstmpl_avil_query;
  $xml_query_array[70][2] = "tmpl4_0";
  $xml_query_array[71][0] = "pname";
  $xml_query_array[71][1] = pname_avil_query;
  $xml_query_array[71][2] = "?";
  $xml_query_array[72][0] = "table_info";
  $xml_query_array[72][1] = table_info_avil_query;
  $xml_query_array[72][2] = "?";
  $xml_query_array[73][0] = "stats_proc";
  $xml_query_array[73][1] = stats_proc_avil_query;
  $xml_query_array[73][2] = "?";
  $xml_query_array[74][0] = "fixed_sfc1_type";
  $xml_query_array[74][1] = fixed_sfc1_type_avil_query;
  $xml_query_array[74][2] = "?";
  $xml_query_array[75][0] = "scale_fact_fixed_sfc1";
  $xml_query_array[75][1] = scale_fact_fixed_sfc1_avil_query;
  $xml_query_array[75][2] = "?";
  $xml_query_array[76][0] = "level";
  $xml_query_array[76][1] = level_avil_query;
  $xml_query_array[76][2] = "?";
  $xml_query_array[77][0] = "fixed_sfc2_type";
  $xml_query_array[77][1] = fixed_sfc2_type_avil_query;
  $xml_query_array[77][2] = "?";
  $xml_query_array[78][0] = "scale_fact_fixed_sfc2";
  $xml_query_array[78][1] = scale_fact_fixed_sfc2_avil_query;
  $xml_query_array[78][2] = "?";
  $xml_query_array[79][0] = "level2";
  $xml_query_array[79][1] = level2_avil_query;
  $xml_query_array[79][2] = "?";
  $xml_query_array[80][0] = "aerosol_type";
  $xml_query_array[80][1] = aerosol_type_avil_query;
  $xml_query_array[80][2] = "?";
  $xml_query_array[81][0] = "typ_intvl_size";
  $xml_query_array[81][1] = typ_intvl_size_avil_query;
  $xml_query_array[81][2] = "?";
  $xml_query_array[82][0] = "scale_fact_1st_size";
  $xml_query_array[82][1] = scale_fact_1st_size_avil_query;
  $xml_query_array[82][2] = "0";
  $xml_query_array[83][0] = "scale_val_1st_size";
  $xml_query_array[83][1] = scale_val_1st_size_avil_query;
  $xml_query_array[83][2] = "0.0";
  $xml_query_array[84][0] = "scale_fact_2nd_size";
  $xml_query_array[84][1] = scale_fact_2nd_size_avil_query;
  $xml_query_array[84][2] = "0";
  $xml_query_array[85][0] = "scale_val_2nd_size";
  $xml_query_array[85][1] = scale_val_2nd_size_avil_query;
  $xml_query_array[85][2] = "0.0";
  $xml_query_array[86][0] = "typ_intvl_wvlen";
  $xml_query_array[86][1] = typ_intvl_wvlen_avil_query;
  $xml_query_array[86][2] = "?";
  $xml_query_array[87][0] = "scale_fact_1st_wvlen";
  $xml_query_array[87][1] = scale_fact_1st_wvlen_avil_query;
  $xml_query_array[87][2] = "0";
  $xml_query_array[88][0] = "scale_val_1st_wvlen";
  $xml_query_array[88][1] = scale_val_1st_wvlen_avil_query;
  $xml_query_array[88][2] = "0.0";
  $xml_query_array[89][0] = "scale_fact_2nd_wvlen";
  $xml_query_array[89][1] = scale_fact_2nd_wvlen_avil_query;
  $xml_query_array[89][2] = "0";
  $xml_query_array[90][0] = "scale_val_2nd_wvlen";
  $xml_query_array[90][1] = scale_val_2nd_wvlen_avil_query;
  $xml_query_array[90][2] = "0.0";
  $xml_query_array[91][0] = "scale";
  $xml_query_array[91][1] = scale_avil_query;
  $xml_query_array[91][2] = "?";
  $xml_query_array[92][0] = "stat_miss_val";
  $xml_query_array[92][1] = stat_miss_val_avil_query;
  $xml_query_array[92][2] = "0";
  $xml_query_array[93][0] = "leng_time_range_prev";
  $xml_query_array[93][1] = leng_time_range_prev_avil_query;
  $xml_query_array[93][2] = "0";
  $xml_query_array[94][0] = "time_inc_betwn_succ_fld";
  $xml_query_array[94][1] = time_inc_betwn_succ_fld_avil_query;
  $xml_query_array[94][2] = "0";
  $xml_query_array[95][0] = "type_of_time_inc";
  $xml_query_array[95][1] = type_of_time_inc_avil_query;
  $xml_query_array[95][2] = "?";
  $xml_query_array[96][0] = "stat_unit_time_key_succ";
  $xml_query_array[96][1] = stat_unit_time_key_succ_avil_query;
  $xml_query_array[96][2] = "?";
  $xml_query_array[97][0] = "bit_map_flag";
  $xml_query_array[97][1] = bit_map_flag_avil_query;
  $xml_query_array[97][2] = "?";

#--------------------------------------------------------------  
# use XML LibXML package
#--------------------------------------------------------------

  use XML::LibXML;

#--------------------------------------------------------------
# If generate_tag_name is d, target will be generated as debug mode
#--------------------------------------------------------------

  our $generate_tag_name;
  our $control_xml_name;
  our $available_xml_name;

#--------------------------------------------------------------
# Structure Construction
#--------------------------------------------------------------

  my $ctrl_parser = XML::LibXML->new();
  my $avil_parser = XML::LibXML->new();

#--------------------------------------------------------------
# Read in XMl file and Organize the document
#--------------------------------------------------------------

  my $XML_dir=$ENV{'PWD'};
  (defined($XML_dir) || !$XML_dir) or die  "Environment variable DATA not set.\n";

  our $ctrl_doc    = "";
  our $avil_doc    = "";

#--------------------------------------------------------------
# Final return array
#--------------------------------------------------------------

  my @output_resultset_list=();

sub constract_ctrl_elements {

#--------------------------------------------------------------
#	Calling mode
#--------------------------------------------------------------

	$generate_tag_name  = shift;

#--------------------------------------------------------------
#       Calling variable
#--------------------------------------------------------------
        
        $control_xml_name = shift;
        $available_xml_name = shift;
        my $ctrl_xml_file = "${control_xml_name}";
        (defined($ctrl_xml_file) || !$ctrl_xml_file) or die  "Variable ctrl_xml_file is missing.\n";
        my $avil_xml_file = "${available_xml_name}";
        (defined($avil_xml_file) || !$avil_xml_file) or die  "Variable available_xml_name is missing.\n";
  
  $ctrl_doc    = $ctrl_parser->parse_file($ctrl_xml_file);
  $avil_doc    = $avil_parser->parse_file($avil_xml_file);

#--------------------------------------------------------------
#	Final result set list
#--------------------------------------------------------------

        my @found_resultset_list =();

#--------------------------------------------------------------    
#	input XML node
#--------------------------------------------------------------

	my $inpdoc = $ctrl_doc;

#--------------------------------------------------------------	    
#	Final Return array
#--------------------------------------------------------------

	my $resultset_array;
	
#--------------------------------------------------------------
#	XML query result nodes
#--------------------------------------------------------------

	my $resultset_nodes_dataset;

#--------------------------------------------------------------
#	Each individual query result set	
#--------------------------------------------------------------

	my $found_result_txt;

#--------------------------------------------------------------	
#	Error handling
#--------------------------------------------------------------

	return unless defined $inpdoc;

#--------------------------------------------------------------
	#	GEFS HardWire
#--------------------------------------------------------------
	my $gefs_only_element = "type_ens_fcst";
	my $gefs_exception_identifier = "ens_fcst";
	my $gefs_case=0;

#--------------------------------------------------------------	
#   Counter of array sets:
#--------------------------------------------------------------
	my $paramset_count=0;
	my $param_count=0;

#--------------------------------------------------------------	
#   Known array size
#--------------------------------------------------------------
	my $level_array_count=0;
	my $level2_array_count=0;
	my $scale_fact_fixed_sfc1_array_count=0;
        my $scale_fact_fixed_sfc2_array_count=0;
        my $scale_array_count=0;

#--------------------------------------------------------------    
#   Find  how many type of datset (i.e GFSPRS)
#   Start query and reset the match counter
#--------------------------------------------------------------

	$resultset_nodes_dataset = $inpdoc->findnodes(data_set_query);
	$xml_query_array[0][3] = 0;
	
#--------------------------------------------------------------
# 	The datset currently working on (i.e. GFSPRS) string
#--------------------------------------------------------------

	my $current_q_dataset_stg = "";

#--------------------------------------------------------------	
#	iter resultsets of datset
#--------------------------------------------------------------

	foreach my $individual_result_node ($resultset_nodes_dataset->get_nodelist) {

#--------------------------------------------------------------	  
# Get string value for current datset
# Constract and push 'name data' into @output_resultset_list
# store the result txt into tmp holder
#--------------------------------------------------------------

      $current_q_dataset_stg=$individual_result_node->string_value();

#--------------------------------------------------------------      
# constract string of txt (i.e. "datset GFSPRS")
#--------------------------------------------------------------

	  $found_result_txt = constract_string_txt ($xml_query_array[0][0],$current_q_dataset_stg);

#--------------------------------------------------------------	  
# Construct return array of string and increase 1 for match count
#--------------------------------------------------------------

	  $paramset_count ++;

#--------------------------------------------------------------	  
# Reset param counter for starting of each new datset
#--------------------------------------------------------------

	  $param_count=0;
	  
#--------------------------------------------------------------
# Write out write out current data
#--------------------------------------------------------------

	  push @output_resultset_list, $found_result_txt;

#--------------------------------------------------------------	  
# In each dataset get main txt body section elements	    
# Init result set node for main level search
#--------------------------------------------------------------

	  my @found_array_ctrl_main=();

#--------------------------------------------------------------	  
# Start to search until reach total number of the element to search as defined above 	
#--------------------------------------------------------------

	  my $emt_ctrl_main = 1;

#--------------------------------------------------------------	    
# Size of the array
#--------------------------------------------------------------

	  my $size_of_current_array=0;
	  
	  do
	  {

#--------------------------------------------------------------
# Start the query expect only single string for each main element of the ctrl XML file
#--------------------------------------------------------------

	   	@found_array_ctrl_main = find_array_using_crtl_xml ($xml_query_array[$emt_ctrl_main][1],$current_q_dataset_stg);

#--------------------------------------------------------------	    	
# Reset result set counter for this query
#--------------------------------------------------------------

	   	$xml_query_array[$emt_ctrl_main][3] = 0;

#--------------------------------------------------------------	    	
# If size of return array is zero then use default
#--------------------------------------------------------------

	   	$size_of_current_array=@found_array_ctrl_main;
	   	if ($size_of_current_array==0){
	          $found_result_txt = constract_string_txt ($xml_query_array[$emt_ctrl_main][0],$xml_query_array[$emt_ctrl_main][2]);
#--------------------------------------------------------------
# Only write when is not special case
#--------------------------------------------------------------
	          if (!(($emt_ctrl_main==16) && ($gefs_case==0))){
	   	    push (@output_resultset_list, $found_result_txt);
	          }
	   	}

#--------------------------------------------------------------	    	
# Get the matching array of elements
#--------------------------------------------------------------

	   	foreach (@found_array_ctrl_main) {

#--------------------------------------------------------------
# increase 1 for match count
#--------------------------------------------------------------

	      $xml_query_array[$emt_ctrl_main][3] =  $xml_query_array[$emt_ctrl_main][3] + 1 ;

#--------------------------------------------------------------	    		
# get indivial result text
#--------------------------------------------------------------

	      my $result_text_ctrl_main = $_;

#--------------------------------------------------------------	    		
# Constract and push 'name data' into @output_resultset_list
#--------------------------------------------------------------

	      $found_result_txt = constract_string_txt ($xml_query_array[$emt_ctrl_main][0],$result_text_ctrl_main);

#--------------------------------------------------------------	    		
# Store in final output
# If it is GEFS case then write out type_ens_fcst
#--------------------------------------------------------------

	      if ($result_text_ctrl_main eq "ens_fcst"){
	      	$gefs_case=1;
	      }
	      
	      # Only write when is not special case
	      if (!(($emt_ctrl_main==16) && ($gefs_case==0))){		
	        # Store in final output
	        push (@output_resultset_list, $found_result_txt);
	      }	      
	    }

#--------------------------------------------------------------	    
# goto next element
#--------------------------------------------------------------

	    $emt_ctrl_main++;
	    	
	  } while ($emt_ctrl_main <= 16);

#--------------------------------------------------------------
	  # finish XML query of the main section on ctrl XML
#--------------------------------------------------------------
	    
#--------------------------------------------------------------
# Loop through all existing "param" section of control file
# Find shortname and look in available table for a match
# Init result set node for main level search
#--------------------------------------------------------------

	  my @found_shortname_crtl_xml=();
	  @found_shortname_crtl_xml = find_array_using_crtl_xml (ctrl_param_shortname_query,$current_q_dataset_stg);

#--------------------------------------------------------------	  
# Init result set for shortname query in available table XML
#--------------------------------------------------------------

	  my @found_shortname_avil_xml=();

#--------------------------------------------------------------	  
# place holder for shortname text in avail file
#--------------------------------------------------------------

	  my $shortname_tmp_text_avil="";

#--------------------------------------------------------------	   
# For each existing shortname in ctrl file
#--------------------------------------------------------------

	  foreach (@found_shortname_crtl_xml) {

#--------------------------------------------------------------	  
# get shortname from control file query
#--------------------------------------------------------------

	   	my $shortname_text_ctrl = $_;

#--------------------------------------------------------------	    	
# Get object for all shortname of the available file
#--------------------------------------------------------------

	   	@found_shortname_avil_xml=find_array_using_avil_xml (avil_param_shortname_query,$shortname_text_ctrl);

#--------------------------------------------------------------	    	
# In each shortname found in avail file try to match the one from ctrl file
#--------------------------------------------------------------

	   	foreach (@found_shortname_avil_xml) {

#--------------------------------------------------------------	  	  
# Try to find same shortname element in available file
# get indivial shortname from avil file
#--------------------------------------------------------------

	      my $shortname_tmp_text_avil = $_;

#--------------------------------------------------------------	    		
# if it is a match with the shortname found above
#--------------------------------------------------------------

	      if ($shortname_text_ctrl eq $shortname_tmp_text_avil) {

#--------------------------------------------------------------	    	
# params counter plus one
#--------------------------------------------------------------

	    	$param_count++;

#--------------------------------------------------------------	    	    
# read all param elements from ctrl file
# set record found counter 
# record found element data in $xml_query_array[$emt_ctrl_main][4]
# loop from array # 26-57 (see above) 
#--------------------------------------------------------------

	    	my $param_ctrl_count=26;
	    	my @found_sub_param_crtl=();
	    	my $number_space_found=0;
	    	my $tmp_regex_st="";
	    	my $test_int=0;
            		
	    	do {

#--------------------------------------------------------------
# here shortname need to have a space in front
#--------------------------------------------------------------

                 @found_sub_param_crtl = find_array_using_crtl_xml ($xml_query_array[$param_ctrl_count][1],$current_q_dataset_stg," ".$shortname_text_ctrl);

#--------------------------------------------------------------                 
# if no result found, take out the leading space and try again
#--------------------------------------------------------------

                 $test_int = @found_sub_param_crtl;
                 if ($test_int == 0) {
                 	@found_sub_param_crtl = find_array_using_crtl_xml ($xml_query_array[$param_ctrl_count][1],$current_q_dataset_stg,$shortname_text_ctrl);
                 } 
                 
#--------------------------------------------------------------                 
# If target is an array, using regex to find out how many members within
#--------------------------------------------------------------

	    		 foreach (@found_sub_param_crtl) {
	    		   $xml_query_array[$param_ctrl_count][3]++;

#--------------------------------------------------------------
# take out new line hidden char
#--------------------------------------------------------------

	    		   $tmp_regex_st = $_;
	    		   $tmp_regex_st =~ s/\n//g;
                           $tmp_regex_st =~ s/\t/ /g;
	    		   $xml_query_array[$param_ctrl_count][4] = $tmp_regex_st;
	    		   
#--------------------------------------------------------------	    				  
# get array size and store in variable
#--------------------------------------------------------------

	    		   if ($param_ctrl_count == 35) {
                             $tmp_regex_st=~ s/\s{2}//g;
                             $number_space_found = () = $tmp_regex_st =~/\s/g;
	    		     $scale_fact_fixed_sfc1_array_count=$number_space_found+1;
	    		   }
	    		   if ($param_ctrl_count == 36) {
#                             $tmp_regex_st=~ s/\s{2}//g;
                             $tmp_regex_st =~ s/\s//g;
                             $tmp_regex_st =~ s/\./\. /g;
                             $tmp_regex_st = trim ($tmp_regex_st);                            

	    		     $number_space_found = () = $tmp_regex_st =~/\s/g;
	    		     $level_array_count=$number_space_found+1;
	    		   }
	    		   if ($param_ctrl_count == 38) {
                             $tmp_regex_st=~ s/\s{2}//g;
	    		     $number_space_found = () = $tmp_regex_st =~/\s/g;
	    		     $scale_fact_fixed_sfc2_array_count=$number_space_found+1;
	    		   }
	    		   if ($param_ctrl_count == 39) {
#                             $tmp_regex_st=~ s/\s{2}//g;
                             $tmp_regex_st =~ s/\s//g;
                             $tmp_regex_st =~ s/\./\. /g;
                             $tmp_regex_st = trim ($tmp_regex_st);

	    		     $number_space_found = () = $tmp_regex_st =~/\s/g;
	    		     $level2_array_count=$number_space_found+1;
	    		   }
	    		   if ($param_ctrl_count == 51) {
                             $tmp_regex_st=~ s/\s{2}//g;
	    		     $number_space_found = () = $tmp_regex_st =~/\s/g;
	    		     $scale_array_count=$number_space_found+1;
	    		   }
                           $xml_query_array[$param_ctrl_count][4] = $tmp_regex_st;
	    		 }
	    		
#--------------------------------------------------------------		
# If size of return array is zero then use default
#--------------------------------------------------------------

	    		 $size_of_current_array=@found_sub_param_crtl;
	    		 if ($size_of_current_array==0){
	    		   $xml_query_array[$param_ctrl_count][4]=$xml_query_array[$param_ctrl_count][2];
	    		   $xml_query_array[$param_ctrl_count][3]=0;
	    		 }
	    				
	    		 $param_ctrl_count ++;
	    		 
	    	 } while ($param_ctrl_count<=57);

#--------------------------------------------------------------	    			
# Store in short name
#--------------------------------------------------------------

	    	 $xml_query_array[27][4]=$shortname_text_ctrl;

#--------------------------------------------------------------	    			
# loop through counter array element in ctrl param data
# read all param elements from avil file if not found in ctrl param section
# set record found in array in "ctrl counter" -  $xml_query_array[$emt_ctrl_main][4]
# loop from array # 66-97 (see above)
#--------------------------------------------------------------

	    	 my $param_avil_count=66;
	    	 my @found_sub_param_avil=();
	    	 my $fnd_cnt=0;
	    	 do {
	    	 	  $fnd_cnt = $xml_query_array[$param_avil_count-40][3];
	    	      if ((defined $fnd_cnt) && ($fnd_cnt==0)){
	    			@found_sub_param_avil = find_array_using_avil_xml ($xml_query_array[$param_avil_count][1],$shortname_text_ctrl);
	    	
#--------------------------------------------------------------		
# if no result found, add a leading space and try again
#--------------------------------------------------------------

                    $test_int = @found_sub_param_avil;
                    if ($test_int == 0) {
                 	  @found_sub_param_avil = find_array_using_avil_xml ($xml_query_array[$param_avil_count][1]," ".$shortname_text_ctrl);
                    }
                 
	    			foreach (@found_sub_param_avil) {
	    			  $xml_query_array[$param_avil_count-40][3]++;
#--------------------------------------------------------------
# take out new line hidden char
#--------------------------------------------------------------

	    		      $tmp_regex_st = $_;
	    		      $tmp_regex_st =~ s/\n//g;
                              $tmp_regex_st =~ s/\t/ /g;
	    		      $xml_query_array[$param_avil_count-40][4] = $tmp_regex_st;

#--------------------------------------------------------------	    				    
# get array size and store in variable
#--------------------------------------------------------------

	    			  if ($param_avil_count == 75) {
                                    $tmp_regex_st=~ s/\s{2}//g;
	    			    $number_space_found = () = $tmp_regex_st =~/\s/g;
	    			    $scale_fact_fixed_sfc1_array_count=$number_space_found+1;
	    			  }
	    			  if ($param_avil_count == 76) {
#                                    $tmp_regex_st=~ s/\s{2}//g;
                                     $tmp_regex_st =~ s/\s//g;
                                     $tmp_regex_st =~ s/\./\. /g;
                                     $tmp_regex_st = trim ($tmp_regex_st);

	    			    $number_space_found = () = $tmp_regex_st =~/\s/g;
	    			    $level_array_count=$number_space_found+1;
	    			  }
	    			  if ($param_avil_count == 78) {
                                    $tmp_regex_st=~ s/\s{2}//g;
	    			    $number_space_found = () = $tmp_regex_st =~/\s/g;
	    			    $scale_fact_fixed_sfc2_array_count=$number_space_found+1;
	    			  }
	    			  if ($param_avil_count == 79) {
#                                   $tmp_regex_st=~ s/\s{2}//g;
                                    $tmp_regex_st =~ s/\s//g;
                                    $tmp_regex_st =~ s/\./\. /g;
                                    $tmp_regex_st = trim ($tmp_regex_st);

	    			    $number_space_found = () = $tmp_regex_st =~/\s/g;
	    			    $level2_array_count=$number_space_found+1;
	    			  }
	    			  if ($param_avil_count == 91) {
                                    $tmp_regex_st=~ s/\s{2}//g;
 	    		            $number_space_found = () = $tmp_regex_st =~/\s/g;
	    		            $scale_array_count=$number_space_found+1;
	    		          }
                                  $xml_query_array[$param_avil_count-40][4] = $tmp_regex_st;
	    		   }
	    		 }
	    		 		
	    		 $param_avil_count ++;
	    				
	    	 } while ($param_avil_count<=97);
	    			
#--------------------------------------------------------------	    			
# loop through all param section on ctrl array find those not found
#   apply the default ($xml_query_array[$emt_ctrl_main][2])value 
#   to $xml_query_array[$emt_ctrl_main][4]
# record found element data using contain from:
#    $xml_query_array[$emt_ctrl_main][0] and  $xml_query_array[$emt_ctrl_main][4]
# Final output will be stored in @output_resultset_list
#--------------------------------------------------------------

	    	 my $all_param_count=26;
	         do {

#--------------------------------------------------------------
# Constract and push 'name data' into @output_resultset_list
#--------------------------------------------------------------

	              $found_result_txt = constract_string_txt ($xml_query_array[$all_param_count][0],$xml_query_array[$all_param_count][4]);

#--------------------------------------------------------------	         
# Store array counter before array element data
#--------------------------------------------------------------

	              if ($all_param_count == 35) {
	    	        push (@output_resultset_list, constract_string_txt("scale_fact_fixed_sfc1_array_count", $scale_fact_fixed_sfc1_array_count));
	    	        $scale_fact_fixed_sfc1_array_count=0;
	    		  }
	    		  if ($all_param_count == 36) {
	    		    push (@output_resultset_list, constract_string_txt("level_array_count",$level_array_count));
	    		    $level_array_count=0;
	    		  }
	    		  if ($all_param_count == 38) {
	    		    push (@output_resultset_list, constract_string_txt("scale_fact_fixed_sfc2_array_count",$scale_fact_fixed_sfc2_array_count));
	    		    $scale_fact_fixed_sfc2_array_count=0;
	    		  }
	    		  if ($all_param_count == 39) {
	    		    push (@output_resultset_list, constract_string_txt("level2_array_count",$level2_array_count));
	    		    $level2_array_count=0;
	    		  }
	    		  if ($all_param_count == 51) {
	    		    push (@output_resultset_list, constract_string_txt("scale_array_count",$scale_array_count));
	    		    $scale_array_count=0;
	    		  }
	    		 
#-------------------------------------------------------------- 
# Store in final output
#--------------------------------------------------------------

	              push (@output_resultset_list, $found_result_txt);
	              
	    		  $xml_query_array[$all_param_count][3]=0; 
	    		  $xml_query_array[$all_param_count][4]=""; 
	    				 
	    	     $all_param_count ++;	  
	         } while ($all_param_count<=57);
	    		
	    			 
	       }

#--------------------------------------------------------------
# if it is not a match; ignore the current shortname 
#--------------------------------------------------------------

       	 }

#--------------------------------------------------------------
# end of looping through shortname in available file
#--------------------------------------------------------------
	    	
	   } 

#--------------------------------------------------------------
# Write out total count of param
# This will be in FILO structure
# Read paramset_count and loop from second element of the array to get it
#--------------------------------------------------------------

       unshift @output_resultset_list, constract_string_txt ("param_count", $param_count);
    	
	}

#--------------------------------------------------------------
# end of processing datset of ctrl file
#--------------------------------------------------------------
    
#--------------------------------------------------------------
# Write out total count of paramset
#--------------------------------------------------------------

	unshift @output_resultset_list, constract_string_txt ("paramset_count", $paramset_count);
	
		
	return @output_resultset_list;	
}

#--------------------------------------------------------------
# Used to constract a string of name - space - value. For final output
#--------------------------------------------------------------

sub constract_string_txt {

#--------------------------------------------------------------
# Setup query string
#--------------------------------------------------------------

	my $text_tag  = shift;

#--------------------------------------------------------------	
# Setup query string
#--------------------------------------------------------------

	my $contain_txt  = shift;

        my $return_text_str;

if ($generate_tag_name eq "1") {
  $return_text_str = trim($text_tag . " " . trim($contain_txt));
}
else {
	
#--------------------------------------------------------------
# Constract return txt without name tag
#--------------------------------------------------------------
  $return_text_str = trim($contain_txt);
}
	
	return $return_text_str; 
		
}

sub find_array_using_crtl_xml {

#--------------------------------------------------------------
# Setup query string
#--------------------------------------------------------------

	my $query_string  = shift;

#--------------------------------------------------------------
# First input parameter
#--------------------------------------------------------------

	my $inpone = shift;

#--------------------------------------------------------------
# Second input parameter
#--------------------------------------------------------------

	my $inptwo = shift;

#--------------------------------------------------------------	
# Final result set list
#--------------------------------------------------------------

        my @found_resultset_list =();

#--------------------------------------------------------------    
# input XML node
#--------------------------------------------------------------

	my $inpdoc = $ctrl_doc;

#--------------------------------------------------------------	
# Return array
#--------------------------------------------------------------

	my $resultset_array;

#--------------------------------------------------------------	
# XML query result nodes
#--------------------------------------------------------------

	my $resultset_nodes;

#--------------------------------------------------------------
# Each individual area	
#--------------------------------------------------------------

	my $individual_result;

#--------------------------------------------------------------	
# Error handling
#--------------------------------------------------------------

	return unless defined $inpdoc;
	return unless defined $query_string;

#--------------------------------------------------------------	
# replace query atring variable with real value
#--------------------------------------------------------------

	$query_string =~ s/\$inpone/$inpone/g;
	$query_string =~ s/\$inptwo/$inptwo/g; 

#--------------------------------------------------------------
# Start query
#--------------------------------------------------------------

	$resultset_nodes = $inpdoc->findnodes($query_string);

#--------------------------------------------------------------	
# iter resultsets
#--------------------------------------------------------------

	foreach my $individual_result_node ($resultset_nodes->get_nodelist) {
	
#--------------------------------------------------------------	
# Get string value for debugging purpose
#--------------------------------------------------------------

		$individual_result = trim($individual_result_node->string_value());

#--------------------------------------------------------------	
# Construct linked list
#--------------------------------------------------------------

		push (@found_resultset_list, $individual_result);
		
	}
	return @found_resultset_list;	
}

sub find_array_using_avil_xml {

#--------------------------------------------------------------
# Setup query string
#--------------------------------------------------------------

	my $query_string  = shift;

#--------------------------------------------------------------
# First input parameter
#--------------------------------------------------------------

	my $inpone = shift;

#--------------------------------------------------------------
# Second input parameter
#--------------------------------------------------------------

	my $inptwo = shift;

#--------------------------------------------------------------		
# Final result set list
#--------------------------------------------------------------

        my @found_resultset_list =();

#--------------------------------------------------------------    
# input XML node
#--------------------------------------------------------------

	my $inpdoc = $avil_doc;

#--------------------------------------------------------------	
# Return array
#--------------------------------------------------------------

	my $resultset_array;

#--------------------------------------------------------------	
# XML query result nodes
#--------------------------------------------------------------

	my $resultset_nodes;

#--------------------------------------------------------------
# Each individual area	
#--------------------------------------------------------------

	my $individual_result;

#--------------------------------------------------------------	
# Error handling
#--------------------------------------------------------------

	return unless defined $inpdoc;
	return unless defined $query_string;

#--------------------------------------------------------------	
# replace query atring variable with real value
#--------------------------------------------------------------

	$query_string =~ s/\$inpone/$inpone/g;
	$query_string =~ s/\$inptwo/$inptwo/g;

#--------------------------------------------------------------
# Start query
#--------------------------------------------------------------

	$resultset_nodes = $inpdoc->findnodes($query_string);

#--------------------------------------------------------------	
# iter resultsets
#--------------------------------------------------------------

	foreach my $individual_result_node ($resultset_nodes->get_nodelist) {

#--------------------------------------------------------------		
# Get string value for debugging purpose
#--------------------------------------------------------------

		$individual_result = trim($individual_result_node->string_value());

#--------------------------------------------------------------	
# Construct linked list
#--------------------------------------------------------------

		push (@found_resultset_list, $individual_result);
		
	}
	return @found_resultset_list;	
}
1;


































