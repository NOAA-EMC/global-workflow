#!/bin/bash

function modify_base {
	expdir="${1:?}"
	NOSCRUB=${NOSCRUB:?}
	echo "Modifying config.base"
	perl -i -p -e "s%(?<=^export STMP=).*\$%${PTMP}%g" "${expdir}/config.base"
	perl -i -p -e "s%(?<=^export PTMP=).*\$%${PTMP}%g" "${expdir}/config.base"
	perl -i -p -e "s%(?<=^export FHMAX_GFS_\\d\\d=\\$\{FHMAX_GFS_\\d\\d:\\-)384%36%g" "${expdir}/config.base"
}

function modify_xml {
	xml_file="${1:?}"
	echo "Modifying ${xml_file}"
	perl -i -p -e 's%<!ENTITY MAXTRIES "2">%<!ENTITY MAXTRIES "1">%g' "${xml_file}"
}

function link_IC {
	rotdir=${1:?}
	icdir=${2:?}
	echo "Copying initial conditions"
	rm -Rf "${rotdir}"
	cp -as "${icdir}/" "${rotdir}"
}

function setup_cold_96 {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py cycled --app ATM --resdet 96 --resens 48 \
		--comrot "${NOSCRUB}/global-workflow" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2021122018 \
		--edate 2021122200 \
		--nens 2 \
		--gfs_cyc 4 \
		--start cold \
		--pslot "${pslot}"
	
	link_IC "${NOSCRUB}/global-workflow/${pslot}" "${NOSCRUB}/global-workflow/C96_cold_IC/"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_cold_96_atmw {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py cycled --app ATMW --resdet 96 --resens 48 \
		--comrot "${NOSCRUB}/global-workflow" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2021122018 \
		--edate 2021122200 \
		--nens 2 \
		--gfs_cyc 4 \
		--start cold \
		--pslot "${pslot}"
	
	link_IC "${NOSCRUB}/global-workflow/${pslot}" "${NOSCRUB}/global-workflow/C96_cold_IC/"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_cold_96_00z {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir="${SAVE}/global-workflow/RUNTEST/expdir/${pslot}"
    workflowdir="${SAVE}/global-workflow/workflow"

	${workflowdir}/setup_expt.py cycled --app ATM --resdet 96 --resens 48 \
		--comrot "${NOSCRUB}/global-workflow/RUNTEST" \
		--expdir "${SAVE}/global-workflow/RUNTEST/expdir" \
		--idate 2021122018 \
		--edate 2021122200 \
		--nens 2 \
		--gfs_cyc 1 \
		--start cold \
		--pslot "${pslot}"
	
	link_IC "${NOSCRUB}/global-workflow/RUNTEST/${pslot}" "${icdir}"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	"${workflowdir}/setup_xml.py" "${expdir}"
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_cold_96_00z_new {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py cycled --app ATM --resdet 96 --resens 48 \
		--comrot "${NOSCRUB}/global-workflow" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2021122018 \
		--edate 2021122200 \
		--nens 2 \
		--gfs_cyc 1 \
		--start cold \
		--pslot "${pslot}"
	
	link_IC "${NOSCRUB}/global-workflow/${pslot}" "${NOSCRUB}/global-workflow/C96_cold_IC_new/"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"
	modify_xml "${expdir}/${pslot}.xml"
}


function setup_cold_96_da {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py cycled --app ATM --resdet 96 --resens 48 \
		--comrot "${NOSCRUB}/global-workflow" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2021122018 \
		--edate 2021122200 \
		--nens 2 \
		--gfs_cyc 0 \
		--start cold \
		--pslot "${pslot}"
	
	link_IC "${NOSCRUB}/global-workflow/${pslot}" "${NOSCRUB}/global-workflow/C96_cold_IC/"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_warm_96_da {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py cycled --app ATM --resdet 96 --resens 48 \
		--comrot "${NOSCRUB}/global-workflow" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2021122000 \
		--edate 2021122200 \
		--nens 2 \
		--gfs_cyc 0 \
		--start warm \
		--pslot "${pslot}"
	
	link_IC "${NOSCRUB}/global-workflow/${pslot}" "${NOSCRUB}/global-workflow/C96_warm_IC/"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_cold_384 {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py cycled --app ATM --resdet 384 --resens 192 \
		--comrot "${NOSCRUB}/global-workflow" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2022012106 \
		--edate 2022012300 \
		--nens 2 \
		--gfs_cyc 4 \
		--start cold \
		--pslot "${pslot}"
	
	link_IC "${NOSCRUB}/global-workflow/${pslot}" "${NOSCRUB}/global-workflow/C384_cold_IC/"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"	
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_warm_384_da {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py cycled --app ATM --resdet 384 --resens 192 \
		--comrot "${NOSCRUB}/global-workflow" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2020083000 \
		--edate 2020090100 \
		--nens 2 \
		--gfs_cyc 0 \
		--start warm \
		--pslot "${pslot}"
	
	link_IC "${NOSCRUB}/global-workflow/${pslot}" "${NOSCRUB}/global-workflow/C384_warm_IC/"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_cpl {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py forecast-only --app S2SWA --resdet 384 \
		--comrot "${NOSCRUB}/global-workflow" \
		--icsdir "${NOSCRUB}/global-workflow/FV3ICS" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2013040100 \
		--edate 2013040100 \
		--pslot "${pslot}"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"	
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_cpl_noaero {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py forecast-only --app S2SW --resdet 384 \
		--comrot "${NOSCRUB}/global-workflow" \
		--icsdir "${NOSCRUB}/global-workflow/FV3ICS" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2013040100 \
		--edate 2013040100 \
		--pslot "${pslot}"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"	
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_atma_96 {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py forecast-only --app ATMA --resdet 96 \
		--comrot "${NOSCRUB}/global-workflow" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2019060100 \
		--edate 2019060300 \
		--gfs_cyc 4 \
		--pslot "${pslot}"

	link_IC "${NOSCRUB}/global-workflow/${pslot}" "${NOSCRUB}/global-workflow/C96_FO_IC"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"
	modify_xml "${expdir}/${pslot}.xml"
}

function setup_fo_384 {
	pslot=${1:?}
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	module load intelpython 2> /dev/null
	expdir=${SAVE}/global-workflow/expdir/${pslot}
	./setup_expt.py forecast-only --app ATM --resdet 384 \
		--comrot "${NOSCRUB}/global-workflow" \
		--expdir "${SAVE}/global-workflow/expdir" \
		--idate 2022012106 \
		--edate 2022012300 \
		--gfs_cyc 4 \
		--pslot "${pslot}"

	link_IC "${NOSCRUB}/global-workflow/${pslot}" "${NOSCRUB}/global-workflow/C384_cold_IC"

	modify_base "${expdir}"

	echo "Finishing experiment setup"
	./setup_xml.py "${expdir}"
	modify_xml "${expdir}/${pslot}.xml"
}

function del_exp {
	pslot=${1:?}	
	NOSCRUB=${NOSCRUB:?}
	SAVE=${SAVE:?}
	PTMP=${PTMP:?}
	expdir="${SAVE}/global-workflow/expdir/${pslot}"

	rm -Rf "${expdir}" "${NOSCRUB}/global-workflow/${pslot}" "${PTMP}/RUNDIRS/${pslot}"
}

function check_xml_ROTDIR {
   xmlfile=${1:?}
   rotdir=$(grep ROTDIR ${xmlfile} | head -1 | sed 's/">//' | sed 's/\"//' | cut -d" " -f3)
   [[ -d ${rotdir} ]] && return 0 || return 1
}
