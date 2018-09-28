. ${NWROOT:?}/versions/${model:?}.ver
eval export HOME${model}=${NWROOT}/${model}.\${${model}_ver:?}
. ${HOMEgfs}/ecflow/ecf/versions/gfs.ver
