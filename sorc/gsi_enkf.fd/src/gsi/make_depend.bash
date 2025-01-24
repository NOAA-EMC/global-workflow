#!/usr/bin/env bash
#$$$  subprogram documentation block
#                .      .    .                                       .
# subprogram:	make_depend.bash
#   prgmmr:	jing guo <jguo@nasa.gov>
#      org:	NASA/GSFC, Global Modeling and Assimilation Office, 610.3
#     date:	2015-06-05
#
# abstract:	This script generates a list of dependences supporting make().
#
# program history log:
#   2015-06-05  j guo   - original design and coding,
#			  and this document block
#
# attributes:
#   language: bash
#   machine:  Linux
#
# synopsis:
#		bash make_depend.bash *.[Ff] *.[Ff]90
#		
# descriptions:
#		This script generates dependence lists in a compact form of
#
#		    x.o : x.f90 y.inc z.o
#
#		without explicit referencing to *.mod, except where a *.mod
#		is considered external.
#
#		Simple Fortran code style is assumed, where USE, INCLUDE	
#		(as well as "#include"), and MODULE statements are searched
#		to find one and only one module name or file name.  Also, no
#		MODULE definition is expected from include-files.
#
#		*.mod entries in USE are resolved to corresponding "*.o" files
#		based on looking-up a table gathering MODULE declarations.
#		This table has the form of
#
#		    a.mod : z.o
#		    b.mod : z.o
#
#		This script can be configured to dynamically generate dependency
#		files (*.d) at the make() time.  However, this version has been
#		configured to generate dependencies statically, to avoid some
#		complicities involved and to support traditional non-GNU make().
#
#$$$  end subprogram documentation bsh make_delock

# These two set commands are used to ensure this script can fail fast on exceptions.
set -e 
set -o pipefail

myname=$(basename $0)
MYname="# $(basename $myname .bash)"

mfile=allmod.list
mfile=.--allmod.list--

warning=""
warning="yes"

#ignore_not_found_mod="yes"
#ignore_not_found_inc="yes"
ignore_dotobj_lookup_external="yes"

multi_lines="yes"
multi_lines=""

if [ $# -eq 0 ]; then
  echo "Usage: $myname <*.[Ff]> <*.[Ff]90>" 1>&2
  exit 1
fi

warn_(){
  local m
  if [[ -n $warning ]]
  then
    m=$1; shift 1
    echo "$MYname:$m(): >>> WARNING <<<" "$@" 1>&2
  fi
}

perr_(){
  local m
  m=$1; shift 1
  echo "$MYname:$m(): >>> ERROR <<<" "$@" 1>&2
}

basepath_(){
  echo "$1" | sed -e's:[.][cFf][^.]*$::'
}

inc_grep_(){
  sed -f $incscr $1 | env LC_ALL=C sort -u
}

inc_screen_(){
# screen out include file names being black-listed
  local n r
  local -a v
  while read r
  do
    v=($r); n=${#v[*]}
    if [[ $n -ne 1 ]]
    then
      warn_ inc_screen_ "suspicious text from \"$1\" ignored, (INCLUDE?) \"$r\""
    else
      case $r in
	$1) ;;
        mpif.h|netcdf.inc) ;;
        *) echo $r ;;
      esac
    fi
  done
}

dotinc_recurs_grep_(){
  local i s

  s=$1
  if [[ ! -r $s ]]
  then
    perr_ "dotinc_recurs_grep_" "can not access, \"$s\""
    exit 2
  fi

  for i in $(inc_grep_ $s | inc_screen_ $s )
  do
    if [[ -r $i || -z $ignore_not_found_inc ]]
    then
      echo $i
    fi

    if [[ -r $i ]]; then
      dotinc_recurs_grep_ $i
    fi
  done
}

use_grep_(){
  #sed -f $usescr $1 1>&2
  sed -f $usescr $1 | tr '[:upper:]' '[:lower:]' | env LC_ALL=C sort -u
}

dotmod_screen_(){
# screen out dotmod names being black-listed
  local n r
  local -a v
  while read r
  do
    v=($r); n=${#v[*]}
    if [[ $n -ne 1 ]]
    then
      r=$(echo $r | sed -e's/[.]mod$//')
      warn_ "dotmod_screen_" "suspicious text from \"$1\" ignored, (USE?) \"$r\""
    else
      case $r in
	mpi.mod|netcdf.mod) ;;
        omp_lib.mod) ;;
        *) echo $r ;;
      esac
    fi
  done
}

dotmod_use_grep_(){
# grep .mod names from uses in a given list of files
  local s
  for s in "$@"
  do
    if [[ ! -r $s ]]
    then
      warn_ "dotmod_use_grep_" "can not read thus ignored, \"$s\""
      continue
    fi
    use_grep_ $s | dotmod_screen_ $1
  done
}

src_grep_(){
  local n o
  local -a v

  n=0
  o=$1
  v=($1)

  if [[ -r $mfile ]]
  then
    o=$(grep -i "^$1 : " $mfile | awk '{print $3}')
    v=($o)
    n=${#v[*]}
    if [[ $n -eq 0 ]]
    then
      o=$1
    fi
    v=($o)
  fi

  if [[ $n -gt 1 ]]
  then
    perr_ "src_grep_" "multiple \".o\" prerequisites are present for the same \".mod\" target, \"$1 : ${v[@]}\""
    exit 2
  elif [[ $n -eq 0 ]]
  then
    if [[ -z $ignore_dotobj_lookup_external ]]; then
      warn_ "src_grep_" "not locally accessible, \"$1\""
    fi
  fi

  if [[ $n -gt 0 || -z $ignore_not_found_mod ]]
  then
    echo "$o"
  fi
}

dotobj_mod_grep_(){
  local m
  for m in "$@"
  do
    src_grep_ $m 
  done
}

filter_out_(){
  local e
  while read e
  do
    case $e in
    $1) ;;
     *) echo "$e" ;;
    esac
  done
}

modhash_make_(){
# create a single entry list
local f o m
local m
for f in "$@"; do
  if [ -f "$f" ]; then

#   Instead of using basename, sed is used to preserve the full path if in use

    #o=$(echo "$f" | sed -e "s:\.[fF]$::" -e "s:\.[fF]90$::").o
    o=$(basepath_ "$f").o

    for m in $(sed -f $modscr $f | env LC_ALL=C sort -u | tr '[:upper:]' '[:lower:]')
    do
      echo $m.mod : $o
    done

  fi
done | env LC_ALL=C sort -u
# "sort -u" at the end is to prevent duplications in the given source file list.
}

check_single_(){
  local n l
  local -a v
  while read l
  do
    v=($l); n=${#v[*]}
    if [[ $n -ne 1 ]]
    then
      warn_ "check_single_" "invalid name from \"$1\", token = \"$l\""
    else
      echo "$l"
    fi
  done
}

dotobj_only_(){
  local o
  for o in "$@"
  do
    case $o in
    *.o) echo "$o" ;;
    esac
  done
}

dotobj_unknown_(){
  local m
  for m in "$@"
  do
    case $m in
    *.o) ;;
      *) echo "$m" ;;
    esac
  done
}

dotinc_known_(){
  local i
  for i in "$@"
  do
    if [[ -r $i ]]
    then
      echo "$i"
    fi
  done
}

dotinc_unknown_(){
  local i
  for i in "$@"
  do
    if [[ ! -r $i ]]
    then
      echo "$i"
    fi
  done
}

fdep_make_(){
local F O
local ilist olist mlist
local imiss omiss

  F="$1"
  if [[ ! -r $F ]]
  then
    perr_ "fdep_make_" "can not read, \"$F\""
    exit 2
  fi

  O=$(basepath_ "$F").o
  #echo "O=$O"
  #exit 0

  ilist=$(dotinc_recurs_grep_ "$F"        | env LC_ALL=C sort -u)
  imiss=$(dotinc_unknown_ $ilist)
  ilist=$(dotinc_known_   $ilist)

  mlist=$(dotmod_use_grep_    "$F" $ilist | env LC_ALL=C sort -u)
  olist=$(dotobj_mod_grep_         $mlist | env LC_ALL=C sort -u | filter_out_ "$O") # || \

  omiss=$(dotobj_unknown_ $olist)
  olist=$(dotobj_only_    $olist)

  if [[ -z $multi_lines ]]
  then
    echo $O : $F $ilist $olist
    if [[ -n $imiss || -n $omiss ]]
    then
      echo "#--" $O : $imiss $omiss
    fi

  else
    echo $O : $F
    if [[ -n $ilist ]]; then echo $O : $ilist; fi
    if [[ -n $olist ]]; then echo $O : $olist; fi

    if [[ -n $imiss || -n $omiss ]]
    then
      if [[ -n $imiss ]]; then
        warn_ "fdep_make_" "locally unknown pre-requisites, \"$O : $imiss\""
        echo $O : $imiss
      fi
      if [[ -n $omiss ]]; then
        warn_ "fdep_make_" "locally unknown pre-requisites, \"$O : $omiss\""
      	echo $O : $omiss
      fi
    fi
  fi
#
# Alternatively, for dynamically generated %.d file creations,
#
# D=$(basename $O .o).d
# ilist=$(dotinc_recurs_grep_ "$F"        | env LC_ALL=C sort -u)
# ulist=$(dotmod_use_grep_    "$F" $ilist | env LC_ALL=C sort -u)
# mlist=$(modhash_make_ $F)
#
# echo "$D : $F $ilist"
# echo "$mlist : $O"
# echo "$O : $F"
# echo "$O : $ulist"
# echo "$O : $ilist"
}

incscr="${TMPDIR:-/tmp}/$$inc"
usescr="${TMPDIR:-/tmp}/$$use"
modscr="${TMPDIR:-/tmp}/$$mod"
m_hash="${TMPDIR:-/tmp}/$$m_hash"

clean_up_on_exit_(){
  rm -f $incscr
  rm -f $usescr
  rm -f $modscr
  rm -f $m_hash
}

trap -- "clean_up_on_exit_" EXIT

# Make sed script
#=================
cat <<'EOT' >$incscr
#n

/^\#include[ 	][ 	]*/{
s/^[^"<]*["<]//
s/[">].*$//
s/^\#include[ 	][ 	]*//
p
b
}

/^[ 	]*[Ii][Nn][Cc][Ll][Uu][Dd][Ee][ 	][ 	]*/{
s/^[ 	]*[Ii][Nn][Cc][Ll][Uu][Dd][Ee][ 	][ 	]*//
s/\!.*$//
s/['"]//g
s/[ 	]*$//
p
b
}
EOT

cat <<'EOT' >$usescr
#n
/^[ 	]*[Uu][Ss][Ee][ 	][ 	]*/{
s/^[ 	]*[Uu][Ss][Ee][ 	][ 	]*//
s/[ 	]*\!.*$//
s/[ 	]*,[	 ]*[Oo][Nn][Ll][Yy].*$//
s/$/.mod/
p
b
}
EOT

cat <<'EOT' >$modscr
#n

/^[ 	]*[Mm][Oo][Dd][Uu][Ll][Ee][ 	][ 	]*[Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee][ 	][ 	]*/{
b
}

/^[ 	]*[Mm][Oo][Dd][Uu][Ll][Ee][ 	][ 	]*/{
s/^[ 	]*[Mm][Oo][Dd][Uu][Ll][Ee][ 	][ 	]*//
s/\!.*$//
s/[ 	]*$//
p
}

EOT

#-- For testing ...
#modhash_make_ "$@"
#exit 0

if [[ ! -r $mfile ]]
then
  mfile=$m_hash
  echo "$MYname: >>> ACTION <<< creating $mfile" 1>&2
  modhash_make_ "$@" >$mfile
  echo "$MYname: >>> ACTION <<< created, $mfile" 1>&2
fi

for F in "$@"
do
  fdep_make_ "$F"
done

