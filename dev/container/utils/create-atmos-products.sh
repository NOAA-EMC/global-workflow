#!/bin/bash

verbose=false

while [[ "$#" -gt 0 ]]; do
  case "$1" in
    -H|--HOMEgfs)
      HOMEgfs="$2"
      shift 2
      ;;
    -c|--container)
      container="$2"
      shift 2
      ;;
    -b|--bindings)
      bindings="$2"
      shift 2
      ;;
    -v|--verbose)
      verbose=true
      shift
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

if [[ ! -v HOMEgfs || ! -v container || ! -v bindings ]]; then
   echo "Usage: create-container-links.sh -H/--HOMEgfs gw-home-dir -c/--container container-fullpath -b/--bindings list-of-binding-dirs [-v]"
   exit 11
fi

if [[ "${verbose}" == "true" ]]; then
   echo "Verbose: ${verbose}"
   echo "HOMEgfs: ${HOMEgfs}"
   echo "container: ${container}"
   echo "bindings: ${bindings}"
fi

eap_script="${HOMEgfs}"/exec/exglobal_atmos_products.sh
cat > "${eap_script}" << EOF_ATMOS_PRODUCTS
#!/bin/bash
 LD_LIBRARY_PATH=\$(dirname ${HOMEgfs})
 export LD_LIBRARY_PATH

 singularity exec \\
        ${bindings} \\
        ${container} \\
        ${HOMEgfs}/scripts/exglobal_atmos_products.sh "\$@"
EOF_ATMOS_PRODUCTS

chmod +x "${eap_script}"

