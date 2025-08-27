#!/usr/bin/env bash

# smile_rocotostat.sh
# Run rocotostat.py for all pslot directories in EXPDIR
# Usage: ./smile_rocotostat.sh /path/to/EXPDIR [optional switches]

set -euo pipefail

EXPDIR="${1:-}"
EXTRA_SWITCHES="${2:-}"

if [[ -z "${EXPDIR}" ]]; then
  echo "Usage: $0 /path/to/EXPDIR [optional switches]"
  exit 1
fi

if [[ ! -d "${EXPDIR}" ]]; then
  echo "ERROR: EXPDIR '${EXPDIR}' does not exist."
  exit 2
fi

for pslot_dir in "${EXPDIR}"/*; do
  pslot="$(basename "${pslot_dir}")"
  xml_file="${pslot_dir}/${pslot}.xml"
  db_file="${pslot_dir}/${pslot}.db"

  if [[ -f "${xml_file}" && -f "${db_file}" ]]; then
    echo "Running rocotostat.py for ${pslot}..."
    python3 "$(dirname "$0")/rocotostat.py" -w "${xml_file}" -d "${db_file}" ${EXTRA_SWITCHES}
  else
    echo "Skipping ${pslot}: missing ${pslot}.xml or ${pslot}.db"
  fi

done
