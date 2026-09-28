#!/bin/bash
# Refresh the ecf_scripts directory from the repo source .ecf files.
#
# Reads ecf_scripts.manifest and per-task #SBATCH headers (written by
# the .def generator), then reassembles each .ecf in the ECF_FILES
# directory: shebang + sbatch_header + source body (minus shebang).
#
# Run this after editing an .ecf template in the repo to pick up
# changes without regenerating the full .def.
#
# Usage:
#   sync_ecf_scripts.sh <ecf_scripts_dir>
#
# The manifest at <ecf_scripts_dir>/ecf_scripts.manifest contains:
#   - Header line: # ECF_SRC_DIR=<path to repo ecflow/scripts>
#   - One line per file: <category/dest_name>\t<category/source_name>
#
# Per-task #SBATCH headers live in <ecf_scripts_dir>/sbatch_headers/<task>.hdr.
#
# Example:
#   sync_ecf_scripts.sh /scratch3/.../EXPDIR/my_C48_test/ecf_scripts

set -eu

if [[ $# -ne 1 ]]; then
    echo "Usage: ${0##*/} <ecf_scripts_dir>" >&2
    exit 1
fi

ecf_dir="$1"
manifest="${ecf_dir}/ecf_scripts.manifest"
scripts_dir="${ecf_dir}/scripts"
headers_dir="${ecf_dir}/sbatch_headers"

if [[ ! -f "${manifest}" ]]; then
    echo "[ERROR] Manifest not found: ${manifest}" >&2
    echo "  Run load_ecflow_case.py first to generate it." >&2
    exit 1
fi

src_dir=$(grep '^# ECF_SRC_DIR=' "${manifest}" | head -1 | cut -d= -f2-)
if [[ -z "${src_dir}" ]]; then
    echo "[ERROR] ECF_SRC_DIR not found in manifest header." >&2
    exit 1
fi

if [[ ! -d "${src_dir}" ]]; then
    echo "[ERROR] Source directory does not exist: ${src_dir}" >&2
    exit 1
fi

count=0
while IFS=$'\t' read -r dest_path source_path; do
    [[ "${dest_path}" =~ ^#.*$ || -z "${dest_path}" ]] && continue

    src="${src_dir}/${source_path}.ecf"
    dest="${scripts_dir}/${dest_path}.ecf"

    if [[ ! -f "${src}" ]]; then
        echo "[WARN] Source not found, skipping: ${src}" >&2
        continue
    fi

    task_name="${dest_path##*/}"
    hdr="${headers_dir}/${task_name}.hdr"

    mkdir -p "$(dirname "${dest}")"

    if [[ -f "${hdr}" ]]; then
        {
            echo '#!/bin/bash'
            cat "${hdr}"
            if head -1 "${src}" | grep -q '^#!/bin/bash'; then
                tail -n +2 "${src}"
            else
                cat "${src}"
            fi
        } > "${dest}"
    else
        cp "${src}" "${dest}"
    fi

    count=$((count + 1))
done < "${manifest}"

echo "[OK] Synced ${count} .ecf files to ${scripts_dir}"
