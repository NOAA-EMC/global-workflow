#!/usr/bin/env python3
"""
Utility script to render J-job Jinja2 templates into static job scripts
using wxflow's Configuration and Jinja tools.
"""

import os
import argparse
import sys
import tempfile
import shutil
from wxflow import Configuration, Jinja, Logger, AttrDict

def render_template(template_path, config_dir, config_names, outdir, output_name=None, pslot="test_pslot", machine="WCOSS2", logger=None):
    if logger is None:
        logger = Logger(level="INFO", colored_log=True)

    template_path = os.path.abspath(template_path)
    if not os.path.exists(template_path):
        logger.error(f"Template file not found: {template_path}")
        return False

    if not template_path.endswith(".j2"):
        logger.error(f"Template file {template_path} must end with .j2")
        return False

    # Basic environment variables often required for sourcing configs
    context = AttrDict({
        'PSLOT': pslot,
        'MACHINE': machine,
        'machine': machine.lower(),
        'HOMEgfs': os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")),
        'EXPDIR': os.path.abspath(outdir),
        'COMROOT': '/tmp/com',
        'ICSDIR': '/tmp/ics',
        'ACCOUNT': 'TEST-ENT',
        'QUEUE': 'debug',
        'PARTITION_BATCH': 'compute',
        'CASE_CTL': 'C48',
        'CDATE': '2021032312',
        'PDY': '20210323',
        'cyc': '12',
        'NET': 'gfs',
        'RUN': 'gfs',
        'APP': 'ATM',
        'MODE': 'cycled',
    })

    # Create a temporary directory to hold rendered config files
    with tempfile.TemporaryDirectory() as tmp_config_dir:
        logger.info(f"Using temporary config directory: {tmp_config_dir}")

        # Prepare the config files (render if .j2)
        parsed_config_list = []
        for name in config_names:
            src_path = os.path.join(config_dir, name)
            j2_src_path = src_path + ".j2"
            dest_path = os.path.join(tmp_config_dir, name)

            if os.path.exists(src_path):
                logger.info(f"Found config: {name}")
                shutil.copy2(src_path, dest_path)
                parsed_config_list.append(name)
            elif os.path.exists(j2_src_path):
                logger.info(f"Found config template: {name}.j2, rendering...")
                try:
                    Jinja(j2_src_path, context).save(dest_path)
                    parsed_config_list.append(name)
                except Exception as e:
                    logger.error(f"Failed to render config template {name}.j2: {e}")
                    return False
            else:
                logger.error(f"Config file not found: {name} (also checked {name}.j2) in {config_dir}")
                return False

        # Initialize Configuration with the temporary directory
        cfg = Configuration(tmp_config_dir)

        logger.info(f"Parsing configuration sequence: {parsed_config_list}")
        config_data = AttrDict()

        try:
            for name in parsed_config_list:
                logger.info(f"Processing config file: {name}")
                data = cfg.parse_config(name, **context)
                config_data.update(data)
                context.update(data)
        except Exception as e:
            logger.error(f"Failed to parse configuration: {e}")
            return False

        # Set the output path
        if output_name:
            output_filename = output_name
        else:
            output_filename = os.path.basename(template_path)[:-3]

        output_path = os.path.join(outdir, output_filename)

        # Create output directory if it doesn't exist
        os.makedirs(outdir, exist_ok=True)

        logger.info(f"Rendering job template {os.path.basename(template_path)} -> {output_path}")
        try:
            j2 = Jinja(template_path, config_data)
            j2.save(output_path)
            os.chmod(output_path, 0o755)
            logger.info("Successfully rendered and saved.")
            return True
        except Exception as e:
            logger.error(f"Failed to render template: {e}")
            return False

def main():
    parser = argparse.ArgumentParser(description="Render a J-job Jinja2 template into a static job script.")
    parser.add_argument("template", help="Path to the .j2 template file")
    parser.add_argument("--config-dir", help="Directory containing config.* files", required=True)
    parser.add_argument("--configs", help="Comma-separated list of config files to source in order",
                        default="config.base,config.com")
    parser.add_argument("--outdir", help="Output directory for the rendered job script", default=".")
    parser.add_argument("--pslot", help="PSLOT name to use for rendering", default="test_pslot")
    parser.add_argument("--machine", help="Machine name (e.g., WCOSS2, HERA)", default="WCOSS2")

    args = parser.parse_args()

    # Initialize logger
    logger = Logger(level="INFO", colored_log=True)

    config_names = [c.strip() for c in args.configs.split(",")]

    success = render_template(
        args.template,
        args.config_dir,
        config_names,
        args.outdir,
        pslot=args.pslot,
        machine=args.machine,
        logger=logger
    )

    if not success:
        sys.exit(1)

if __name__ == "__main__":
    main()
