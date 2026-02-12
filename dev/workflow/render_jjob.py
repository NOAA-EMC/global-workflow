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
    
    template_path = os.path.abspath(args.template)
    if not os.path.exists(template_path):
        logger.error(f"Template file not found: {template_path}")
        sys.exit(1)
        
    if not template_path.endswith(".j2"):
        logger.error(f"Template file {template_path} must end with .j2")
        sys.exit(1)

    # Prepare list of config files
    config_names = [c.strip() for c in args.configs.split(",")]
    
    # Basic environment variables often required for sourcing configs
    # These will be set in the bash environment and used for Jinja rendering of configs
    context = AttrDict({
        'PSLOT': args.pslot,
        'MACHINE': args.machine,
        'machine': args.machine.lower(),
        'HOMEgfs': os.path.abspath(os.path.join(os.path.dirname(__file__), "../..")),
        'EXPDIR': os.path.abspath(args.outdir),
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
            src_path = os.path.join(args.config_dir, name)
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
                    sys.exit(1)
            else:
                logger.error(f"Config file not found: {name} (also checked {name}.j2) in {args.config_dir}")
                sys.exit(1)

        # Initialize Configuration with the temporary directory
        cfg = Configuration(tmp_config_dir)
        
        logger.info(f"Parsing configuration sequence: {parsed_config_list}")
        config_data = AttrDict()
        
        # We process files one by one to ensure they are found and to allow for debugging
        # Note: we pass context to each call to provide bootstrap variables
        try:
            for name in parsed_config_list:
                logger.info(f"Processing config file: {name}")
                # We use a clean environment for each but pass the context
                # wxflow's parse_config typically expects a single file name and returns its variables
                data = cfg.parse_config(name, **context)
                config_data.update(data)
                # Update context with results from previous config to allow cascading references
                context.update(data)
        except Exception as e:
            logger.error(f"Failed to parse configuration: {e}")
            logger.info(f"Contents of {tmp_config_dir}: {os.listdir(tmp_config_dir)}")
            sys.exit(1)
            
        # Set the output path (removing .j2 extension)
        output_filename = os.path.basename(template_path)[:-3]
        output_path = os.path.join(args.outdir, output_filename)
        
        # Create output directory if it doesn't exist
        os.makedirs(args.outdir, exist_ok=True)
        
        logger.info(f"Rendering job template {os.path.basename(template_path)} -> {output_path}")
        try:
            # Initialize Jinja with the template and the combined configuration data
            # Use dot notation if available in config_data
            j2 = Jinja(template_path, config_data)
            
            # Save to file
            j2.save(output_path)
            
            # Make the resulting job script executable
            os.chmod(output_path, 0o755)
            logger.info("Successfully rendered and saved.")
        except Exception as e:
            logger.error(f"Failed to render template: {e}")
            sys.exit(1)

if __name__ == "__main__":
    main()
