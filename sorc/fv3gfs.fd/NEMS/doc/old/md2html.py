#! /usr/bin/env python

# Generates the README.html from README.md using the markdown2 module
# in markdown.py.  Provide no arguments; just run from the app-level
# doc directory.

import markdown2
import StringIO
import logging
import sys

# Logging object for this module:
logger=None

TOP='''<html>
<head>
  <title>NEMSLegacy Build and Test Instructions</title>
  <link rel="stylesheet" type="text/css" href="README.css">
</head>
<body>
'''

BOTTOM='''
</body>
</html>'''

# List of extra options to turn on in markdown2:
EXTRAS=['tables','code-friendly']

def write(outfile,infiles):
    # Open README.html as htmlf, in truncate mode:
    logger.info('%s: output file'%(outfile,))
    with open(outfile,'wt') as htmlf:
        # Write the heading and open the <body> tag:
        htmlf.write(TOP)

        # Loop over all input files, writing each one:
        for infile in infiles:
            logger.info('%s: input file'%(infile,))
            try:
                htmlf.write(convert(infile))
            except EnvironmentError as ee:
                logger.warning('%s: skipping file: %s'%(infile,str(ee)))

        # Close the body and html tags:
        htmlf.write(BOTTOM)


def convert(infile):
    # Open the *.md file as mdf:
    with open(infile,'rt') as mdf:
        # Read all of the *.md file into md:
        md=mdf.read()

    # convert the contents of the *.md file to HTML and return it:
    return markdown2.markdown(md,extras=EXTRAS)
    
def initlogging():
    global logger
    logger=logging.getLogger('md2html')
    oformat=logging.Formatter(
        "%(asctime)s.%(msecs)03d %(name)s (%(filename)s:%(lineno)d) "
        "%(levelname)s: %(message)s",
        "%m/%d %H:%M:%S")
    root=logging.getLogger()
    root.setLevel(logging.INFO)
    logstream=logging.StreamHandler(sys.stderr)
    logstream.setFormatter(oformat)
    logstream.setLevel(logging.INFO)
    root.addHandler(logstream)

def main(args):
    initlogging()
    if len(args)<2:
        usage()
    outfile=args[-1]   # last argument is the output file
    infiles=args[0:-1] # remaining arguments are the input files
    logger.info('Out %s in %s'%(outfile,
                                ':'.join(infiles)))
    write(outfile,infiles)

if __name__=='__main__':
    main(sys.argv[1:])
