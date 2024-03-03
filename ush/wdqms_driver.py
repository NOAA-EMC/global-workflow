import argparse
import logging
from wdqms import WDQMS


if __name__ == "__main__":

    # Parse command line
    ap = argparse.ArgumentParser()
    ap.add_argument("-i", "--input_list", nargs='+', default=[],
                    help="List of input GSI diagnostic files")
    ap.add_argument("-t", "--type",
                    help="WDQMS file type (SYNOP, TEMP, MARINE)")
    ap.add_argument("-o", "--outdir",
                    help="Out directory where files will be saved")
    ap.add_argument('-d', '--debug',
                    help="Print debugging statements to log file",
                    action="store_const", dest="loglevel",
                    const=logging.DEBUG,
                    default=logging.WARNING)
    ap.add_argument('-v', '--verbose',
                    help="Print information statements about code",
                    action="store_const", dest="loglevel",
                    const=logging.INFO)

    args = ap.parse_args()


    if args.type not in ['SYNOP', 'TEMP', 'MARINE']:
        raise ValueError(f'{args.type} not a correct input. Inputs include: ' \
                         'SYNOP, TEMP, MARINE')

    WDQMS(args.input_list, args.type, args.outdir, args.loglevel)
