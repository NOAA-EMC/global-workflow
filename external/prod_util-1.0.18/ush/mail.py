#!/usr/bin/python

# Author:  Kit Menlove <kit.menlove@noaa.gov>
# Purpose: If the user is in the prod group, submit an e-mail message to the production e-mail
#          queue. For other users, the message is merely printed to standard out.
# Usage:   mail.py -s subject [-c cc-addr] [-b bcc-addr] [--html] [-v] to-addr message_file
#          mail.py -s subject [-c cc-addr] [-b bcc-addr] [--html] [-v] [to-addr] < message_file
#          echo "$msg" | mail.py -s subject [-c cc-addr] [-b bcc-addr] [--html] [-v] [to-addr]
# Input:   message_file - a text file with the body of the message
#          to-addr - a comma-delimited list of recipient e-mail addresses

from __future__ import print_function
from os import getenv, getuid, path
import re, grp, pwd
import fileinput
import sqlite3
from subprocess import Popen, PIPE
from email.utils import formatdate

email_regex=r"[a-zA-Z][-+._%a-zA-Z0-9]*@[a-zA-Z0-9]+(?:[-.][a-zA-Z0-9]+){0,12}\.[a-zA-Z]{2,15}"
def validate_email_address_list(raw_address):
    address = re.sub(r"\s+", '', raw_address)
    match_result = re.match(r"{e}(?:,{e})*$".format(e=email_regex), address)
    if not match_result:
        raise ValueError('{0} does not contain a list of valid email addresses'.format(address))
    return address


if not path.exists('/gpfs/hps') and not path.exists('/etc/prod'):
    # Not on WCOSS
    def send(*args,**kwargs): pass
else:
    
    # prod jobs go to the prod database, everything else goes to the para database
    envir = getenv('envir')
    current_user=pwd.getpwuid(getuid())[0]
    current_user_address=('REDACTED' if current_user in ('nwprod', 'ecfprod') else current_user) + '@noaa.gov'
    default_recipient=(getenv('MAILTO') if getenv('MAILTO') else current_user_address)
    
    def send(subject, message_body, to_address=default_recipient, cc_address=None, bcc_address=None, from_name=None, is_html=False, verbose=False):
        # Generate the "from" address
        getsys = Popen(('getsystem.pl', '-p'), stdout=PIPE)
        current_system = getsys.communicate()[0]
        if getsys.returncode == 0:
            from_address = "{0}@{1}.wcoss.ncep.noaa.gov".format(current_user, current_system.lower())
        else:
            from_address = current_user_address
    
        # Prepend the environment to the subject if not "prod"
        if envir == 'prod' or envir == None:
            message_subject = subject.strip()
        else:
            message_subject = ("[{0}] {1}".format(envir, subject.strip()))
    
        # If certain job-information variables are set, create a string containing their values to append to the message
        job_info = []
    
        ecFlow_task_path = getenv('ECF_NAME')
        if ecFlow_task_path:
            job_info.append(("ecFlow Task", ecFlow_task_path))
    
        stdout_file = getenv('LSB_OUTPUTFILE')
        if stdout_file:
            if path.isfile(stdout_file):
                stdout_file = path.realpath(stdout_file)
            else:
                pdy = getenv('PDY')
                if pdy:
                    stdout_file_pdy = stdout_file.replace('/today/', "/{0}/".format(pdy), 1)
                    if path.isfile(stdout_file_pdy):
                        stdout_file = path.realpath(stdout_file_pdy)
            job_info.append(("Standard Output", stdout_file))
    
        if job_info:
            job_info_text = '<br /><hr /><table>' if is_html else "\n{0}\n".format('-'*80)
            for info in job_info:
                if is_html:
                    job_info_text += "<tr><td><b>{0}</b>:</td><td style=\"padding-left:3px\">{1}</td></tr>".format(*info)
                else:
                    job_info_text += info[0] + ": " + info[1] + "\n"
            if is_html:
                job_info_text += "</table>"
        else:
            job_info_text = ""
    
        # Make sure html messages are wrapped in <html></html> tags
        # TODO: Do we need to validate or encode the message body?  Make sure it isn't too long or contains characters that would mess up the query?
        if is_html:
            if re.match(r"\s*<html", message_body, re.IGNORECASE):
                message_body = re.sub(r"(?i)(</body|</html)", r"{0}\1".format(job_info_text), message_body, 1)
            else:
                message_body = '<html><body>' + message_body.strip() + job_info_text + '</body></html>'
        else:
            message_body += job_info_text
    
        message_info = {
            "timestamp": formatdate(),
            "target_address": to_address,
            "carbon_copy_address": cc_address,
            "blind_carbon_copy_address": bcc_address,
            "from_name": from_name,
            "from_address": from_address,
            "reply_to": current_user_address,
            "message_subject": message_subject,
            "message_body": message_body,
            "is_html": is_html
        }
        # Only users within the prod group are allowed to add messages to the production message queue
        if current_user in grp.getgrnam("prod").gr_mem:
            if getenv('COMROOT'):
                email_database = "{0}/logs/{1}/email.db".format(getenv('COMROOT'), 'prod' if envir == 'prod' else 'para')
                sql = sqlite3.connect(email_database)
                if not sql.execute('SELECT name FROM sqlite_master WHERE type=\'table\' AND name=\'pending\';').fetchone():
                    sql.execute('''CREATE TABLE pending (
                                       submit_time INT, to_addr TEXT, cc_addr TEXT,
                                       bcc_addr TEXT, from_name TEXT, from_addr TEXT,
                                       reply_to TEXT, subject TEXT, body TEXT, is_html INT
                                   )''')
                sql.execute('''INSERT INTO pending VALUES (
                                   :timestamp, :target_address, :carbon_copy_address,
                                   :blind_carbon_copy_address, :from_name, :from_address,
                                   :reply_to, :message_subject, :message_body, :is_html
                               )''', message_info)
                sql.commit()
                sql.close()
                if verbose:
                    print('The following message has been queued for transmission:')
            else:
                print('WARNING: The following message will NOT be sent because $COMROOT is not defined:')
                verbose=True
        else:
            print('The following message will NOT be sent due to insufficient permissions:')
            verbose=True
    
        # Print the email to stdout if verbose flag is set or the user is not in the prod group
        if verbose:
            print('-'*80)
            print("To: %(target_address)s" % message_info)
            if message_info['from_name']:
                print("From: \"%(from_name)s\" <%(from_address)s>" % message_info)
            else:
                print("From: %(from_address)s" % message_info)
            print("""Date: %(timestamp)s
    Subject: %(message_subject)s
    %(message_body)s
    --------------------------------------------------------------------------------""" % message_info)
    
    if __name__ == "__main__":
        import argparse
    
        def EmailType(raw_address):
            try:
                address_list = validate_email_address_list(raw_address)
                if not address_list:
                    raise argparse.ArgumentTypeError('One or more addresses were invalid')
                return address_list
            except ValueError:
                raise argparse.ArgumentTypeError('%s is not a valid address list' % raw_address)
    
        # Parse command line arguments
        parser = argparse.ArgumentParser(description='Submit an e-mail message to the production e-mail queue to be transmitted by the jsendmail job.  The message body may be piped via stdin or provided as an input file (i.e. message_file) following the recipient list.',
            usage='''echo "$msg" | %(prog)s -s subject [-c cc-addr] [-b bcc-addr] [--html] [-v] [to-addr]
    usage: %(prog)s -s subject [-c cc-addr] [-b bcc-addr] [--html] [-v] to-addr message_file
    usage: %(prog)s -s subject [-c cc-addr] [-b bcc-addr] [--html] [-v] [to-addr] < message_file''')
        # Subject is optional when $jobid variable is set
        if getenv('jobid'):
            parser.add_argument('-s', '--subject', default="Message from WCOSS job " + getenv('jobid'), metavar='subject', help="subject of the e-mail message")
        else:
            parser.add_argument('-s', '--subject', required=True, metavar='subject', help="subject of the e-mail message")
        parser.add_argument('-c', '--cc', type=EmailType, metavar='cc-addr', help='comma-delimited list of carbon copy recipient(s)')
        parser.add_argument('-b', '--bcc', type=EmailType, metavar='bcc-addr', help='comma-delimited list of blind carbon copy recipient(s)')
        parser.add_argument('-n', '--from', dest='from_name', metavar='name', help='name or title of the sender - will not change the underlying address')
        parser.add_argument('-v', '--verbose', action='store_true', help='print the message header and body')
        # Default "to" address to user's NOAA inbox (or REDACTED when user is nwprod or ecfprod)
        parser.add_argument('address', default=default_recipient, nargs='?', type=EmailType, metavar='to-addr',
            help='comma-delimited e-mail address(es) of the intended recipient(s); if omitted, the message will be sent ' +
            'to the recipient(s) specified in the $MAILTO variable, or the current user @noaa.gov if undefined')
        parser.add_argument('--html', action='store_true', help='send the message as HyperText Markup Language (HTML)')
        (args, message) = parser.parse_known_args()
    
        message_body = ''.join(fileinput.input(message))
        send(args.subject, message_body, args.address, args.cc, args.bcc, args.from_name, args.html, args.verbose)
    
    
