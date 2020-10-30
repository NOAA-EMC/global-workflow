### gsi_utils.py
###          a collection of functions, classes, etc.
###          used for the GSI global analysis

def isTrue(str_in):
    """ isTrue(str_in)
    - function to translate shell variables to python logical variables

    input: str_in - string (should be like 'YES', 'TRUE', etc.)
    returns: status (logical True or False)

    """
    str_in = str_in.upper()
    if str_in in ['YES','.TRUE.']:
        status = True
    else:
        status = False
    return status

def link_file(from_file, to_file):
    """ link_file(from_file, to_file)
    - function to check if a path exists, and if not, make a symlink
    input: from_file - string path
            to_file   - string path
    """
    import os
    if not os.path.exists(to_file):
        if not os.path.islink(to_file):
            os.symlink(from_file, to_file)
        else:
            print(to_file+" exists, unlinking.")
            os.unlink(to_file)
            os.symlink(from_file, to_file)
        print("ln -s "+from_file+" "+to_file)

def copy_file(from_file, to_file):
    import shutil
    shutil.copy(from_file, to_file)
    print("cp "+from_file+" "+to_file)

def make_dir(directory):
    import os
    os.makedirs(directory)
    print("mkdir -p "+directory)

def write_nml(nml_dict, nml_file):
    """ write_nml(nml_dict, nml_file)
    - function to write out namelist dictionary nml_dict to file nml_file
    input: nml_dict - dictionary of dictionaries
                        first dictionary is &nml, second is nmlvar='value'
                        NOTE: this shoudl be an OrderedDict or else it might fail
           nml_file - string path to write namelist file to
    """
    nfile = open(nml_file, 'w')

    for nml, nmlvars in nml_dict.items():
        nfile.write('&'+nml+'\n')
        for var, val in nmlvars.items():
            nfile.write('  '+str(var)+' = '+str(val)+'\n')
        nfile.write('/\n\n')
    nfile.close()


def get_ncdims(ncfile):
    """ get_ncdims(ncfile)
    - function to return dictionary of netCDF file dimensions and their lengths
    input: ncfile - string to path to netCDF file
    output: ncdims - dictionary where key is the name of a dimension and the
                        value is the length of that dimension

                        ex:  ncdims['pfull'] = 127
    """
    try:
        import netCDF4 as nc
    except ImportError:
        print("Python Error!")
        print("netCDF4 Python module not available. Do you have the proper Python available in your environment?")
        print("Hera: module use -a /contrib/modulefiles && module load anaconda/2.3.0")
        print("Dell: module load python/3.6.3")
        print(" ")
    ncf = nc.Dataset(ncfile)
    ncdims = {}
    for d in ncf.dimensions.keys():
        ncdims[d] = int(len(ncf.dimensions[d]))
    ncf.close()

    return ncdims

def get_nemsdims(nemsfile,nemsexe):
    """ get_nemsdims(nemsfile,nemsexe)
    - function to return dictionary of NEMSIO file dimensions for use
    input:  nemsfile - string to path nemsio file
            nemsexe  - string to path nemsio_get executable
    output: nemsdims - dictionary where key is the name of a dimension and the
                       value is the length of that dimension
                       ex: nemsdims['pfull'] = 127
    """
    import subprocess
    ncdims = {
                'dimx': 'grid_xt',
                'dimy': 'grid_yt',
                'dimz': 'pfull',
                }
    nemsdims = {}
    for dim in ['dimx','dimy','dimz']:
        out = subprocess.Popen([nemsexe,nemsfile,dim],stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
        stdout, stderr = out.communicate()
        nemsdims[ncdims[dim]] = int(stdout.split(' ')[-1].rstrip())
    return nemsdims

def get_timeinfo(ncfile):
    """ get_timeinfo(ncfile)
     - function to return datetime objects of initialized time and valid time
     input: ncfile - string to path to netCDF file
     returns: inittime, validtime - datetime objects
              nfhour - integer forecast hour
    """
    try:
        import netCDF4 as nc
    except ImportError:
        print("Python Error!")
        print("netCDF4 Python module not available. Do you have the proper Python available in your environment?")
        print("Hera: module use -a /contrib/modulefiles && module load anaconda/2.3.0")
        print("Dell: module load python/3.6.3")
        print(" ")
    import datetime as dt
    import re
    ncf = nc.Dataset(ncfile)
    time_units = ncf['time'].units
    date_str = time_units.split('since ')[1]
    date_str = re.sub("[^0-9]", "", date_str)
    initstr = date_str[0:10]
    inittime = dt.datetime.strptime(initstr,"%Y%m%d%H")
    nfhour = int(ncf['time'][0])
    validtime = inittime + dt.timedelta(hours=nfhour)
    ncf.close()

    return inittime, validtime, nfhour
