from PyNIO import nio
from spharm import Spharmt, regrid
import numpy as np
from dateutils import dateshift, daterange
import sys, os

datapath1 = '/lfs0/tmp/whitaker/ensda/ensrf_test2'
runname1 = 'enkf'
datapath2 = '/lfs0/tmp/whitaker/ncepgfs/gsi_test'
runname2 = 'gdas1'
#datapath2 = '/lfs0/tmp/whitaker/ensda/ensrf_test'
#runname2 = 'enkf'

date1 = sys.argv[1]
date2 = sys.argv[2]
fhr = sys.argv[3]
varshort = sys.argv[4]

print datapath1
print datapath2
print 'fhr = ',fhr

def getmean(diff,coslats):
   meancoslats = coslats.mean()
   diffvar = (coslats*diff).mean()
   return  diffvar/meancoslats

def getcorr(f,a):
   f = f - f.mean(axis=0)
   a = a - a.mean(axis=0)
   covfa = (f*a).mean(axis=0)
   varf = (f**2).mean(axis=0)
   vara = (a**2).mean(axis=0)
   return covfa/(np.sqrt(varf)*np.sqrt(vara))

def getmse(f,a):
   return ((f-a)**2).mean(axis=0)

nlonsin = 800; nlatsin = 400
nlons = 360; nlats = 181
ntrunc = 20
latbound = 20.

sin = Spharmt(nlonsin,nlatsin)
sout = Spharmt(nlons,nlats)

delta = 360./nlons
lats = 90.-delta*np.arange(nlats)
coslats = np.cos((np.pi/180.)*lats)
coslats = coslats[:,np.newaxis]*np.ones((nlats,nlons))
latnh = lats.tolist().index(latbound)
latsh = lats.tolist().index(-latbound)
coslatsnh = coslats[0:latnh+1,:]
coslatssh = coslats[latsh:,:]
coslatstr = coslats[latnh:latsh+1,:]
lons = delta*np.arange(nlons)
lons, lats = np.meshgrid(lons,lats[::-1])

filename = varshort
if varshort == 'hgt':
    varname = 'HGT_3_ISBL_10'
    ecvarname = 'gh_P0_L100_GGA0'
elif varshort == 'mslp':
    varname = 'PRMSL_3_MSL_10'
    ecvarname = 'msl_P0_L101_GGA0'
elif varshort == 'temp':
    varname = 'TMP_3_ISBL_10'
    ecvarname = 't_P0_L100_GGA0'
elif varshort == 'uwnd':
    varname = 'U_GRD_3_ISBL_10'
    ecvarname = 'u_P0_L100_GGA0'
    filename = 'uv'
elif varshort == 'vwnd':
    varname = 'V_GRD_3_ISBL_10'
    ecvarname = 'v_P0_L100_GGA0'
    filename = 'uv'
elif varshort in ['chi','psi']:
    filename = 'uv'
    varnamev = 'V_GRD_3_ISBL_10'
    varnameu = 'U_GRD_3_ISBL_10'
    ecvarnamev = 'v_P0_L100_GGA0'
    ecvarnameu = 'u_P0_L100_GGA0'
else:
    print 'unknown variable',varshort
    raise SystemExit

if filename in ['mslp']:
    levels = [None]
elif filename == 'hgt':
    levels = [1000,925,850,700,500,300,250,200,50]
else:
    levels = [1000,925,850,700,500,300,250,200]

for level in levels:
    ecfilename = filename+'.grib'
    f = nio.open_file('/p72/reanl/whitaker/'+ecfilename)
    lonsin = f.variables['lon_0'][:]
    latsin = f.variables['lat_0'][:]
    datesin = f.variables['initial_time0'][:]
    dates = []
    for date in datesin:
        d = date.split('/')
        mm = d[0]; dd = d[1]; yyyy = d[2][0:4]
        hh = (d[2].split('(')[1]).split(':')[0]
        dates.append(yyyy+mm+dd+hh)
    if varshort not in ['psi','chi']:
        varin = f.variables[ecvarname]
    else:
        varinu = f.variables[ecvarnameu]
        varinv = f.variables[ecvarnamev]
    datefcsts = daterange(date1,date2,12)
    fcsts_2 = np.zeros((len(datefcsts),nlats,nlons),np.float32)
    fcsts_1 = np.zeros((len(datefcsts),nlats,nlons),np.float32)
    anals = np.zeros((len(datefcsts),nlats,nlons),np.float32)
    if levels[0] is not None:
        eclevels = (0.01*f.variables['lv_ISBL0'][:]).tolist()
        nlevec = eclevels.index(level)
    for ntime,date in enumerate(datefcsts):
        dateverif = dateshift(date,int(fhr))
        nt = dates.index(dateverif)
        if varshort in ['chi','psi']:
            if fhr=='0':
                grbfile_1 =\
                os.path.join(os.path.join(datapath1,date),runname1+'.t'+date[8:10]+'z.pgrbanl')
                grbfile_2 =\
                os.path.join(os.path.join(datapath2,date),runname2+'.t'+date[8:10]+'z.pgrbanl')
            else:
                grbfile_1 =\
                os.path.join(os.path.join(datapath1,date),runname1+'.t'+date[8:10]+'z.pgrbf'+fhr)
                grbfile_2 =\
                os.path.join(os.path.join(datapath2,date),runname2+'.t'+date[8:10]+'z.pgrbf'+fhr)
            f2 = nio.open_file(grbfile_2+'.grib')
            levels = f2.variables['lv_ISBL3'][:].tolist()
            nlev = levels.index(level)
            dat2u = f2.variables[varnameu][nlev]
            dat2v = f2.variables[varnamev][nlev]
            f2.close()
            f1 = nio.open_file(grbfile_1+'.grib')
            levels = f1.variables['lv_ISBL3'][:].tolist()
            nlev = levels.index(level)
            dat1u = f1.variables[varnameu][nlev]
            dat1v = f1.variables[varnamev][nlev]
            f1.close()
            datainu = varinu[nt,nlevec,:,:]
            datainv = varinv[nt,nlevec,:,:]
            datverifu = regrid(sin,sout,datainu,ntrunc=ntrunc)
            datverifv = regrid(sin,sout,datainv,ntrunc=ntrunc)
            if varshort == 'psi':
                datverif,chi =\
                sout.getpsichi(datverifu,datverifv,ntrunc=ntrunc)
                dat2,chi = sout.getpsichi(dat2u,dat2v,ntrunc=ntrunc)
                dat1,chi = sout.getpsichi(dat1u,dat1v,ntrunc=ntrunc)
            else:
                psi,datverif = sout.getpsichi(datverifu,datverifv,ntrunc=ntrunc)
                psi,dat2 = sout.getpsichi(dat2u,dat2v,ntrunc=ntrunc)
                psi,dat1 = sout.getpsichi(dat1u,dat1v,ntrunc=ntrunc)
        else:
            if fhr=='0':
                grbfile_1 =\
                os.path.join(os.path.join(datapath1,date),runname1+'.t'+date[8:10]+'z.pgrbanl')
                grbfile_2 =\
                os.path.join(os.path.join(datapath2,date),runname2+'.t'+date[8:10]+'z.pgrbanl')
            else:
                grbfile_1 =\
                os.path.join(os.path.join(datapath1,date),runname1+'.t'+date[8:10]+'z.pgrbf'+fhr)
                grbfile_2 =\
                os.path.join(os.path.join(datapath2,date),runname2+'.t'+date[8:10]+'z.pgrbf'+fhr)
            f2 = nio.open_file(grbfile_2+'.grib')
            levels = f2.variables['lv_ISBL3'][:].tolist()
            if level is not None:
                nlev = levels.index(level)
                dat2 = f2.variables[varname][nlev]
            else:
                dat2 = f2.variables[varname][:]
            if ntrunc is not None:
                dat2spec = sout.grdtospec(dat2,ntrunc=ntrunc)
                dat2 = sout.spectogrd(dat2spec)
            f2.close()
            f1 = nio.open_file(grbfile_1+'.grib')
            if level is not None:
                levels = f1.variables['lv_ISBL3'][:].tolist()
                nlev = levels.index(level)
                dat1 = f1.variables[varname][nlev]
            else:
                dat1 = f1.variables[varname][:]
            if ntrunc is not None:
                dat1spec = sout.grdtospec(dat1,ntrunc=ntrunc)
                dat1 = sout.spectogrd(dat1spec)
            f1.close()
            if level is not None:
                datain = varin[nt,nlevec,:,:]
            else:
                datain = varin[nt,:,:]
            datverif = regrid(sin,sout,datain,ntrunc=ntrunc)
        fcsts_2[ntime] = dat2
        fcsts_1[ntime] = dat1
        anals[ntime] = datverif
    corr_2 = getcorr(fcsts_2,anals)
    corr_1 = getcorr(fcsts_1,anals)
    corrnh = corr_2[0:latnh+1,:]
    corrsh = corr_2[latsh:,:]
    corrtr = corr_2[latnh:latsh+1,:]
    corrg_2 =  getmean(corr_2,coslats)
    corrnh_2 = getmean(corrnh,coslatsnh)
    corrsh_2 = getmean(corrsh,coslatssh)
    corrtr_2 = getmean(corrtr,coslatstr)
    corrnh = corr_1[0:latnh+1,:]
    corrsh = corr_1[latsh:,:]
    corrtr = corr_1[latnh:latsh+1,:]
    corrg_1 =  getmean(corr_1,coslats)
    corrnh_1 = getmean(corrnh,coslatsnh)
    corrsh_1 = getmean(corrsh,coslatssh)
    corrtr_1 = getmean(corrtr,coslatstr)
    mse_2 = getmse(fcsts_2,anals)
    mse_1 = getmse(fcsts_1,anals)
    if varshort in ['psi','chi']:
        mse_2 = mse_2/1.e10
        mse_1 = mse_1/1.e10
    msenh = mse_2[0:latnh+1,:]
    msesh = mse_2[latsh:,:]
    msetr = mse_2[latnh:latsh+1,:]
    mseg_2 =  np.sqrt(getmean(mse_2,coslats))
    msenh_2 = np.sqrt(getmean(msenh,coslatsnh))
    msesh_2 = np.sqrt(getmean(msesh,coslatssh))
    msetr_2 = np.sqrt(getmean(msetr,coslatstr))
    msenh = mse_1[0:latnh+1,:]
    msesh = mse_1[latsh:,:]
    msetr = mse_1[latnh:latsh+1,:]
    mseg_1 =  np.sqrt(getmean(mse_1,coslats))
    msenh_1 = np.sqrt(getmean(msenh,coslatsnh))
    msesh_1 = np.sqrt(getmean(msesh,coslatssh))
    msetr_1 = np.sqrt(getmean(msetr,coslatstr))
    print '%s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f' %\
         (level,corrnh_1,corrnh_2,corrtr_1,corrtr_2,\
         corrsh_1,corrsh_2,corrg_1,corrg_2)
    print '%s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f' %\
         (level,msenh_1,msenh_2,msetr_1,msetr_2,\
         msesh_1,msesh_2,mseg_1,mseg_2)
