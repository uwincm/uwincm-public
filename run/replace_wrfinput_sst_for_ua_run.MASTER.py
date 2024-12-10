from netCDF4 import Dataset
import sys


##############################################################################
##
## Copy the SST from the source file to the destination file.
##
## This script is used to get the coupled model SST in to the
##   wrfinput file so it will be used in the uncoupled run.
##
## Because the SST (specifically, "TSK" variable) gets replaced with
##   the data from HYCOM during initialization, we generally run the
##   coupled model and take the first wrfout file as the input SST.
##
## Dependencies:
##   - glob (this is a standard Python module)
##   - NetCDF4 module
##
## Usage:
##
## python replace_wrfinput_sst_for_ua_run.py
##   - Will use default source (first wrfout_d01 file)
##     and destination (wrfinput_d01) files.
##
## python replace_wrfinput_sst_for_ua_run.py wrfout_d01_2010-09-15_00:00:00
##   - Will use specified source file
##     and destination (wrfinput_d01) files.
##
## python replace_wrfinput_sst_for_ua_run.py wrfout_d01_2010-09-15_00:00:00 wrfinput_d01
##   - Will use specified source and destination files.
##     (May be useful if you need to do it for nested domains).
##
## !!! It is recommended to copy the MASTER file to a local file     !!!
## !!! Since any updates on the Gitlab repository will go to MASTER. !!!
##
##############################################################################

## Handle command line arg for SST source file.
## If command line arg not specified, use the first wrfout_d01 file.
if len(sys.argv) < 2:
    import glob
    filein    = sorted(glob.glob('wrfout_d01_*'))[0]
else:
    filein    = sys.argv[1]


## Handle command line arg for SST destination file.
## If the command line arg not specified, use wrfinput_d01.
if len(sys.argv) < 3:
    fileout   = 'wrfinput_d01'
else:
    fileout   = sys.argv[2]


fieldName = 'TSK'

print('Input file:    '+filein)
print('Output file:   '+fileout)
print('Field to copy: '+fieldName)

ncin  = Dataset(filein,'r',format='NETCDF3_CLASSIC')
ncout = Dataset(fileout,'r+',format='NETCDF3_CLASSIC')

ncout.variables[fieldName][:] = ncin.variables[fieldName][:]

ncin.close()
ncout.close()

print('Done.')
