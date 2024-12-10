#!/bin/csh
cd ${main_dir_wrf_track}
set log_file_temp = ${hur_name}_${hur_num}.log  
echo ${CS_N} $CS_E
echo $wrfoutfile
#################################
#### Get the basin runmodel  ####
#################################
if ( $basin == 'wpc' ) then
   set basin_atcf = WP
else
   set basin_atcf = AL
endif
if ( $coupled_type == 'ao' ) then
   set modelname_atcf = CWRF
else
   set modelname_atcf = WRF
endif
set hur_num_atcf = `echo $hur_num | cut -c 1-2`
#################################
rm namelist.vortex
sed "s/wrfoutfile/$wrfoutfile/g;\
     s/cg_out_file1/${cg_out_file1}/g;\
     s/FGLAT/$CS_N/g;\
     s/FGLON/$CS_E/g;\
     s/omlcall/$oceanswitch/g;\
     s/w2cout/$w2cout/g;\
     s/cgout/$cgout/g;\
     s/TSTART/1/g;\
     s/basinname/$basin_atcf/g;\
     s/modelname/$modelname_atcf/g;\
     s/hurricanenumber/$hur_num_atcf/g;\
    " namelist.vortex.orig > namelist.vortex     

#### Run program, get new log file
./vortex.exe

cp vortex.log $log_file_temp
echo "date_and_time,lat,lon,max_wind_kts,min_slp_mb,rmw_nm,r34_ne,r34_se,r34sw,r34nw,r50ne,r50se,r50sw,r50nw,r64ne,r64se,r64sw,r64nw" > $log_file_temp.csv
cat vortex.log.csv >> $log_file_temp.csv
