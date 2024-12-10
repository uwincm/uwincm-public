#!/bin/csh
cd ${main_dir_wrf_track}
set log_file_temp = ${hur_name}_${hur_num}.log  
set log_file = vortex.log
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
##############################################################
#### Get the maximum time step processed so far, lon, lat ####
##############################################################
set lat_start1 = `awk -F"," 'NR > 0 { printf("%g\n",$7/10) }' $log_file_temp | tail -1 `
set lon_start1 = `awk -F"," 'NR > 0 { printf("%g\n",$8/10) }' $log_file_temp | tail -1 `
if ( $basin == 'atl' ) then
   set lon_start1 = -$lon_start1
endif
echo $basin $lon_start1
alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'
echo $lon_start1 $lat_start1 
echo $wrfoutfile
set nl = `wc -l $log_file_temp | awk '{print $1}'`
while  ( ( $lon_start1 == 990 || $lat_start1 == 990 ) && $nl > 3 )
       @ nl = $nl - 1  
       set lat_start1 = `awk -F"," 'NR == $nl { printf("%g\n",$7/10)  }' $log_file_temp | tail -1 `
       set lon_start1 = `awk -F"," 'NR == $nl { printf("%g\n",$8/10)  }' $log_file_temp | tail -1 `
       if ( $basin == 'atl' ) then
          set lon_start1 = -$lon_start1
       endif
end
##############################################################
rm namelist.vortex
sed "s/wrfoutfile/$wrfoutfile/g;\
     s/cg_out_file1/${cg_out_file1}/g;\
     s/FGLAT/$lat_start1/g;\
     s/FGLON/$lon_start1/g;\
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

awk 'NR > 0 { print $0 }' vortex.log >> $log_file_temp
awk 'NR > 0 { print $0 }' vortex.log.csv >> $log_file_temp.csv
