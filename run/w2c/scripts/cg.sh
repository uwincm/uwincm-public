#!/bin/sh
cd ${main_dir_wrf_track}
namelist_new=namelist.vortex
log_file=vortex.log
cp namelist.cg.orig $namelist_new
replace cg_in1 $cg_1 --  $namelist_new    
replace cg_in2 $cg_2 --  $namelist_new    
replace cg_in3 $cg_3 --  $namelist_new    
replace exist_1 $exist_file1 --  $namelist_new
replace exist_2 $exist_file2 --  $namelist_new
replace exist_3 $exist_file3 --  $namelist_new
if [ $w2c == y ]; then
replace cgout 'true' --  $namelist_new
else
replace cgout 'false' --  $namelist_new
fi
cg.exe
