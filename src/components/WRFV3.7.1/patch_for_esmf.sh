#!/bin/bash
#
# Apply this patch for using WRF within ESMF framework;
# This is only for case when WRF is compiled with ESMF option OFF!!
#
# Renames all ESMF_* objects in external/esmf_time_f90/ into something else, 
# e.g. timelib_*. This avoids name conflicts when linking WRF to an actual 
# ESMF library. This should be fixed one of future releases of WRF - there is 
# absolutely no reason for these "borrowed" ESMF routines to be called ESMF.
#
# Above action  creates need for adding two routines to frame/module_domain.F
#
# mcurcic 
# 2011/12/26
#
#cp -rv external/esmf_time_f90 external/esmf_time_f90.orig
cd external/esmf_time_f90

for file in *.F90 *.inc; do
  echo patch_for_esmf.sh: Patching $file
  sed -i 's/esmf/timelib/gi' $file
  sed -i 's/esmc/timelibc/gi' $file
done

for file in *.inc; do
  echo patch_for_esmf.sh: Patching $file
  mv $file timelib${file#'ESMF'}
done

cd -
echo patch_for_esmf.sh: Done
exit
