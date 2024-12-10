#!/bin/sh

# Usage: copy_run_directory_excluding_output.sh run run_copy
#
# Instead of copying the entire run directory including large model output files
# in order to try a small change, copy everything *except* the output files.
# This script uses rsync to accomplish this.


if [[ $# -lt 2 ]]
then
    
    echo "Usage: copy_run_directory_excluding_output.sh run run_copy"
    exit 1
    
else

    echo "Copying $1 to $2, excluding model outputs."
    rsync -a --verbose \
	  --exclude="PET*" \
    	  --exclude="wrfout_*" \
    	  --exclude="wrfrst_*" \
    	  --exclude="umwmout*" \
    	  --exclude="umwmrst*" \
	  --include="nest/*" \
	  --exclude="archv.*" \
	  --exclude="restart_out*" \
    	  --exclude="cplout_*" \
	  $1/* $2

    echo "Done."
    exit 0
    
fi
