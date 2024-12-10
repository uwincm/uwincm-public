#!/bin/bash

##
## Link or copy the archv.*.[ab] files to the nest/ directory.
## +++ To copy instead of linking, specify the -c flag +++
##
## The script will print out the linking or copying commands it uses.
##
## Specify the path and prefix for the archv files to link to the nest/ directory.
## The prefix is the path that will result in the files being listed using "ls",
## most likely, everything up to the "archv" portion of the file names.
## The files you are referencing should end like "2011_238_00.[ab]".
##
## Examples:
## 1. For subregion files: cpfile.sh hycom-domain-dir/subregion/subregion/subregion
## 2. For remaph files: cpfile.sh hycom-domain-dir/remaph/remaph/remaph41_subregion
## 3. For template/tutorial: cpfile.sh hycom-domain-template-dir/hycom.20110825.gofs3p1.reanalysis/12/subregion_fix_remaph41_archv
##

if [[ $# < 1 ]]
then
    cat <<EOF
usage: cpfile.sh [-c] prefix_to_archv_files
- Will use the files prefix_to_archv_files*.[ab].
- Specify -c to copy instead of ln -sf.
EOF
    exit 1
fi


CMD="ln -sf"
while getopts c flag
do
    case "${flag}" in
        c) CMD='cp'
        shift 1
    esac
done

mkdir -p ./nest

subregion_path=$1
for targetfile in $(ls ${1}*.[ab])
do
	rightpart=`echo $targetfile | rev | cut -d '/' -f1 |rev`
	rightpart2=`echo $rightpart | rev | cut -d '.' -f2 |rev`
	rightpart3=`echo $rightpart | rev | cut -d '.' -f1 |rev`
	echo  $CMD $targetfile "nest/archv."${rightpart2}"."${rightpart3}
	$CMD $targetfile "nest/archv."${rightpart2}"."${rightpart3}
done

echo 'Done.'

exit 0
