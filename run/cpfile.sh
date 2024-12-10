#!/bin/bash

subregion_path=$1
for targetfile in $(ls $1/subregion*)
do
	rightpart=`echo $targetfile | rev | cut -d '/' -f1 |rev`
	rightpart2=`echo $rightpart | rev | cut -d '.' -f2 |rev`
	rightpart3=`echo $rightpart | rev | cut -d '.' -f1 |rev`
	ln -sf $targetfile "nest/archv."${rightpart2}"."${rightpart3}
done

