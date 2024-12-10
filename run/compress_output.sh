#!/bin/sh

## Usage: On the command line, specify what to compress, among:
## wrfout, wrfrst, cplout, umwmout, umwmrst
## or "all" to do all of the above!


if [ $# -eq 0 ]
then
    cat <<EOF
Usage: compress_output.sh list_of_netcdf_outputs

       list_of_netcdf_outputs is one of more of:
       wrfout wrfrst cplout umwmout umwmrst.
       OR specify "all" for all of the above!
EOF
    exit 1
    
else
    
    mkdir -p compressed

    ##
    ## WRF Output (wrfout)
    ##

    if [[ "$*" == "wrfout" || "$*" == "all" ]]
    then

	files=(wrfout*)
	if [ -e "${files[0]}" ]
	then
	
	    echo "Creating compressed copies of wrfout in compressed/ directory."

	    for ff in wrfout*
	    do
		echo $ff
		
		compression_atts_lines=`ncdump -h $ff | grep compression | wc -l`

		if [ $compression_atts_lines -gt 0 ]
		then
		
		    echo "File appears to already be compressed. Skipping."

		else
       
		    nccopy -d 1 -s $ff compressed/$ff
		    ncatted -a compression,global,c,c,'Original wrfout data was compressed with: nccopy -d 1 -s' compressed/$ff  # Add global attribute "compression"

		fi
	    
	    done

	    echo "Moving the compressed copies from compressed/ directory to original directory."
	    mv compressed/wrfout* .


        else
	    echo "No wrfout files."
	fi
	    
	echo Done.

    fi


    ##
    ## WRF Restart (wrfrst)
    ##
    if [[ "$*" == "wrfrst" || "$*" == "all" ]]
    then
	
	files=(wrfrst*)
	if [ -e "${files[0]}" ]
	then

	    echo "Creating compressed copies of wrfrst in compressed/ directory."

	    for ff in wrfrst*
	    do
		echo $ff

		compression_atts_lines=`ncdump -h $ff | grep compression | wc -l`
		
		if [ $compression_atts_lines -gt 0 ]
		then
		
		    echo "File appears to already be compressed. Skipping."

		else
       
		    nccopy -d 1 -s $ff compressed/$ff
		    ncatted -a compression,global,c,c,'Original wrfrst data was compressed with: nccopy -d 1 -s' compressed/$ff  # Add global attribute "compression"

		fi
	    
	    done

	    echo "Moving the compressed copies from compressed/ directory to original directory."
	    mv compressed/wrfrst* .

        else
	    echo "No wrfrst files."
	fi
	    
	echo Done.

    fi


    ##
    ## Coupler Output (cplout)
    ##

    if [[ "$*" == "cplout" || "$*" == "all" ]]
    then

	files=(cplout*)
	if [ -e "${files[0]}" ]
	then
	
	    echo "Creating compressed copies of cplout in compressed/ directory."

	    for ff in cplout*
	    do
		echo $ff
		
		compression_atts_lines=`ncdump -h $ff | grep compression | wc -l`

		if [ $compression_atts_lines -gt 0 ]
		then
		
		    echo "File appears to already be compressed. Skipping."

		else
       
		    nccopy -d 1 -s $ff compressed/$ff
		    ncatted -a compression,global,c,c,'Original cplout data was compressed with: nccopy -d 1 -s' compressed/$ff  # Add global attribute "compression"

		fi
	    
	    done

	    echo "Moving the compressed copies from compressed/ directory to original directory."
	    mv compressed/cplout* .


        else
	    echo "No cplout files."
	fi
	    
	echo Done.

    fi


    ##
    ## UMWM Output (umwmout)
    ##

    if [[ "$*" == "umwmout" || "$*" == "all" ]]
    then

	files=(output/umwmout*)
	if [ -e "${files[0]}" ]
	then

	    mkdir compressed/output
	    echo "Creating compressed copies of umwmout in compressed/output directory."

	    for ff in output/umwmout*
	    do
		echo $ff
		
		compression_atts_lines=`ncdump -h $ff | grep compression | wc -l`

		if [ $compression_atts_lines -gt 0 ]
		then
		
		    echo "File appears to already be compressed. Skipping."

		else
       
		    nccopy -d 1 -s $ff compressed/$ff
		    ncatted -a compression,global,c,c,'Original wrfout data was compressed with: nccopy -d 1 -s' compressed/$ff  # Add global attribute "compression"

		fi
	    
	    done

	    echo "Moving the compressed copies from compressed/output directory to output/ directory."
	    mv compressed/output/umwmout* ./output

	    rm -r ./compressed/output
	    
        else
	    echo "No umwmout files."
	fi
	    
	echo Done.

    fi


    ##
    ## UMWM Restart (umwmrst)
    ##

    if [[ "$*" == "umwmrst" || "$*" == "all" ]]
    then

	files=(restart/umwmrst*)
	if [ -e "${files[0]}" ]
	then

	    mkdir compressed/restart
	    echo "Creating compressed copies of umwmout in compressed/restart directory."

	    for ff in restart/umwmrst*
	    do
		echo $ff
		
		compression_atts_lines=`ncdump -h $ff | grep compression | wc -l`

		if [ $compression_atts_lines -gt 0 ]
		then
		
		    echo "File appears to already be compressed. Skipping."

		else
       
		    nccopy -d 1 -s $ff compressed/$ff
		    ncatted -a compression,global,c,c,'Original wrfout data was compressed with: nccopy -d 1 -s' compressed/$ff  # Add global attribute "compression"

		fi
	    
	    done

	    echo "Moving the compressed copies from compressed/restart directory to restart/ directory."
	    mv compressed/restart/umwmrst* ./output

	    rm -r ./compressed/restart
	    
        else
	    echo "No umwmrst files."
	fi
	    
	echo Done.

    fi

    
    ##
    ## Finish up.
    ##

    ## This directory should be empty.
    rm -r ./compressed
    
    echo All done.
    
    exit 0

fi



