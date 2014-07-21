#!/bin/bash

rc=$1
resource=$2
PWD=`pwd`
path=/usr/local/Cluster-Apps/slurm/bin

function dna_start_rc_resource {
	
#	rc=$1
#	resource=$2
#	
#	- Create config file [We have 1 parameter, others can be added as per requirement]-
#		- Partition : 
#	- SLURM_ JOB_NODELIST = `sbatch $resource`
#	- dna_check $SLURM_ JOB_NODELIST
#	- Appending open port to each node in $SLURM_ JOB_NODELIST and create CAD
#	- Check the MD5 hash of the executable on the shared path - md5sum -c --status
#	- srun -p $partition --nodelist=$CAD[IP] --exclusive $rc controller $CAD[IP-2] $CAD[Port] 
#	- for CAD in `cat $CAD`
#	  do
#       srun -p $partition --nodelist=$CAD --exclusive $rc slave $CAD[IP-3..N] $CAD[Port] 
#	 done
#	- srun -p $partition --nodelist=$CAD[IP] --exclusive $rc master $CAD[IP-1] $CAD[Port] 


#Read partion from config.cf
	partition=`cat $PWD/config.cf | grep -w "Partition" | awk '{print $3}'`
	submitJobToSLURM $resource
	if [ $? -eq 0 ]; then
		echo -e "Job submission completed!!"
	else
		echo -e "Error : Some issue occurs while submitting!!"
	fi



}

function submitJobToSLURM {

	getResource=`echo $1`
        echo -e "Job submission to SLURM initiated!!"
        getJobID=`$path/sbatch $PWD/$getResource $PWD/$rc | awk '{print $4}'`
        while true;do
                sleep 2

                # Check job status
                STATUS=`squeue -j $getJobID -t PD,R -h -o %t`

                if [ "$STATUS" = "R" ];then
                        # Job is running, break the while loop
                        break
                elif [ "$STATUS" != "PD" ];then
                        echo "Job is not Running or Pending. Aborting"
                        scancel $getJobID
                        exit 1
                fi

                echo -n "."

        done

        while true;do
                STATUS=`squeue -j $getJobID -t PD,R -h -o %t`
                if [ "$STATUS" = "" ]; then
                        break;
                fi
                echo -n "."
        done

        echo -e "Job submission to SLURM Completed and CH process!!\n"
}

####### Main

function usage
{
	echo -e "Usage: ./dna.sh <dot-product binary> <requested SLURM resource script>"
}

if [ $# -gt 0 ]; then
	echo "Command line contains $# arguments"
	dna_start_rc_resource 
	ret=$?
	if [ $ret -eq 0 ]; then
		echo -e "Completed Successfully!!"
	else
		echo -e "Some issue occured!!"
	fi	
else
	echo "Command line contains no arguments"
	usage
fi

