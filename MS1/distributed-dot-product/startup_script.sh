#!/bin/bash
path=/usr/local/Cluster-Apps/slurm/bin
PWD=`pwd`

#Initial setup before submitting the job
function Setup {
	echo -e "Setup initiated!!"
	mkdir -p $PWD/DNA_Logs
	touch $PWD/CAD.list
	touch $PWD/dot-product-result.log

}

#This function will initiate the job submission using sbatch
function JobSubmission {
	echo -e "Job submission to SLURM initiated!!"
	getJobID=`$path/sbatch $PWD/submission.sh | awk '{print $4}'`
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
	done
		
	echo -e "Job submission to SLURM Completed and CH process!!\n"

}

#This function will collect the logs corresponding to particular JOBID
function Logs {
	mkdir -p $PWD/DNA_Logs/Logs-Job.$getJobID	
	mv $PWD/machine.file.$getJobID $PWD/DNA_Logs/Logs-Job.$getJobID
	mv $PWD/CAD.$getJobID.list $PWD/DNA_Logs/Logs-Job.$getJobID
	mv $PWD/slurm-$getJobID.out $PWD/DNA_Logs/Logs-Job.$getJobID
	mv $PWD/dot-product-result.log $PWD/DNA_Logs/Logs-Job.$getJobID
}

#Main Program
Setup
JobSubmission
Logs
