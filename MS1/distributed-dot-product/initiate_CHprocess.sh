#!/bin/bash
SLURM_JOB_NODELIST=$1
JOBID=$2
EXE=$3

PWD=`pwd`

function generateSlurmNodeList {
	echo -e "JobID: $JOBID\n======"
	echo "Time: `date`"
	echo "Running on master node: `hostname`"
	echo "Current directory: $PWD"

	if [ "$SLURM_JOB_NODELIST" ]; then
        	#! Create a machine file:
        	export NODEFILE=`generate_pbs_nodefile`
        	cat $NODEFILE | uniq > $PWD/machine.file.$JOBID
        	echo -e "\nNodes allocated:\n================"
        	echo `cat $PWD/machine.file.$JOBID | sed -e 's/\..*$//g'`
	fi
}

#This function generated CAD list
function generatingCADlist {
	echo -e "Generating CAD List\n============\n"
	mv $PWD/CAD.list $PWD/CAD.$JOBID.list
	portNo=6010
	# Create a file in the format <Hostname>:<Port no>
	while read line
	do
        	echo $line | sed "s/$/:${portNo}/" >> $PWD/CAD.$JOBID.list
        	portNo=`expr ${portNo} + 1`

	done<machine.file.$JOBID
	echo `cat $PWD/CAD.$JOBID.list`
}

function startingCHprocess {
	
	echo -e "\n============\nCH Process initiated\n============\n"
	flag=0
	for machine in `cat machine.file.$JOBID`
	do
		if [ $flag -eq 0 ]
		then
			echo -e "Skip Master node IP\n============\n"
			getMasterIP=$machine
			flag=1
		else
        		ipAdd=`ssh $machine nslookup $machine | grep Address | tail -1 | awk '{print $2}'`
        		portNo=`cat $PWD/CAD.$JOBID.list | grep $machine | cut -f2 -d":"`
			echo -e "\n============\nStarting Slave==<$ipAdd>=====<$portNo>=====\n============\n"
        		ssh $machine ps -ef | grep dot-product
        		srun -p tesla --nodelist=$machine --exclusive $EXE slave $ipAdd $portNo &
		fi
        	sleep 2
        	echo -e "Sleeping for 2 secs!!"
	done

        ipAdd=`nslookup $getMasterIP | grep Address | tail -1 | awk '{print $2}'`
        portNo=`cat $PWD/CAD.$JOBID.list | grep $getMasterIP | cut -f2 -d":"`
        echo -e "\n============\nStarting Master====<$ipAdd>=====<$portNo>=====\n============\n"
	echo -e "Results of dot-product\n=============================\n" >> $PWD/dot-product-result.log
        srun -p tesla --nodelist=$getMasterIP --exclusive $EXE master $ipAdd $portNo >> $PWD/dot-product-result.log 

	wait
	echo -e "\n============\nCH Process initiation completed\n============\n"
}

#Main program
generateSlurmNodeList
generatingCADlist
startingCHprocess


