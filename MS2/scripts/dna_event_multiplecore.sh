#!/bin/bash

	
if [ $# -eq 3 ]; then
	callFunc=$1
	rc=$2
	if [ $callFunc == "start" ]; then
		resource=$3
	else
		JOBID=$3
	fi
	
elif [ $# -eq 4 ]; then
	callFunc=$1
	rc=$2
	JOBID=$3
	slurmJobNodeList=$4
else
	callFunc=usage
	
fi

PWD=`pwd`
#binPath=$HOME/.cabal/bin
binPath=$HOME/RC/MS1/ddp-erlang-style
Path=/usr/local/Cluster-Apps/slurm/bin
getUser=`id -n -u`
partition=`sacctmgr list user $getUser WithAssoc | awk '{print $6}' | tail -1`

function start 
{
	submitJobToSLURM $resource $rc
	if [ $? -eq 0 ]; then
		echo -e "Job submission completed!!"
	else
		echo -e "Error : Some issue occurs while submitting!!"
	fi



}

#Since, sbatch exits immediately after the script is successfully transferred to the SLURM controller and assigned a SLURM job ID. 
#So, this function is required to keep the script alive untill all the execution completed.
function submitJobToSLURM {

	getResource=`echo $1`
        echo -e "Job submission to SLURM initiated!!"
        getJobID=`$Path/sbatch $PWD/$getResource $binPath/$rc | awk '{print $4}'`
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

function check {
	slurmJobNodeList=$PWD/machine.file.$JOBID

	if [ -f $slurmJobNodeList ]; then
       		for machine in `cat $slurmJobNodeList`
        	do
                	for rcPid in `ssh $machine ps -ef | grep $rc | grep -v grep | grep -v slurmd | grep -v $callFunc |awk '{print $2}'`
                	do
				if [ "$rcPid" == "" ]; then
					echo "Check operation performed on $machine. No binary running!!"
				else
					echo "Binary($rc) still running on $machine. Hence, exiting!!"
					echo "Please execute - dna stop $rc $resource !!"
					return 1
				fi	
                	done
        	done
	else
		echo "Error : Either JOBID is wrong or machine.file.$JOBID has been removed!!"
		return 1
	fi
}

function stop {

	slurmJobNodeList=$PWD/machine.file.$JOBID

	if [ -f $slurmJobNodeList ]; then
       		for machine in `cat $slurmJobNodeList`
        	do
                	for rcPid in `ssh $machine ps -ef | grep $rc | grep -v grep | grep -v slurmd | grep -v $callFunc | grep -v bin | awk '{print $2}'`
                	do
				if [ "$rcPid" == "" ]; then
					echo "Stop operation performed. No binary running to kill!!"
				else
					echo "Killing binary ($rc) running on $machine!!"
					kill $rcPid
				fi	
                	done
        	done
	else
		echo "Error : Either JOBID is wrong or machine.file.$JOBID has been removed!!"
		return 1
	fi
}

#This function generated CAD list
function generatingCADFile {
        echo -e "Generating CAD File\n============\n"
        touch $PWD/CAD.$JOBID.file

	slurmJobNodeList=$PWD/machine.file.$JOBID
	numberOfCores=12	
        #portNo=79001 # Has some issue with automation, but can be done.
        # Create a file in the format <Hostname>:<Port no>
	for machine in `cat $slurmJobNodeList`
        do
        portNo=79001
		getIP=`ssh $machine nslookup $machine | grep Address | tail -1 | awk '{print $2}'` 
		coreCount=1	
		while [ $coreCount -le $numberOfCores ]; 
		do
			echo $getIP | sed "s/$/:${portNo}/" >> $PWD/CAD.$JOBID.file
			portNo=`expr ${portNo} + 1`
			coreCount=`expr $coreCount + 1`
		done
        done
        echo `cat $PWD/CAD.$JOBID.file`
	machineCount=`cat $slurmJobNodeList | wc -l`
	totalmachineCount=`expr $machineCount \* $numberOfCores`
	countCAD=`cat $PWD/CAD.$JOBID.file | wc -l`
	if [ $totalmachineCount -eq $countCAD ]; then
		echo "CAD file generated successfully!"
	else
		echo "CAD file is either incomplete or wrong information!!"
		return 1
	fi
}

function md5sum {
        /usr/bin/md5sum $rc >  $PWD/md5sum.md5
        status=`/usr/bin/md5sum -c $PWD/md5sum.md5`
        if [ "$status" == "$rc: OK" ];then
                return 0
        else
                return 1
        fi
}

function startingCHprocess {
set -x
        echo -e "\n============\nCH Process initiated\n============\n"
        flag=0
	numberOfCores=12
	noOfProcess=`cat $slurmJobNodeList | wc -l`
	noOfComputeP=`expr $noOfProcess - 1`
	noOfComputeP=$(($noOfComputeP * $numberOfCores))
	itemCount=`expr $noOfComputeP \* 15000000`
	itemCounti=`expr $numberOfCores \* 150`
	#itemCount=`expr $itemCounti \* $noOfProcess`
	linkCount=0
	linkCountM=0
	# chunkSize is actually chunkNo and goes from one to the number of threads
    chunkSize=0
        for machine in `cat $slurmJobNodeList`
        do
        	if [ $flag -eq 0 ]
                then
                    echo -e "Skip Master node IP\n============\n"
                    getMasterIP=$machine
                    flag=1
                else
                        ipAdd=`ssh $machine nslookup $machine | grep Address | tail -1 | awk '{print $2}'`
			cat $PWD/CAD.$JOBID.file | grep -w "$ipAdd" | sed -n '1,12p' >> $PWD/temp_CAD.txt
			cat $PWD/temp_CAD.txt
			cp $rc slave.$machine
			# chunkSize=0
			if [[ $linkCount -eq 0 ]];then
				srun -p $partition --nodelist $machine -N1 -n1 --exclusive ln -s /ramdisks/file.$machine $HOME/input_data.txt
				linkCount=1
			fi
			while read -u 3 -r line || [[ -n $line ]];
			do
				chunkSize=`expr $chunkSize + 1`
				portNo=`echo $line | cut -f2 -d":"`
                echo -e "\n============\nStarting Slave==<$ipAdd>=====<$portNo>=====\n============\n"
                # create-float /tmp/file.slave1 itemcount processcount chunkno
				srun -p $partition --nodelist $machine -N1 -n1 --exclusive ./create-floats /ramdisks/file.$machine $itemCount $noOfComputeP $chunkSize
                srun -p $partition --nodelist $machine -N1 -n1 --exclusive ./slave.$machine slave --cad $PWD/CAD.$JOBID.file --ip $ipAdd --port $portNo +RTS -l-au &
			done 3< $PWD/temp_CAD.txt

			rm -rf $PWD/temp_CAD.txt

                	sleep 2
               		echo -e "Sleeping for 2 secs!!"
                fi
		rm -rf $PWD/temp_CAD.txt
        done

	rm -rf $PWD/temp_CAD.txt
        ipAdd=`nslookup $getMasterIP | grep Address | tail -1 | awk '{print $2}'`
	cat $PWD/CAD.$JOBID.file | grep $ipAdd | sed -n '1,12p' >> $PWD/temp_CAD.txt
	cp $rc master.$getMasterIP	
	
	srun -p $partition --nodelist $getMasterIP -N1 -n1 --exclusive rm -rf $HOME/input_data.txt
	srun -p $partition --nodelist $getMasterIP -N1 -n1 --exclusive touch /ramdisks/file.$getMasterIP
	srun -p $partition --nodelist $getMasterIP -N1 -n1 --exclusive ln -s /ramdisks/file.$getMasterIP $HOME/input_data.txt
	while read -u 3 line
        do
		portNo=`echo $line | cut -f2 -d":"`
		srun -p $partition --nodelist $getMasterIP -N1 -n1 --exclusive ./create-floats /ramdisks/file.$getMasterIP $itemCount $noOfComputeP 0
        	echo -e "\n============\nStarting Master====<$ipAdd>=====<$portNo>=====\n============\n"
       		srun -p $partition --nodelist $getMasterIP -N1 -n1 --exclusive ./master.$getMasterIP master --cad $PWD/CAD.$JOBID.file --ip $ipAdd --port $portNo --filename $HOME/input_data.txt +RTS -l-au 
	done 3< $PWD/temp_CAD.txt
	rm -rf $PWD/temp_CAD.txt
        echo -e "\n============\nCH Process initiation completed\n============\n"
}

function collectLogs {
	echo -e "\n============\nCollecting all the logs\n============\n"
	mkdir -p $PWD/Logs_$JOBID

	flag=0
	for machine in `cat $slurmJobNodeList`
	do
		if [ $flag -eq 0 ]
                then
			ghc-events show $PWD/master.$machine.eventlog > $PWD/Logs_$JOBID/master.$machine.eventlog.txt
			mv $PWD/master.$machine.eventlog $PWD/Logs_$JOBID/.
			rm $PWD/master.$machine
			flag=1
		else
			ghc-events show $PWD/slave.$machine.eventlog > $PWD/Logs_$JOBID/slave.$machine.eventlog.txt
			mv $PWD/slave.$machine.eventlog $PWD/Logs_$JOBID/.
			rm $PWD/slave.$machine
		fi
		#srun -p $partition --nodelist $machine -N1 -n1 --exclusive rm -rf $HOME/input_data.txt
		srun -p $partition --nodelist $machine -N1 -n1 --exclusive rm -rf /ramdisks/file.$machine 
		rm -rf $PWD/temp_CAD.txt
	done		
	mv $PWD/slurm-$JOBID.out $PWD/Logs_$JOBID/.
	mv $PWD/machine.file.$JOBID $PWD/Logs_$JOBID/.
	mv $PWD/CAD.$JOBID.file $PWD/Logs_$JOBID/.
	mv $PWD/md5sum.md5 $PWD/Logs_$JOBID/.

	echo -e "\n============\nCollecting of logs completed\n============\n"
	
	
}

####### Main

function usage
{
	echo -e "Usage: "
	echo -e "./dna.sh start <dot-product binary> <requested SLURM resource script>"
	echo -e "./dna.sh check <dot-product binary> <JOBID>"
	echo -e "./dna.sh stop <dot-product binary> <JOBID>"
	
}

$callFunc 