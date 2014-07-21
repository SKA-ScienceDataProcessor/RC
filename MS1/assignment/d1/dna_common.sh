#!/bin/bash
funcExec=$1
slurmJobNodeList=$2
JOBID=$3
rc=$4

PWD=`pwd`
partition=`cat $PWD/config.cf | grep -w "Partition" | awk '{print $3}'`
path=`cat $PWD/config.cf | grep -w "Path" | awk '{print $3}'`


function dna_check_rc {

	check_rc="ps -df | grep $rc | awk '{print \$2}'"
        for machine in `cat $slurmJobNodeList`
	do
		for pid in `ssh $machine ps -df | grep $rc | grep -v grep | grep -v $funcExec | grep -v slurmd`
		do
			echo "PID at" $machine ..$pid
			dna_stop_rc $pid
		done
	done
	
}   

function dna_stop_rc {
	getPid=`echo $1`
	kill $getPid
	rm -rf $slurmJobNodeList 
}

	
#This function generated CAD list
function generatingCADFile {

	echo -e "Generating CAD File\n============\n"
	touch $PWD/CAD.$JOBID.file
	portNo=6010 # Has some issue with automation, but can be done.
	# Create a file in the format <Hostname>:<Port no>
	while read line
	do
        	echo $line | sed "s/$/:${portNo}/" >> $PWD/CAD.$JOBID.file
        	portNo=`expr ${portNo} + 1`

	done<$slurmJobNodeList
	echo `cat $PWD/CAD.$JOBID.file`
}

function md5sum {

	/usr/bin/md5sum $path >  $PWD/md5sum.md5	
	status=`/usr/bin/md5sum -c $PWD/md5sum.md5`
	if [ "$status" == "$path: OK" ];then
		return 0
	else
		return 1
	fi
}
	
function startingCHprocess {
	
	echo -e "\n============\nCH Process initiated\n============\n"
	flag=0
	for machine in `cat $slurmJobNodeList`
	do
		if [ $flag -eq 0 ]
		then
			echo -e "Skip Master node IP\n============\n"
			getMasterIP=$machine
			flag=1
		else
        		ipAdd=`ssh $machine nslookup $machine | grep Address | tail -1 | awk '{print $2}'`
        		portNo=`cat $PWD/CAD.$JOBID.file | grep $machine | cut -f2 -d":"`
			echo -e "\n============\nStarting Slave==<$ipAdd>=====<$portNo>=====\n============\n"
        		srun -p tesla --nodelist=$machine --exclusive $rc slave $ipAdd $portNo 1>> $PWD/slurm-$JOBID.out &
		fi
        	sleep 2
        	echo -e "Sleeping for 2 secs!!"
	done

        ipAdd=`nslookup $getMasterIP | grep Address | tail -1 | awk '{print $2}'`
        portNo=`cat $PWD/CAD.$JOBID.file | grep $getMasterIP | cut -f2 -d":"`
        echo -e "\n============\nStarting Master====<$ipAdd>=====<$portNo>=====\n============\n"
	echo -e "Results of dot-product\n=============================\n" >> $PWD/dot-product-result.log
        srun -p tesla --nodelist=$getMasterIP --exclusive $rc master $ipAdd $portNo 1>> $PWD/slurm-$JOBID.out

	wait
	echo -e "\n============\nCH Process initiation completed\n============\n"
}

$funcExec
