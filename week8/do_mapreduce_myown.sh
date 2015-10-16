#!/bin/bash

#######################################
#set variables
HELP="\nSpecify users, movies, or ratings with ${0}.\nFor example:\n   ${0} users\n"
STREAMING=/usr/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-mr1.jar
SCRIPTS=/home/cloudera/Exercises/scripts
HDFS_LOC=/user/data/movielens

#######################################
#if no input is given show help
if [ $# -eq 0 ]; then
  echo -e $HELP
  exit 1
fi

#######################################
#run the command, first delete contents of folder
hadoop fs -rm -r ${HDFS_LOC}/cleaned/${1}
hadoop jar $STREAMING \
	-files ${SCRIPTS}/clean_${1}_dat_myown.py \
	-Dmapred.reduce.tasks=0 \
	-input ${HDFS_LOC}/raw/${1} \
	-output ${HDFS_LOC}/cleaned/${1} \
	-mapper clean_${1}_dat_myown.py
