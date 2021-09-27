#!/bin/bash

#create results folder
mkdir -p results
#remove .log files
rm -r *.log
#create new output file
rm -r 0-output.txt
cp -r 0-output_raw.txt 0-output.txt
#run hype
#HYPE_src='/mnt/d/Work/Resources/Models/hype_5_12_1_src'
HYPE_src='../Code_testing/hype_5_12_1_src_HGDM_v01'

#create new output file
rm -r $HYPE_src/0-output.txt
cp -r $HYPE_src/0-output_raw.txt $HYPE_src/0-output.txt

$HYPE_src/hype