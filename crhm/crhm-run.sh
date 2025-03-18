#!/bin/bash

# A script to run CRHM CLI with proper logging and error handling

cur_datetime=$(date +"%Y-%m-%d-%H-%M-%S")

# Get input values for prj_name and run_tag
prj_name=$1
run_tag=$2

# Check if prj_name and run_tag are provided
if [ -z "$prj_name" ] || [ -z "$run_tag" ]; then
  echo "Usage: $0 <prj_name> <run_tag>"
  exit 1
fi

prj_file=${HOME}/local-usask/working-papers/ablation-paper/crhm/prj/${prj_name}.prj

mkdir -p ${HOME}/local-usask/working-papers/ablation-paper/crhm/output/${prj_name}
mkdir -p ${HOME}/local-usask/working-papers/ablation-paper/crhm/logs/${prj_name}

out_file=${HOME}/local-usask/working-papers/ablation-paper/crhm/output/$prj_name/${cur_datetime}_${prj_name}_${run_tag}_output.txt

log_file=${HOME}/local-usask/working-papers/ablation-paper/crhm/logs/$prj_name/${cur_datetime}_${prj_name}_${run_tag}.log

crhm_cmd="/home/alex/Documents/code/crhmcode/crhmcode/build/crhm $prj_file -o $out_file"

# Log the run information
echo -e "\nSpinning up CRHM... \n\nPrj: $prj_name \nRuntag: $run_tag \n\nWriting logs to: $log_file \nOutput to: $out_file.\n" | tee -a $log_file
echo -e "\nSending $crhm_cmd to the command line.\n" | tee -a $log_file

# Run CRHM and capture both stdout and stderr
$crhm_cmd > $log_file 2>&1

# Check if CRHM exited successfully
if [ $? -ne 0 ]; then
  echo -e "\nError: CRHM encountered an issue. Check $log_file for details.\n" | tee -a $log_file
  exit 1
fi

# Handle the CRHM log file (only if it exists)
if [ -f crhmRun.log ]; then
  echo -e "\nDetailed CRHM run logs copied from main directory:\n" >> $log_file
  cat crhmRun.log >> $log_file
  rm crhmRun.log
else
  echo -e "\nWarning: crhmRun.log not found, skipping copy.\n" >> $log_file
fi

# Append PRJ file values to log
echo -e "\nPrj File Values for This Run:\n" >> $log_file
cat $prj_file >> $log_file

