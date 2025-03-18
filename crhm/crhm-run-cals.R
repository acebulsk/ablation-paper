library(dplyr)
library(tidyr)
library(furrr)
library(fs)  # For file operations

# Set up parallel processing
# plan(multisession, workers = parallel::detectCores() - 1) 
# plan(multisession, workers = 2) 

crhm_run_script <- 'crhm/crhm-run.sh'
prj_name <- "ffr_closed_canopy_cc0.88_cansnobal_for_calibration"
prj_file_orig <- paste0('crhm/prj/', prj_name, '.prj')  # Original PRJ file
cal_run_id <- 'a'

# Setup parameter space
unld_to_melt_ratio <- seq(0.05, 0.2, by = 0.01)
SW_to_LW_fn <- seq(0.05, 0.2, by = 0.01)

par_space <- expand_grid(unld_to_melt_ratio, SW_to_LW_fn) |> 
  mutate(cal_id = paste0(cal_run_id, sprintf("%05d", row_number()))) |>
  select(cal_id, unld_to_melt_ratio, SW_to_LW_fn) #|> 
  #slice(1:3)  # Select only the first 3 runs for testing

# Function to execute each run
run_crhm <- function(cal_id, unld_to_melt_ratio, SW_to_LW_fn) {
  
  cal_prj_name <- paste0(prj_name, '_', cal_id)
  
  # Define a unique project file for this run
  prj_file_run <- paste0('crhm/prj/', cal_prj_name, '.prj')
  
  # Copy the original .prj file to avoid overwriting
  file_copy(prj_file_orig, prj_file_run, overwrite = TRUE)
  
  # Update the copied project file with new parameters
  CRHMr::setPrjParameters(prj_file_run, 'CanSnobalCRHM unld_to_melt_ratio', unld_to_melt_ratio)
  CRHMr::setPrjParameters(prj_file_run, 'CanSnobalCRHM SW_to_LW_fn', SW_to_LW_fn)
  
  # Generate run tag
  run_tag <- paste0("cal_ID_", cal_id, 
                    "_unl_melt_r_", unld_to_melt_ratio, 
                    "_B_", SW_to_LW_fn)
  
  # Construct and execute the command
  command <- paste(crhm_run_script, cal_prj_name, run_tag)
  system(command, wait = FALSE)  # Run in background
  # Print status
  print(paste("Completed run:", cal_id))
}

# Sequential execution using a for-loop
# Sequential execution using a for-loop
for (i in seq_len(nrow(par_space))) {
  run_crhm(
    cal_id = par_space$cal_id[i], 
    unld_to_melt_ratio = par_space$unld_to_melt_ratio[i], 
    SW_to_LW_fn = par_space$SW_to_LW_fn[i]
  )
  
  # Wait for 10 seconds between runs
  cat("Waiting 10 seconds before next run...\n")
  Sys.sleep(10)  # Wait for 30 seconds
}

# TODO need to fix log file output in crhm see CRHMArguments.cpp class to do this 
# test non-parallel
#par_space |> purrr::pmap(run_crhm)

# Run in parallel
#par_space |> future_pwalk(run_crhm)
