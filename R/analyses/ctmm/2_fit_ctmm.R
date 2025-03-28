## Fit CTMMs
library(foreach)
library(ctmm)
cores = parallel::detectCores()
cl <- parallel::makeForkCluster(cores-1, outfile = "")
doParallel::registerDoParallel(cl)

## Read in track list without outliers
tracks <- readRDS("./outputs/telemetry/zebby_as_telemetry.RDS")

## Fitting function
fitting_function <- function(i)
{
  
  start_time <- Sys.time()
  
  ## Initial values
  ctmm_starting_values <- ctmm::ctmm.guess(tracks[[i]],
                                           CTMM = ctmm::ctmm(error = TRUE),
                                           interactive=FALSE)
  
  ## Fit models with OU anisotropic error
  fit <- ctmm::ctmm.select(tracks[[i]],
                           CTMM = ctmm_starting_values,
                           verbose = TRUE,
                           trace = 1)
  

  ## Save
  saveRDS(fit,
          paste0("./outputs/ctmm_fits/",
                 names(tracks[i]),
                 ".RDS"))
 
  time_taken = Sys.time()-start_time
  
  cat("Individual", i, "- Finished after", time_taken, "\n")
   
}

foreach(i=1:length(tracks),.packages='ctmm',
        .verbose = TRUE) %dopar%
  { fitting_function(i) }