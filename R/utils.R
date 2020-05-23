## Put common utilities here

# How many seconds have elapsed since start time t0 (as defined by a t0 <- Sys.time() call) 
get_seconds_elapsed <- function(t0) {
  return(as.numeric(difftime(Sys.time(), t0, units = 'secs')))
}

# Use Sys.sleep() to pause until a certain amount of time has elapsed
pause_until <- function(start_time,min_time,debug=FALSE) {
  ## Make sure the proper amount of time has elapsed for the query per min_time
  seconds_elapsed <- get_seconds_elapsed(start_time)
  if (debug == TRUE) message(paste0('Time elapsed: ', round(seconds_elapsed,1),' seconds'))
  
  # Sleep if necessary to make query take the minimum amount of time
  if (seconds_elapsed < min_time) {
    Sys.sleep(min_time - seconds_elapsed)
    if (debug == TRUE) message(paste0('Time elapsed (after sleep): ', 
                                      round(get_seconds_elapsed(start_time),1),' seconds'))
  }
}
