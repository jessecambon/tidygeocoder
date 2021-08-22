# some slow computation
slow_function <- function(x, pb = NULL){
  if(!is.null(pb)){
    pb()
  }
  
  # do nothing with x for now
  Sys.sleep(0.5)
  return(x)
}

## single geocoding bar
# handlers(list(
#   handler_progress(
#     format   = ":spin Geocoding :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
#     width    = 60,
#     complete = "+",
#     clear = FALSE
#   )))

## batch bar
handlers(list(
  handler_progress(
    format   = ":spin Geocoding: :total [:bar] elapsed: :elapsed",
    width    = 80,
    complete = "+",
    clear = FALSE
  )))

library(progressr)
pb_slow <- function(x_vec){
  pb <- progressor(along = x_vec)
  res <- mapply(slow_function, x_vec,
                MoreArgs = (list(pb = pb)))
  return(res)
}

with_progress(
  pb_slow(1 : 12)
)

# emulate a batch job (ie. no feedback on progress)
pb_batch <- function(sleep) {
  pb <- progressor(steps = 1)
  Sys.sleep(sleep)
}

with_progress(
  pb_batch(6)
)
