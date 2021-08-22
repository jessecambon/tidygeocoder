# some slow computation
slow_function <- function(id, x_mat, pb = NULL){
  if(!is.null(pb)){
    pb()
  }
  
  res <- x_mat[id, id]
  Sys.sleep(0.25)
  return(res)
}

handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 60,
    complete = "+"
  )))

# a large variable, ~800M
colnum <- 1000
x_mat <- matrix(rnorm(colnum ^ 2), nrow = colnum)

library(progressr)
pb_slow <- function(id_vec, x_mat){
  pb <- progressor(along = id_vec)
  res <- sapply(id_vec, slow_function, 
                       x_mat = x_mat, 
                       pb = pb)
  return(res)
}

with_progress(
  pb_slow(1 : 12, x_mat)
)
