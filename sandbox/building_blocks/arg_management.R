

print_args <- function(x,y, ...) {
  print(paste0('x = ', x))
  print(paste0('y = ', y))
}

arg_catch <- function(x,y=1, ...) {
  args  <- c(as.list(environment()), list(...))
  return(args)
 # eval.parent(this_call)
}


x <- arg_catch('blah',z = 5)

do.call(print_args,x)
