

print_args <- function(x, y, z, f) {
  print(paste0('x = ', x))
  print(paste0('y = ', y))
  print(paste0('z = ', z))
  print(paste0('f = ', f))
  class(f)
}

arg_catch <- function(x, y=1, ...) {
  args  <- c(as.list(environment()), list(...))
  do.call(print_args, args)
 # eval.parent(this_call)
}


arg_catch(x= 'blah', z = 5, f= 'return')

#do.call(print_args,x)
