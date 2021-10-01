

test_func <- function(a = 1, b = 5) {
  
  if(missing("a")) print('a is missing!') else print('a is not missing!')
  
#  return(as.list(environment()))
}

test_func(a=1)
