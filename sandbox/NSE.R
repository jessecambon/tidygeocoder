# http://adv-r.had.co.nz/Computing-on-the-language.html

## NSE - passing a variable name as a function argument
select_mt2 <- function(x) {
  mtcars[deparse(substitute(x))]
}

select_mt2(cyl)


## Using the same variable name as a function that you call

show_digits <- function(digits = 0) {
  x <- round(mtcars$wt, digits = digits)
  
  print(x)
}

show_digits(digits = 2)

## Using ...

show_digits <- function(...) {
  x <- round(mtcars$wt, ...)
  
  print(x)
}

show_digits(digits = 1)
