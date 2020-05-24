# http://adv-r.had.co.nz/Computing-on-the-language.html

select_mt <- function(x) {
  x_sub <- substitute(x)
  mtcars[deparse(x_sub)]
}

select_mt(cyl)



select_mt2 <- function(x) {
  mtcars[deparse(substitute(x))]
}

select_mt2(cyl)