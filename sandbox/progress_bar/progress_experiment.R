# https://github.com/r-lib/progress

library(progress)


# elapsed time (could use for batch geocoding)
pb <- progress_bar$new(
  format = "  downloading [:bar] :elapsedfull", clear = FALSE, width= 60)
for (i in 1:1000) {
  pb$tick()
  Sys.sleep(1 / 100)
}
