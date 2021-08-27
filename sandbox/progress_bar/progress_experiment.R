# https://github.com/r-lib/progress

library(progress)

# pb <- progress_bar$new(
#   format = "  downloading [:bar] :elapsedfull",
#   total = 1000, clear = FALSE, width= 60)
# 
# f <- function() {
#   pb$tick(0)
#   Sys.sleep(2)
#   for (i in 1:total) {
#     pb$tick(1)
#     Sys.sleep(1 / 100)
#   }
# }
#f()


target_func <- function(x, pb) {
  Sys.sleep(1/5)
  pb$tick()
  return(x)
}


total <- 25
pb <- progress_bar$new(
  format = "[:bar] Geocoding: :current/:total (:percent)",
  clear = FALSE,
  total = total)

pb$tick(0)
mapply(target_func, 1:total, MoreArgs = list(pb = pb))



