

city <- c('New York', 'Chicago', 'New Orleans')
state <- c('New York', 'Illinois')
country <- c('United States', 'United States')

# remove a literal double quote from a string
# used with NSE
rm_quote <- function(string) gsub("\"","", string)

dummy_print <- function(city, lat = lat,recurse = FALSE) {
  lat <- lat <- rm_quote(deparse(substitute(lat)))
  
  if (recurse == TRUE) {
    return(do.call(mapply, 
list(FUN = dummy_print, USE.NAMES = FALSE, city = city, 
     MoreArgs=list(lat = lat))))
  }
  else {
    print(paste0('city: ', city))
    return(lat)
  }
}

#x <- mapply(dummy_print, city, state, country)

dummy_print(city, lat = latitude, recurse = TRUE)
