# OSM query

city <- c('New York', 'Chicago')
state <- c('New York', 'Illinois')
country <- c('United States', 'United States')

dummy_print <- function(city, state, country, z) {
  print(paste0('city: ', city))
  print(paste0('state: ', state))
  print(paste0('country: ', country))
  print(z)
  return(paste0(city, ', ', state, ', ', country))
}

#x <- mapply(dummy_print, city, state, country)
x <- do.call(mapply, list(FUN = dummy_print, USE.NAMES = FALSE, city = city, state = state, country = country, z = 1))


# https://nominatim.org/release-docs/develop/api/Search/
url_base <- "http://nominatim.openstreetmap.org/search"

library(httr)
library(jsonlite)

# limit =1 limits query to one result

soup <- httr::GET(url = url_base, 
                  query = list(q = addr, format = 'json',limit = '1'))

dat <- jsonlite::fromJSON(httr::content(soup, as = 'text', encoding = "UTF-8"), simplifyVector = TRUE)

# Get lat/long coordinates
coords <- as.numeric( c(dat$lat, dat$lon) )
