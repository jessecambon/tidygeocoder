# Census
## https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.pdf

### NOTE: different endpoint urls for US and Europe are available

df <- data.frame(
  id = id,
  street = .data[[street]],
  city = city,
  state = state,
  zip = zip,
  stringsAsFactors = FALSE
)

library(httr)
library(jsonlite)

# Write a Temporary CSV
tmp <- tempfile(fileext = '.csv')
utils::write.table(df, tmp, col.names = FALSE, row.names = FALSE,
                   na = '', sep = ',')


addr <- c('901 15th St NW Washington, DC', '1600 Pennsylvania Ave Washington, DC')

url_base <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"

# limit=1 limits the query to one result
req <-
  httr::POST(url,
             body = list(
               addressFile = httr::upload_file(tmp),
               benchmark = benchmark,
               vintage = vintage,
               format = 'json'
             ),
             encode = 'multipart',
             httr::timeout(timeout * 60)
  )

cnt <- httr::content(req, as = 'text', encoding = 'UTF-8')
resp <- httr::GET(url = url_base, 
      query = list(address = addr, format = 'json', benchmark = '4'))

df <- utils::read.csv(text = cnt, header = FALSE,
                      col.names = cols,
                      fill = TRUE, stringsAsFactors = FALSE,
                      na.strings = '')

# dataframe is returned
dat <- jsonlite::fromJSON(httr::content(resp, as = 'text', encoding = "UTF-8"), simplifyVector = TRUE)

# Obtain latitude and longitude, take first one if there are multiple
coord_xy <- dat$result$addressMatches$coordinates[1,]

lat_lng <- c(coord_xy$y, coord_xy$x)

