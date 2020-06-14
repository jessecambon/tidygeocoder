test1 <- geo(address = c('11 Wall St New York, NY', '1600 Pennsylvania Ave NW Washington, DC', '11 Wall St New York, NY'), verbose = TRUE)
test2 <- geo(address = '11 Wall St New York, NY')
test3 <- geo(address = '11 Wall St New York, NY')
test4 <- geo(street = c('1600 Pennsylvania Ave NW', '11 Wall Street'), city = c('Washington', 'New York'), state = c('DC', 'NY'), verbose = TRUE)

census_geo1 <- test1 <- geo(address = c('11 Wall St New York, NY', '1600 Pennsylvania Ave NW Washington, DC', '11 Wall St New York, NY'), 
                  verbose = TRUE, full_results = TRUE, return = 'geographies')

census_geo2 <- test1 <- geo(address = c('11 Wall St New York, NY', '1600 Pennsylvania Ave NW Washington, DC', '11 Wall St New York, NY'), 
      verbose = TRUE, full_results = TRUE, return = 'locations', unique_only = TRUE)

## Batch Geocodio 
geocodio_batch <- geo(address = c('11 Wall St New York, NY', '1600 Pennsylvania Ave NW Washington, DC',
                '11 Wall St New York, NY', 'Toronto, Canada'),
          verbose = TRUE, method = 'geocodio', full_results = TRUE, flatten = FALSE)

geocodio_batch_c1 <- geo(method = 'geocodio', street = c('1600 Pennsylvania Ave NW', '11 Wall Street'), 
                city = c('Washington', 'New York'), state = c('DC', 'NY'), verbose = TRUE, full_results = TRUE)

geocodio_batch_c2 <- geo(method = 'geocodio', street = c('1600 Pennsylvania Ave NW', '11 Wall Street', ''), 
  city = c('Washington', 'New York', 'Nashville'), state = c('DC', 'NY', 'TN'), verbose = TRUE, full_results = TRUE)

geocode_geocodio1 <- tibble::tibble(address = c('11 Wall St New York, NY', 
    '1600 Pennsylvania Ave NW Washington, DC', '11 Wall St New York, NY', 
    'Toronto, Canada')) %>% 
  geocode(address = address, verbose = TRUE, method = 'geocodio', full_results = TRUE)


iq1 <- geo(address = 'Lima, Peru', method = 'iq')
iq2 <- geo(city = c('Beijing', 'Lima'), country = c('China','Peru'), method = 'iq', verbose = T)

## full results
full1 <- geo_osm('Lima, Peru', full_results = TRUE)
full2 <- geo('Vancouver, Canada', method = 'geocodio', full_results = TRUE, verbose = T)
full3 <- geo('11 Wall St, New York, NY', method = 'census', full_results = TRUE, verbose = T)


## Testing helper functions

package_addresses(city = c('Lima'), country = c('Peru'), verbose = T)
package_addresses(city = c('Lima', 'Shanghai'), country = c('Peru', 'China'), verbose = T)


## Advanced Options

geo(c('11 Wall St NY, NY', '1600 Pennsylvania Ave NW Washington, DC', '11 Wall St NY, NY'),verbose = T, unique_only = T, return_addresses = T)
geo(c('11 Wall St NY, NY', '1600 Pennsylvania Ave NW Washington, DC', ''), method = 'osm', verbose = T, unique_only = F, return_addresses = T, no_query = T)
gca1 <- geo(c('11 Wall St NY, NY', '1600 Pennsylvania Ave NW Washington, DC', ''), method = 'geocodio', mode = 'single', 
              verbose = T, unique_only =T, return_addresses = T, full_results = T)
gca2 <- geo(c('11 Wall St NY, NY', ''), method = 'geocodio', mode = 'batch', verbose = T, unique_only = T, return_addresses = T, full_results = T)

sample_addresses %>% geocode(addr, return_addresses = TRUE, lat = latitude, long = longitude, return = 'geographies', full_results = T, verbose = T)
