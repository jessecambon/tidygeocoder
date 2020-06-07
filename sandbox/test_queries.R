test1 <- geo(address = c('11 Wall St New York, NY', '1600 Pennsylvania Ave NW Washington, DC', '11 Wall St New York, NY'), verbose = TRUE)
test2 <- geo(address = '11 Wall St New York, NY')
test3 <- geo(address = '11 Wall St New York, NY')
test4 <- geo(street = c('1600 Pennsylvania Ave NW', '11 Wall Street'), city = c('Washington', 'New York'), state = c('DC', 'NY'), verbose = TRUE)

census_geo1 <- test1 <- geo(address = c('11 Wall St New York, NY', '1600 Pennsylvania Ave NW Washington, DC', '11 Wall St New York, NY'), 
                  verbose = TRUE, full_results = TRUE, return = 'geographies')

geocodio_batch <- geo(address = c('11 Wall St New York, NY', '1600 Pennsylvania Ave NW Washington, DC',
                '11 Wall St New York, NY', 'Toronto, Canada'),
          verbose = TRUE, method = 'geocodio', full_results = TRUE, flatten = FALSE)

iq1 <- geo(address = 'Lima, Peru', method = 'iq')
iq2 <- geo(city = c('Beijing', 'Lima'), country = c('China','Peru'), method = 'iq', verbose = T)

## full results
full1 <- geo_osm('Lima, Peru', full_results = TRUE)
full2 <- geo('Vancouver, Canada', method = 'geocodio', full_results = TRUE, verbose = T)
full3 <- geo('11 Wall St, New York, NY', method = 'census', full_results = TRUE, verbose = T)


## Testing helper functions

package_addresses(city = c('Lima'), country = c('Peru'), verbose = T)
package_addresses(city = c('Lima', 'Shanghai'), country = c('Peru', 'China'), verbose = T)

