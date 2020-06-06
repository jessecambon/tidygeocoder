test1 <- geo(address = c('11 Wall St New York, NY', '1600 Pennsylvania Ave NW Washington, DC', '11 Wall St New York, NY'), verbose = TRUE)
test2 <- geo(address = '11 Wall St New York, NY')
test3 <- geo(address = '11 Wall St New York, NY')
test4 <- geo(street = c('1600 Pennsylvania Ave NW', '11 Wall Street'), city = c('Washington', 'New York'), state = c('DC', 'NY'), verbose = TRUE)