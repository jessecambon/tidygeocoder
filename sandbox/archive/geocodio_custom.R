library(tidygeocoder)
library(tidyverse)


some_addresses <- tribble(
  ~name,                  ~addr,
  "White House",          "1600 Pennsylvania Ave, Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
  "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"                                  
)

test <- some_addresses %>% geocode(
  addr, method = "geocodio", 
  full_results = TRUE,
  custom_query = list(fields = 'cd,stateleg')
)
