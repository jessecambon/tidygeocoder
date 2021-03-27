### Maximum number of addresses/coordinates allowed for each service

batch_limit_reference <- tibble::tribble(
  ~method,   ~batch_limit, 
  "census",    1e4,
  "geocodio",  1e4,
  "tomtom",    1e4,       
  "here",      1e6,    
  "mapquest",  100,    
  "bing",      50,
)
  
usethis::use_data(batch_limit_reference, overwrite = TRUE)
