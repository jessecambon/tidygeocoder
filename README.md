# tidygeocoder

A tidyverse-style geocoder interface for R. Utilizes US Census and Nominatim (OSM) geocoder services. Returns latitude and longitude in tibble format for ease of use.


## Installation Instructions

To download, install, and load package from github

```
install.packages('devtools')
library(devtools)
install_github("jessecambon/tidygeocoder")
library(tidygeocoder)
```


## Notes For Me

* General Instructions: http://r-pkgs.had.co.nz/
* Devtools cheat sheet: https://www.rstudio.com/wp-content/uploads/2015/03/devtools-cheatsheet.pdf
* Create man documents based off of R/ directory code files with CTRL+SHIFT+D (devtools::document(roclets=c('rd', 'collate', 'namespace')))
* Use load_all() from devtools to load the package (for testing)
* Create new vignette with `use_vignette("my-vignette")`

## Todo

* Add github example with a map
* Implement a tidy cascade function
* Make OSM able to take custom lat/long column names
* Tell user when they have hit usage limit for OSM?
