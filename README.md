# tidygeo

A tidyverse-style geocoder interface for R. Utilizes US Census and Nominatim (OSM) geocoder services. Returns latitude and longitude in tibble format for easy of use.


## Installation Instructions

Must have devtools installed. Run `install.packages('devtools')` if this is not the case.

```
library(devtools)
install_github("jessecambon/tidygeocoder")
```

## Development Notes

* Run roxygenise() to create NAMESPACE file.
