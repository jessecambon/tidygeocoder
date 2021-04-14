
<!-- README.md is generated from README.Rmd. Please edit that file directly and reknit -->

# tidygeocoder<a href='https://jessecambon.github.io/tidygeocoder/'><img src="man/figures/tidygeocoder_hex.png" align="right" height="130px"/></a>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/jessecambon/tidygeocoder/blob/master/LICENSE.md)
[![CRAN](https://www.r-pkg.org/badges/version/tidygeocoder)](https://cran.r-project.org/package=tidygeocoder)
[![CRAN Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/tidygeocoder)](https://CRAN.R-project.org/package=tidygeocoder)
[![CRAN Downloads Per
Month](http://cranlogs.r-pkg.org/badges/tidygeocoder)](https://cran.r-project.org/package=tidygeocoder)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4448251.svg)](https://doi.org/10.5281/zenodo.4448251)
<!-- badges: end -->

Tidygeocoder makes getting data from geocoder services easy. A unified
high-level interface is provided for the [supported geocoder
services](https://jessecambon.github.io/tidygeocoder/articles/geocoder_services.html)
and results are returned in [tibble](https://tibble.tidyverse.org/)
(dataframe) format. For details see the usage example below, the
[Getting Started
Vignette](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html),
and [blog posts on
tidygeocoder](https://jessecambon.github.io/tag/tidygeocoder).

**Features:**

-   Both **forward geocoding** (addresses ⮕ coordinates) and **reverse
    geocoding** (coordinates ⮕ addresses) are supported.
-   **Batch geocoding** (geocoding multiple addresses or coordinates in
    a single query) is used by default if supported by the geocoder
    service when multiple inputs (addresses or coordinates) are provided
    (with some noted exceptions for services with slower batch
    geocoding).
-   Duplicate, NA, and blank input data is handled elegantly; only
    unique inputs are submitted in queries, but the rows in the original
    data are preserved by default.
-   The usage limits of geocoder services are respected by pausing to
    slow down the rate of querying as necessary.

## Installation

To install the stable version from CRAN (the official R package
servers):

``` r
install.packages('tidygeocoder')
```

Alternatively, you can install the latest development version from
GitHub:

``` r
devtools::install_github("jessecambon/tidygeocoder")
```

## Usage

In this example we will geocode a few addresses using the `geocode()`
function and plot them on a map with ggplot.

``` r
library(dplyr)
library(tibble)
library(tidygeocoder)

# create a dataframe with addresses
some_addresses <- tribble(
~name,                  ~addr,
"White House",          "1600 Pennsylvania Ave NW, Washington, DC",
"Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
"Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"                                  
)

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr, method = 'osm', lat = latitude , long = longitude)
```

The `geocode()` function attaches latitude and longitude columns to our
input dataset of addresses. The [Nominatim
(“osm”)](https://nominatim.org/) geocoder is used here, but other
services can be specified with the `method` argument. See the `geo()`
function documentation for details.

| name                 | addr                                       | latitude |  longitude |
|:---------------------|:-------------------------------------------|---------:|-----------:|
| White House          | 1600 Pennsylvania Ave NW, Washington, DC   | 38.89770 |  -77.03655 |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79520 | -122.40279 |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87887 |  -87.63591 |

Now that we have the longitude and latitude coordinates, we can use
ggplot to plot our addresses on a map.

``` r
library(ggplot2)
library(maps)
library(ggrepel)

ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
  borders("state") + geom_point() + 
  geom_label_repel(aes(label = name)) + 
  theme_void()
```

<img src="man/figures/README-usamap-1.png" style="display: block; margin: auto;" />

To return the full results from a geocoder service (not just latitude
and longitude) you can use `full_results = TRUE`. Additionally, for the
Census geocoder you can use `return_type = 'geographies'` to return
geography columns (state, county, Census tract, and Census block).

``` r
full <- some_addresses %>%
  geocode(addr, method = 'census', full_results = TRUE, 
          return_type = 'geographies')
```

| name                 | addr                                       |      lat |       long |  id | input\_address                                  | match\_indicator | match\_type | matched\_address                                | tiger\_line\_id | tiger\_side | state\_fips | county\_fips | census\_tract | census\_block |
|:---------------------|:-------------------------------------------|---------:|-----------:|----:|:------------------------------------------------|:-----------------|:------------|:------------------------------------------------|:----------------|:------------|:------------|:-------------|:--------------|:--------------|
| White House          | 1600 Pennsylvania Ave NW, Washington, DC   | 38.89875 |  -77.03535 |   1 | 1600 Pennsylvania Ave NW, Washington, DC, , ,   | Match            | Exact       | 1600 PENNSYLVANIA AVE NW, WASHINGTON, DC, 20500 | 76225813        | L           | 11          | 001          | 980000        | 1034          |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79470 | -122.40314 |   2 | 600 Montgomery St, San Francisco, CA 94111, , , | Match            | Exact       | 600 MONTGOMERY ST, SAN FRANCISCO, CA, 94111     | 192281262       | R           | 06          | 075          | 061101        | 2014          |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87851 |  -87.63666 |   3 | 233 S Wacker Dr, Chicago, IL 60606, , ,         | Match            | Exact       | 233 S WACKER DR, CHICAGO, IL, 60606             | 112050003       | L           | 17          | 031          | 839100        | 2008          |

To perform **reverse geocoding** (obtaining addresses from latitude and
longitude coordinates), we can use the `reverse_geocode()` function. The
arguments are similar to the `geocode()` function, but we now are
specifying the latitude and longitude columns in our dataset with the
`lat` and `long` arguments. The single line address is returned in a
column named by the `address` argument. See the `reverse_geo()` function
documentation for more details on reverse geocoding.

``` r
rev1 <- lat_longs %>%
  reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                  address = address_found, full_results = TRUE)
```

| name                 | addr                                       | latitude |  longitude | address\_found                                                                                                                                   | place\_id | licence                                                                  | osm\_type |   osm\_id | osm\_lat           | osm\_lon            | historic    | house\_number | road                          | city          | state                | postcode | country       | country\_code | boundingbox                                          | tourism              | neighbourhood      | building     | suburb | county      |
|:---------------------|:-------------------------------------------|---------:|-----------:|:-------------------------------------------------------------------------------------------------------------------------------------------------|----------:|:-------------------------------------------------------------------------|:----------|----------:|:-------------------|:--------------------|:------------|:--------------|:------------------------------|:--------------|:---------------------|:---------|:--------------|:--------------|:-----------------------------------------------------|:---------------------|:-------------------|:-------------|:-------|:------------|
| White House          | 1600 Pennsylvania Ave NW, Washington, DC   | 38.89770 |  -77.03655 | White House, 1600, Pennsylvania Avenue Northwest, Washington, District of Columbia, 20500, United States                                         | 147370893 | Data © OpenStreetMap contributors, ODbL 1.0. <https://osm.org/copyright> | way       | 238241022 | 38.897699700000004 | -77.03655315        | White House | 1600          | Pennsylvania Avenue Northwest | Washington    | District of Columbia | 20500    | United States | us            | 38.8974908 , 38.897911 , -77.0368537, -77.0362519    | NA                   | NA                 | NA           | NA     | NA          |
| Transamerica Pyramid | 600 Montgomery St, San Francisco, CA 94111 | 37.79520 | -122.40279 | Transamerica Pyramid, 600, Montgomery Street, Financial District, San Francisco, San Francisco City and County, California, 94111, United States |  95364489 | Data © OpenStreetMap contributors, ODbL 1.0. <https://osm.org/copyright> | way       |  24222973 | 37.795200550000004 | -122.40279267840137 | NA          | 600           | Montgomery Street             | San Francisco | California           | 94111    | United States | us            | 37.7948854 , 37.7954472 , -122.4031399, -122.4024317 | Transamerica Pyramid | Financial District | NA           | NA     | NA          |
| Willis Tower         | 233 S Wacker Dr, Chicago, IL 60606         | 41.87887 |  -87.63591 | Willis Tower, 233, South Wacker Drive, Printer’s Row, Loop, Chicago, Cook County, Illinois, 60606, United States                                 | 103673983 | Data © OpenStreetMap contributors, ODbL 1.0. <https://osm.org/copyright> | way       |  58528804 | 41.878871700000005 | -87.63590893936448  | NA          | 233           | South Wacker Drive            | Chicago       | Illinois             | 60606    | United States | us            | 41.8785389 , 41.8791932 , -87.6363362, -87.6354746   | NA                   | Printer’s Row      | Willis Tower | Loop   | Cook County |

For further documentation, refer to the [Getting Started
Vignette](https://jessecambon.github.io/tidygeocoder/articles/tidygeocoder.html)
and the [function
documentation](https://jessecambon.github.io/tidygeocoder/reference/index.html).

## Contributing

Contributions to the tidygeocoder package are welcome. File [an
issue](https://github.com/jessecambon/tidygeocoder/issues) for bug fixes
or suggested features. If you would like to add support for a new
geocoder service, reference [the developer
notes](https://jessecambon.github.io/tidygeocoder/articles/developer_notes.html)
for instructions.

## Citing tidygeocoder

``` r
citation('tidygeocoder')
```

</br>

<blockquote>


    To cite tidygeocoder in publications use:

      Jesse Cambon, Christopher Belanger (2021). tidygeocoder: Geocoding
      Made Easy (version 1.0.2). DOI: 10.5281/zenodo.4448251. URL:
      https://CRAN.R-project.org/package=tidygeocoder

    A BibTeX entry for LaTeX users is

      @Misc{,
        title = {tidygeocoder: Geocoding Made Easy},
        author = {Jesse Cambon and Christopher Belanger},
        year = {2021},
        publisher = {Zenodo},
        note = {R package version 1.0.2},
        url = {https://CRAN.R-project.org/package=tidygeocoder},
        doi = {10.5281/zenodo.4448251},
      }

</blockquote>

Or refer to the [citation
page](https://jessecambon.github.io/tidygeocoder/authors.html).
