## Test environments
* local Pop! OS 22.04 install, R 4.4.3
* GitHub Actions [R-CMD-check](https://github.com/jessecambon/tidygeocoder/blob/main/.github/workflows/check-standard.yaml)
* GitHub Actions [R-hub](https://github.com/jessecambon/tidygeocoder/blob/main/.github/workflows/rhub.yaml)
* winbuilder r-old devel: `devtools::check_win_oldrelease()`
* winbuilder r-devel : `devtools::check_win_devel()`
* Linux and Mac OS checked via `rhub::rhub_check()`