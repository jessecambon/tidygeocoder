## Test environments
* local Ubuntu install, R 4.0.5
* GitHub Actions [R-CMD-check](https://github.com/jessecambon/tidygeocoder/blob/main/.github/workflows/check-full.yaml)
* winbuilder r-old devel: `devtools::check_win_oldrelease()`
* winbuilder r-devel : `devtools::check_win_devel()`
* Other environments checked via `rhub::check_for_cran(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`

## R CMD check results

0 errors | 0 warnings | 1 notes

## Notes 

- RE the NOTE, the DOI is reserved and will become active once released.