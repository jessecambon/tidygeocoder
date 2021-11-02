## Test environments
* local Ubuntu install, R 4.1.0
* GitHub Actions [R-CMD-check](https://github.com/jessecambon/tidygeocoder/blob/main/.github/workflows/check-full.yaml)
* winbuilder r-old devel: `devtools::check_win_oldrelease()`
* winbuilder r-devel : `devtools::check_win_devel()`
* Other environments checked via `rhub::check_for_cran()`

## R CMD check results

0 errors | 0 warnings | 1 notes

## Notes 

- This release was required to prevent the package from being removed from CRAN due to CRAN check issues that occured without network connectivity.
- RE the inactive Zenodo URL, this DOI is reserved and will become active once released.