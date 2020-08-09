## Test environments
* local Ubuntu install, R 4.0.2
* winbuilder r-old devel: `devtools::check_win_oldrelease()`
* winbuilder r-devel : `devtools::check_win_devel()`
* Other environments checked via `rhub::check_for_cran(env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`
```

## R CMD check results

0 errors | 0 warnings | 0 notes
