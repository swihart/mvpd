# mvpd R package
Bruce Swihart  
JUN 2022

## Submission 1

* Improved help pages for fit_mvss and rmvss
* Added more tests

## Test environments
Local OS X: R version 4.1.2 (2021-11-01)
  * Platform: x86_64-apple-darwin17.0 (64-bit)
  * Running under: macOS Mojave 10.14.6
  
rhub::check(platform = "debian-gcc-devel"): Debian Linux, R-devel, GCC
rhub::check(platform = "windows-x86_64-devel"): Windows Server 2022, R-devel, 64 bit

devtools::check_win_devel(): https://win-builder.r-project.org/k34qu7C45g6N

M1: https://mac.r-project.org/macbuilder/results/1655754160-88ed4dd9ad5ffc27/

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.


## Downstream dependencies
There are currently no downstream dependencies for this package.

# mvpd R package
Bruce Swihart  
MAR 2022

## Submission 1

  * Add subgaussian stable capabilities
  
## Test environments
Local OS X: R version 4.1.2 (2021-11-01)
  * Platform: x86_64-apple-darwin17.0 (64-bit)
  * Running under: macOS Mojave 10.14.6
  
rhub::check(platform = "debian-gcc-devel"): Debian Linux, R-devel, GCC
rhub::check(platform = "windows-x86_64-devel"): Windows Server 2022, R-devel, 64 bit

devtools::check_win_devel(): https://win-builder.r-project.org/e3lsr7vF72Ee/00check.log

M1: https://mac.r-project.org/macbuilder/results/1648663324-6f43f8e486a28f28/

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.


## Downstream dependencies
There are currently no downstream dependencies for this package.



# mvpd R package
Bruce Swihart  
Mar 2022

## Re-Submission 1

  * remove vestigal vignette builder line from DESCRIPTION
  * Initial submission to CRAN
  * This package is similar to one I just sent to CRAN (mvgb).  However,
  mvpd will be distinct methodology and have a more permissive license, fewer dependencies, etc.

## Test environments
Local OS X: R version 4.1.2 (2021-11-01)
  * Platform: x86_64-apple-darwin17.0 (64-bit)
  * Running under: macOS Mojave 10.14.6
  
rhub::check(platform = "debian-gcc-devel"): Debian Linux, R-devel, GCC
rhub::check(platform = "windows-x86_64-devel"): Windows Server 2022, R-devel, 64 bit

devtools::check_win_devel()

M1: https://mac.r-project.org/macbuilder/results/1646799691-c733d3110d3181d9/

## R CMD check results
There were no ERRORs or WARNINGs. 1 NOTE due to New submission to CRAN.


## Downstream dependencies
There are currently no downstream dependencies for this package.
