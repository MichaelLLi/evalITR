## Test environments
* local Windows 10 install, R 3.6.3
* local Windows 10 install, R 3.5.0
* win-builder (devel and release)
* Mac OS X 10.11 (on Rhub), R-release
* Ubuntu 16.04 (on travis-ci), R-release

## R CMD check results
There were no ERRORs or WARNINGs. The only note relates to "unable to verify current time", which is caused by the external API that base R calls to verify system time, and not an error that exists in this package. 


## Downstream dependencies