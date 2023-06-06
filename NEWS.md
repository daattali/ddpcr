# ddpcr 1.15.1 2023-04-01

- Small maintenance work to keep the package on CRAN

# ddpcr 1.15 2020-06-01

- More changes to make dplyr v1.0.0 work

# ddpcr 1.14 2020-03-20

- Make ddpcr work with dplyr v1.0.0

# ddpcr 1.13 2020-02-28

- Prepare for R 4.0.0 by adding `stringsAsFactors = TRUE` where needed

# ddpcr 1.12 2020-01-12

- Make code work with new `tidyselect` and fix CRAN errors/warnings

# ddpcr 1.11 2019-01-03

- Fix error-prone code that did not properly vectorize if statements

# ddpcr 1.10 2018-11-26

- update to work with new `readr` version

# ddpcr 1.9 2018-05-27

- updates to work with new `testthat` package, new `dplyr` package, and new CRAN policies

# ddpcr 1.8 2017-05-19

- refactor code to work with new versions of `dplyr` and `ggplot2`

# ddpcr 1.7 2016-12-08

- add support for targets for channel 1 and 2, in newer QuantaSoft export formats (v1.7.4) (thanks @acnb)
- some improvements to Shiny UI

# ddpcr 1.6 2016-11-11

- fix all ggplot2 code to work with new version ggplot2 2.2.0
- add reference to f1000 paper in shiny app About section
- suppress messages from readr
- upgrade to DT v0.2

# ddpcr 1.5 2016-08-03

- better error message when trying to read directory that doesn't have any data files (#10)
- changed default droplet volume to 85 nL based on literature (@seaaan) (#12)
- change one unit test from `expect_identical` to `expect_equal` to reflect a change in `readr` (#13)

# ddpcr 1.4 2016-06-24

Small internal changes to reflect `dplyr` update

# ddpcr 1.3 2016-06-03

Prepare for re-release to CRAN and to F1000Research

# ddpcr 1.2.0

2016-03-22

- Added a 'dirty' bit to a plate, to keep track of whether any changes to the settings have been made that require a re-run of the analysis.  Both the shiny app and the command line inform the user if they are using a dirty plate object

- A few bug fixes in the shiny app (don't show in Advanced Settings section the settings that are already shown in Basic Settings, don't show Advanced Settings parameters of other plate types)
