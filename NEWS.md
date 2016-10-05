# ddpcr 1.5.xxxx
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
