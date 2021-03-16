# APSIM Wheat Phenology

![R-CMD-check](https://github.com/APSIMInitiative/APSIMWheatPhenology/workflows/R-CMD-check/badge.svg)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/APSIMWheatPhenology)](https://cran.r-project.org/package=APSIMWheatPhenology)



[APSIMWheatPhenology](https://apsiminitiative.github.io/APSIMWheatPhenology/) package rewrites [APSIM ](https://www.apsim.info/) wheat phenology model using R to speed up the performance for multiple simulations.


## Installation

Currently on [Github](https://github.com/APSIMInitiative/APSIMWheatPhenology) only. Install with:

```r
remotes::install_github('APSIMInitiative/APSIMWheatPhenology')
```

## Wheat phenology

Wheat phenology can be calculated in multiple ways.

```r
met <- system.file("extdata/weather.met", package = "APSIMWheatPhenology")
sim <- system.file("extdata/example.sim", package = "APSIMWheatPhenology")
# Flowering time using parameter in the sim file
df <- wheatPhenology(sim, stages = 6, stageNames = 'flow', model = 'APSIM',
                     met = met, daily = FALSE)
# Models V1 or V2 can be used
df <- wheatPhenology(sim, stages = 6, stageNames = 'flow', model = 'V1',
                     met = met, daily = FALSE)

# Weather can be passed as a weaana object
met_data <- weaana::readWeatherRecords(met)
df <- wheatPhenology(sim, stages = 6, stageNames = 'flow', model = 'APSIM',
                     met = met_data, daily = FALSE)

# Multiple stages can be calculated
df <- wheatPhenology(sim, stages = c(5.74, 6), stageNames = c("head", 'flow'),
                     model = 'APSIM',
                     met = met_data, daily = FALSE)

# Daily output can be exported
df <- wheatPhenology(sim, stages = c(5.74, 6), stageNames = c("head", 'flow'),
                     model = 'APSIM',
                     met = met_data, daily = TRUE)

# Multiple simulations can be specified in a factor
# Any parameters related with plant can be used
factors <- expand.grid(list(vern_sens = seq(1, 5), photop_sens = seq(1, 5)))
df <- wheatPhenology(sim, stages = c(5.74, 6), stageNames = c("head", 'flow'),
                     model = 'APSIM',
                     met = met_data, daily = FALSE,
                     factors = factors)

```
