test_that("wheat phenology", {

    met <- system.file("extdata/weather.met", package = "APSIMWheatPhenology")
    sim <- system.file("extdata/example.sim", package = "APSIMWheatPhenology")
    # Flowering time using parameter in the sim file
    df <- wheatPhenology(sim, stages = 6, stageNames = 'flow', model = 'APSIM',
                         met = met, daily = FALSE)
    expect_equal(nrow(df), 20)
    expect_equal(df$flow_das[1], 84)

    # Models V1 or V2 can be used
    df <- wheatPhenology(sim, stages = 6, stageNames = 'flow', model = 'V1',
                         met = met, daily = FALSE)
    expect_equal(nrow(df), 20)
    expect_equal(df$flow_das[1], 114)

    # Weather can be passed as a weaana object
    met_data <- weaana::readWeatherRecords(met)
    df <- wheatPhenology(sim, stages = 6, stageNames = 'flow', model = 'APSIM',
                         met = met_data, daily = FALSE)
    expect_equal(nrow(df), 20)
    expect_equal(df$flow_das[1], 84)

    # Multiple stages can be calculated
    df <- wheatPhenology(sim, stages = c(5.74, 6), stageNames = c("head", 'flow'),
                         model = 'APSIM',
                         met = met_data, daily = FALSE)
    expect_equal(nrow(df), 20)
    expect_equal(df$head_das[1], 74)

    # Daily output can be exported
    df <- wheatPhenology(sim, stages = c(5.74, 6), stageNames = c("head", 'flow'),
                         model = 'APSIM',
                         met = met_data, daily = TRUE)
    expect_equal(nrow(df), 2651)
    expect_equal(df$stage[1], 1.999)


    # Multiple simulations can be specified in a factor
    # Any parameters related with plant can be used
    factors <- expand.grid(list(vern_sens = seq(1, 5), photop_sens = seq(1, 5)))
    df <- wheatPhenology(sim, stages = c(5.74, 6), stageNames = c("head", 'flow'),
                         model = 'APSIM',
                         met = met_data, daily = FALSE,
                         factors = factors)
    expect_equal(nrow(df), 500)
    expect_equal(df$head_das[1], 66)
    expect_equal(unique(df$vern_sens), seq(1, 5))
    expect_equal(unique(df$photop_sens), seq(1, 5))
})
