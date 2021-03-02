test_that("read para", {
    example_file <- system.file("extdata/example.sim", package = "APSIMWheatPhenology")
    a <- readPara(NULL, sim = example_file)
    expect_null(a)
    sowing_date <- readPara(name = "date")
    expect_equal(sowing_date, "16-apr")
})
