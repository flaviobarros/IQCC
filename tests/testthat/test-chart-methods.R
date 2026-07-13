test_that("generalized variance chart methods expose a stable summary", {
    set.seed(123)
    x <- array(rnorm(5 * 6 * 2), dim = c(5, 6, 2))
    chart <- cchart.GV(x, Sigma = diag(2), type = "normal", plot = FALSE)

    s <- summary(chart)
    expect_s3_class(s, "summary.cchart.GV")
    expect_equal(s$dimensions, c(n = 6, p = 2))
    expect_equal(s$subgroups[["phase1"]], 5)
    expect_true(all(c("lcl", "center", "ucl") %in% names(s$limits)))

    expect_output(returned_chart <- print(chart),
                  "Generalized Variance Control Chart")
    expect_output(returned_summary <- print(s),
                  "Generalized Variance Control Chart")
    expect_identical(returned_chart, chart)
    expect_identical(returned_summary, s)
})

test_that("DS-np chart methods summarize stages and signals", {
    x1 <- c(0, 1, 2, 3, 1, 0, 2, 4, 1, 0)
    x2 <- c(NA, NA, 2, NA, NA, NA, 3, NA, NA, NA)
    chart <- cchart.DSnp(
        x1,
        n1 = 10,
        n2 = 20,
        p0 = 0.05,
        x2 = x2,
        wl = 1.5,
        ucl1 = 2.5,
        ucl2 = 4.5,
        plot = FALSE
    )

    s <- summary(chart)
    expect_s3_class(s, "summary.cchart.DSnp")
    expect_equal(sum(s$stage_counts), length(x1))
    expect_equal(nrow(s$signals), sum(chart$data$signal))
    expect_true(all(c("arl0", "ass0", "p_signal0") %in%
                    names(s$performance)))

    expect_output(returned_chart <- print(chart),
                  "Double-Sampling np Control Chart")
    expect_output(returned_summary <- print(s),
                  "Double-Sampling np Control Chart")
    expect_identical(returned_chart, chart)
    expect_identical(returned_summary, s)
})
