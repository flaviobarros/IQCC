test_that("relative-range quantiles reproduce Barbosa et al. Table 2", {
  reference <- paste(
    "Barbosa, Gneri, and Meneguetti (2013),",
    "Range Control Charts Revisited, doi:10.1080/03610918.2011.639967"
  )
  probabilities <- c(0.0010, 0.00135, 0.9980, 0.9973, 0.9990, 0.99865)
  published <- rbind(
    `2` = c(0.00177, 0.00239, 4.37025, 4.24261, 4.65351, 4.53274),
    `5` = c(0.36739, 0.39653, 5.23478, 5.12314, 5.48375, 5.37740),
    `10` = c(1.08458, 1.12634, 5.74143, 5.63772, 5.97331, 5.87416)
  )

  fixtures <- expand.grid(
    probability_index = seq_along(probabilities),
    n = c(2L, 5L, 10L),
    KEEP.OUT.ATTRS = FALSE
  )
  fixtures <- fixtures[order(fixtures$n, fixtures$probability_index), ]
  fixtures$reference <- reference
  fixtures$table <- "Table 2"
  fixtures$row <- paste0("n = ", fixtures$n)
  fixtures$probability <- probabilities[fixtures$probability_index]
  fixtures$published_value <- mapply(
    function(n, probability_index) {
      published[as.character(n), probability_index]
    },
    fixtures$n,
    fixtures$probability_index
  )
  fixtures$calculated_value <- mapply(
    stats::qtukey,
    p = fixtures$probability,
    nmeans = fixtures$n,
    MoreArgs = list(df = Inf)
  )
  # Table 2 prints five decimal places, so half a unit in the last place is the
  # strict rounding tolerance. The ratio must remain at most one for every cell.
  fixtures$tolerance <- 5e-6
  fixtures$tolerance_ratio <- abs(
    fixtures$calculated_value - fixtures$published_value
  ) / fixtures$tolerance

  for (i in seq_len(nrow(fixtures))) {
    provenance <- sprintf(
      "%s; %s; %s; p = %.5f; published = %.5f; calculated = %.10f; tolerance = %.1e; ratio = %.3f",
      fixtures$reference[i], fixtures$table[i], fixtures$row[i],
      fixtures$probability[i], fixtures$published_value[i],
      fixtures$calculated_value[i], fixtures$tolerance[i],
      fixtures$tolerance_ratio[i]
    )
    expect_true(fixtures$tolerance_ratio[i] <= 1, info = provenance)
  }

  invisible(capture.output(european <- table.qtukey(alpha = 0.0020, n = 10)))
  invisible(capture.output(us <- table.qtukey(alpha = 0.0027, n = 10)))
  package_values <- cbind(
    european[c("2", "5", "10"), "alpha/2"],
    us[c("2", "5", "10"), "alpha/2"],
    european[c("2", "5", "10"), "1-alpha"],
    us[c("2", "5", "10"), "1-alpha"],
    european[c("2", "5", "10"), "1-alpha/2"],
    us[c("2", "5", "10"), "1-alpha/2"]
  )

  expect_equal(as.vector(t(package_values)), fixtures$calculated_value,
               tolerance = 1e-12)
})

test_that("published relative-range rows satisfy an independent Tippett CDF", {
  reference <- paste(
    "Barbosa, Gneri, and Meneguetti (2013),",
    "Range Control Charts Revisited, Table 2"
  )
  fixtures <- data.frame(
    reference = reference,
    table = "Table 2",
    row = c("n = 2", "n = 5", "n = 10"),
    n = c(2L, 5L, 10L),
    probability = c(0.99865, 0.0010, 0.9980),
    published_value = c(4.53274, 0.36739, 5.74143)
  )

  relative_range_cdf <- function(w, n) {
    stats::integrate(
      function(x) {
        n * stats::dnorm(x) *
          (stats::pnorm(x + w) - stats::pnorm(x)) ^ (n - 1)
      },
      lower = -Inf,
      upper = Inf,
      rel.tol = 1e-10,
      subdivisions = 1000L
    )$value
  }

  fixtures$calculated_value <- mapply(
    relative_range_cdf,
    fixtures$published_value,
    fixtures$n
  )
  # Propagate the source's five-decimal rounding interval through the
  # independently integrated CDF rather than choosing an arbitrary tolerance.
  rounding_half_width <- 5e-6
  lower_probability <- mapply(
    relative_range_cdf,
    fixtures$published_value - rounding_half_width,
    fixtures$n
  )
  upper_probability <- mapply(
    relative_range_cdf,
    fixtures$published_value + rounding_half_width,
    fixtures$n
  )
  fixtures$tolerance <- pmax(
    abs(lower_probability - fixtures$probability),
    abs(upper_probability - fixtures$probability)
  )
  fixtures$tolerance_ratio <- abs(
    fixtures$calculated_value - fixtures$probability
  ) / fixtures$tolerance

  for (i in seq_len(nrow(fixtures))) {
    provenance <- sprintf(
      "%s; %s; %s; p = %.5f; published quantile = %.5f; integrated probability = %.10f; tolerance = %.2e; ratio = %.3f",
      fixtures$reference[i], fixtures$table[i], fixtures$row[i],
      fixtures$probability[i], fixtures$published_value[i],
      fixtures$calculated_value[i], fixtures$tolerance[i],
      fixtures$tolerance_ratio[i]
    )
    expect_true(fixtures$tolerance_ratio[i] <= 1, info = provenance)
  }
})
