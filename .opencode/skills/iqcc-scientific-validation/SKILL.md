# IQCC Scientific Validation Test Fixture Skill

Scaffold test fixtures for reproducing published tables/figures from the
statistical control chart literature. Follow the convention established in
`tests/testthat/test-range-paper.R`.

## When to use

- Reproducing a published table, figure, equation, or row of numerical results
  from a paper in `sources/papers/`.
- Adding a numerical cross-check against a published source for any `*_limits`,
  `*_alpha_risk`, `*_arl`, `*_ass`, `*_stat` function.
- Any test tagged as "scientific" or "validation" that must record provenance.

## Fixture record contract

Each published value tested MUST include these fields, either as columns in a
`data.frame` or as explicit arguments to an expectation:

| Field | Description | Example |
|-------|-------------|---------|
| `reference` | Full citation + DOI | `"Barbosa, Gneri, Meneguetti (2013), doi:10.1080/03610918.2011.639967"` |
| `table` | Table/figure/equation identifier | `"Table 2"` |
| `row` | Row within the source | `"n = 5"` |
| `parameters` | Input parameters used | `n = 5, sigma = 1, alpha = 0.0027` |
| `published_value` | Value from the source | `0.39653` |
| `calculated_value` | Value from the implementation | `0.3965281` |
| `tolerance` | Half-width of the acceptable interval | `5e-5` |
| `tolerance_ratio` | `abs(calculated - published) / tolerance` | `0.038` |
| `tolerance_rationale` | Why this tolerance is correct | `"Table 2 prints 5 decimal places; half a unit in the last place"` |

## Scaffolding a fixture data.frame

Use this pattern when testing multiple values with shared metadata:

```r
reference <- "Full citation, doi:..."
fixtures <- expand.grid(
  n = c(2L, 5L, 10L),
  parameter2 = c(...),
  KEEP.OUT.ATTRS = FALSE
)
fixtures <- fixtures[order(fixtures$n), ]
fixtures$reference <- reference
fixtures$table <- "Table X"
fixtures$row <- paste0("n = ", fixtures$n)
fixtures$published_value <- c(...)  # in the same order as the grid
fixtures$calculated_value <- mapply(function(n, ...) {
    # function call here
}, fixtures$n, fixtures$parameter2)
fixtures$tolerance <- 5e-6  # half-unit rounding
fixtures$tolerance_ratio <- abs(
  fixtures$calculated_value - fixtures$published_value
) / fixtures$tolerance
```

## Expectation loop

Always iterate over fixture rows with a provenance string:

```r
for (i in seq_len(nrow(fixtures))) {
  provenance <- sprintf(
    "%s; %s; %s; params = (%s); published = %.5f; calculated = %.10f; tolerance = %.1e; ratio = %.3f",
    fixtures$reference[i], fixtures$table[i], fixtures$row[i],
    paste of parameters,
    fixtures$published_value[i], fixtures$calculated_value[i],
    fixtures$tolerance[i], fixtures$tolerance_ratio[i]
  )
  expect_true(fixtures$tolerance_ratio[i] <= 1, info = provenance)
}
```

## Tolerance rules

- **Published rounding**: If the source prints `k` decimal places, tolerance is
  `0.5 * 10^(-k)`. Always state this as the rationale.
- **Propagated rounding**: For tests that transform a published rounded value
  (e.g., integrate a CDF at a published quantile), propagate the rounding
  interval through the transformation to obtain the tolerance — do not pick an
  arbitrary small number.
- **Monte Carlo error**: When comparing against simulation, set tolerance to
  `z_{alpha/2} * SE` and report the standard error explicitly.
- **Do not widen tolerances** to make a test pass. If a value does not
  reproduce, classify the result as partial or qualitative.

## Test naming convention

```r
test_that("topic reproduces Source, Table X, row n", {
test_that("topic satisfies an independent Source CDF derivation", {
```

## Independent derivation

When validating a published result through an independent mathematical path
(e.g., Tippett CDF integration to verify Tukey quantiles), scaffold a second
fixture block with its own reference string that states the relationship:

```r
reference <- paste(
  "Independent Tippett CDF verification of",
  "Barbosa et al. (2013), Table 2"
)
```

See `test-range-paper.R` for a complete example of this pattern.
