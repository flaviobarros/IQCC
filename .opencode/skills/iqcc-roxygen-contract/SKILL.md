# IQCC Roxygen Documentation Contract Skill

Generate standardized roxygen documentation for exported functions following
the IQCC package conventions. Every public function must have a complete
contract covering inputs, outputs, edge cases, and scientific references.

## Required sections (in this order)

### 1. Title (first line)
Short, starts with a verb: "Compute", "Build", "Extract", "Plot", "Print".

```
#' Compute Probability Limits for p Charts
#' Build a Range Control Chart
```

### 2. Description (paragraph)
One or two sentences. State what the function does *without* a plot (for pure
numerical functions) or that it draws the chart (for wrappers).

```
#' Compute normal or Cornish-Fisher probability limits for a p chart without
#' constructing a plot.
```

### 3. @param — every parameter

Document for each parameter:
- Type, dimension, constraints (scalar/vector, integer/real, positive, etc.)
- What happens with `NA`, `NaN`, `Inf`
- Default value and its meaning
- Phase role when applicable (Phase I reference vs Phase II monitoring)

Typical validation rules:
```r
if(!is.numeric(sigma) || length(sigma) != 1 || !is.finite(sigma) || sigma <= 0)
    stop("sigma must be a finite positive scalar")
if(length(n) != 1 || !is.numeric(n) || !is.finite(n) || n < 2 || n != floor(n))
    stop("n must be an integer greater than or equal to 2")
```

### 4. @return — named list structure

Document every component of the returned list with `\describe`:

```
#' @return A named list with components:
#' \describe{
#'   \item{lcl}{Lower control limit.}
#'   \item{ucl}{Upper control limit.}
#'   \item{center}{The expected value under normality.}
#' }
```

For plotting wrappers: `@return Invisibly, the "qcc" object [...] The function also draws the chart.`

### 5. @details — formulas

Use `\deqn{}{}` for LaTeX formulas. Keep equations simple and reference the
article for derivations.

```
#' \deqn{LCL = \sigma \, F_W^{-1}(\alpha/2; n)}
```

### 6. @section — Phase convention (when applicable)

Document Phase I vs Phase II roles explicitly:

```
#' @section Phase convention:
#' The exact chart treats \code{y} as Phase I reference data and \code{x} as
#' the plotted monitoring data. [...] Plug-in limits do not account for
#' Phase I sampling variability.
```

### 7. @section — Decision convention / rule (when applicable)

State the inequality that triggers a signal:

```
#' @section Decision rule:
#' A subgroup statistic signals out of control when \eqn{stat < LCL} or
#' \eqn{stat > UCL}. Equality to a limit is treated as in control.
```

### 8. @section — Errors (when applicable)

Document error conditions explicitly:

```
#' @section Errors:
#' An error is raised for an unsupported \code{type}, for \code{n < 2},
#' or when \code{y} is omitted for the exact chart.
```

### 9. @references — with DOI

```
#' @references
#' Barbosa, E. P., Gneri, M. A., and Meneguetti, A. (2013). Range control
#' charts revisited. \emph{Communications in Statistics - Simulation and
#' Computation}, 42(2), 247--262. \doi{10.1080/03610918.2011.639967}.
```

### 10. @seealso — cross-references

Link related pure functions, wrappers, and constants:

```
#' @seealso \code{\link{r_shewhart_limits}}, \code{\link{cchart.R}},
#'   \code{\link{alpha.risk}}, \code{\link{d2}}
```

### 11. @export, @author, @importFrom

```
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @importFrom stats qtukey
#' @importFrom qcc qcc sd.R
```

Use `@importFrom graphics` for base-R plotting functions used in wrappers.

### 12. @examples — executable

```r
#' @examples
#' data(pistonrings)
#' r_exact_limits(sigma = 2, n = 5)
#' r_shewhart_limits(sigma = 1, n = 10, nsigmas = 2)
```

- Must be runnable without user interaction
- Use `\donttest{}` only for simulation-heavy or computationally expensive
  examples

## Pattern reference

See these files for complete examples of the IQCC roxygen style:

| Pattern | File |
|---------|------|
| Pure numerical limits | `R/pchart_limits.R`, `R/gv_limits.R`, `R/r_limits.R` |
| Plotting wrapper | `R/cchart.R.R`, `R/cchart.GV.R` |
| Risk function | `R/alpha.risk.R`, `R/gv_alpha_risk.R` |
| Statistics | `R/gv_stat.R`, `R/trv.R` |

## Keeping man/ in sync

Always regenerate after editing roxygen:

```bash
Rscript -e 'devtools::document()'
```

Inspect `git diff` on `.Rd` files to verify only intended changes are present.
If unrelated roxygen churn appears (e.g., from a different R/package version),
restore the affected `.Rd` files before committing.
