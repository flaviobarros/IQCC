#' Double-Sampling np Chart: Complete Design Search
#'
#' Perform a joint discrete search over first-stage sample size (n1),
#' second-stage sample size (n2), and DS-np control limits to find
#' optimal chart designs. This function performs an exhaustive search
#' over the Cartesian product of \code{n1_range} and \code{n2_range},
#' calling \code{\link{dsnp_limits}} for each pair and collecting the
#' best feasible candidates across all pairs.
#'
#' The search is discrete and exhaustive within the supplied ranges; it
#' is not a continuous optimization. The cost grows with the number of
#' (n1, n2) pairs and the number of limit candidates evaluated per pair.
#' This function does not implement curtailed inspection.
#'
#' When \code{alpha} is \code{NULL} and \code{arl0_min} is provided, an
#' effective alpha of \code{1 / arl0_min} is used to guide
#' \code{dsnp_limits()}, but final feasibility is checked against the
#' explicit \code{arl0 >= arl0_min} condition.
#'
#' @param p0 In-control nonconforming proportion. Scalar in (0, 1).
#' @param p1 Out-of-control nonconforming proportion. Scalar in (0, 1).
#'   Must be greater than \code{p0}.
#' @param n1_range Integer vector of first-stage sample sizes to evaluate.
#'   Positive integers, no duplicates, no NAs.
#' @param n2_range Integer vector of second-stage sample sizes to evaluate.
#'   Positive integers, no duplicates, no NAs.
#' @param arl0_min Minimum acceptable in-control ARL. Scalar greater than 1,
#'   or \code{NULL} if \code{alpha} is supplied.
#' @param alpha Maximum acceptable false alarm probability. Scalar in (0, 1),
#'   or \code{NULL} if \code{arl0_min} is supplied. When both are provided,
#'   candidates must satisfy both constraints.
#' @param objective Optimization objective. One of \code{"arl1"} (minimize
#'   out-of-control ARL), \code{"ass0"} (minimize in-control ASS), or
#'   \code{"weighted"} (minimize a weighted combination of normalized ARL1
#'   and ASS0).
#' @param weights Named numeric vector with elements \code{arl1} and
#'   \code{ass0}. Both must be non-negative and at least one must be
#'   positive. Only used when \code{objective = "weighted"}.
#' @param allow_empty_warning Logical. If \code{FALSE} (default), discard
#'   candidates with no integer in the warning zone.
#' @param max_results Maximum number of feasible candidates to return.
#'   Positive integer.
#' @param progress Logical. If \code{TRUE}, print progress messages during
#'   the search.
#' @return An object of class \code{"dsnp_design"} with the following
#' elements:
#' \describe{
#'   \item{best}{The top-ranked feasible candidate (a one-row data.frame).}
#'   \item{candidates}{Up to \code{max_results} feasible candidates, sorted
#'   by the chosen objective.}
#'   \item{parameters}{A list of the input parameters.}
#'   \item{search}{A list with search summary counts:}
#'   \describe{
#'     \item{n_pairs_evaluated}{Number of (n1, n2) pairs evaluated.}
#'     \item{n_candidates_evaluated}{Total number of limit candidates
#'       evaluated across all pairs.}
#'     \item{n_feasible}{Number of candidates satisfying global feasibility.}
#'     \item{n_failed_pairs}{Number of (n1, n2) pairs where
#'       \code{dsnp_limits()} failed.}
#'   }
#'   \item{failures}{A data.frame of failed pairs with columns n1, n2,
#'     and message. Empty if no failures occurred.}
#' }
#' @export
#' @author Daniela R. Recchia, Emanuel P. Barbosa
#' @seealso \link{dsnp_limits}, \link{dsnp_prob_accept}, \link{dsnp_arl},
#'   \link{dsnp_ass}
#' @references Joekes, S., Smrekar, M. and Barbosa, E. (2015). Extending a
#' double sampling control chart for non-conforming proportion in high quality
#' processes to the case of small samples.
#' @examples
#'
#' # Small example for fast execution
#' res <- dsnp_design(
#'     p0 = 0.05, p1 = 0.10,
#'     n1_range = 5:6, n2_range = 8:10,
#'     arl0_min = 50, objective = "arl1"
#' )
#' res$best[, c("n1", "n2", "wl", "ucl1", "ucl2", "arl0", "arl1")]
#' @importFrom utils head

# Internal helper: min-max scale a vector that should be minimized.
# +Inf values (worst case) receive scale 1.  NA, NaN, and -Inf are rejected.
.scale_minimize <- function(x)
{
    if(any(is.na(x) | is.nan(x) | x == -Inf))
        stop("objective values contain invalid non-finite values")

    finite <- is.finite(x)
    out <- rep(1, length(x))

    if(!any(finite))
        return(out)

    xmin <- min(x[finite])
    xmax <- max(x[finite])

    if(xmax == xmin)
        out[finite] <- 0
    else
        out[finite] <- (x[finite] - xmin) / (xmax - xmin)

    out
}

dsnp_design <- function(
    p0,
    p1,
    n1_range,
    n2_range,
    arl0_min = 200,
    alpha = NULL,
    objective = c("arl1", "ass0", "weighted"),
    weights = c(arl1 = 1, ass0 = 1),
    allow_empty_warning = FALSE,
    max_results = 20,
    progress = FALSE
)
{
    # --- Validate p0 and p1 ---
    if(length(p0) != 1 || !is.finite(p0) || p0 <= 0 || p0 >= 1)
        stop("p0 must be a finite scalar in (0, 1)")
    if(length(p1) != 1 || !is.finite(p1) || p1 <= 0 || p1 >= 1)
        stop("p1 must be a finite scalar in (0, 1)")
    if(p1 <= p0)
        stop("p1 must be greater than p0")

    # --- Validate objective ---
    objective <- match.arg(objective)

    # --- Validate weights ---
    if(!is.numeric(weights) || length(weights) != 2)
        stop("weights must be a numeric vector of length 2")
    if(!all(c("arl1", "ass0") %in% names(weights)))
        stop("weights must have names 'arl1' and 'ass0'")
    if(any(!is.finite(weights)) || any(weights < 0))
        stop("weights must be finite and non-negative")
    if(all(weights == 0))
        stop("at least one weight must be positive")

    # --- Validate arl0_min and alpha ---
    if(is.null(arl0_min) && is.null(alpha))
        stop("at least one of arl0_min or alpha must be provided")

    if(!is.null(arl0_min))
    {
        if(length(arl0_min) != 1 || !is.finite(arl0_min) || arl0_min <= 1)
            stop("arl0_min must be a finite scalar greater than 1")
    }

    if(!is.null(alpha))
    {
        if(length(alpha) != 1 || !is.finite(alpha) || alpha <= 0 || alpha >= 1)
            stop("alpha must be NULL or a finite scalar in (0, 1)")
    }

    # --- Validate n1_range and n2_range ---
    validate_range <- function(x, name)
    {
        if(length(x) == 0)
            stop(name, " must be a non-empty vector")
        if(any(!is.finite(x)) || any(x != floor(x)) || any(x < 1))
            stop(name, " must contain positive integers with no NA, NaN, or Inf")
        unique(sort(x))
    }

    n1_range <- validate_range(n1_range, "n1_range")
    n2_range <- validate_range(n2_range, "n2_range")

    # --- Validate max_results and progress ---
    if(length(max_results) != 1 || !is.finite(max_results) ||
       max_results != floor(max_results) || max_results < 1)
        stop("max_results must be a positive integer")
    if(length(progress) != 1 || !is.logical(progress) || is.na(progress))
        stop("progress must be a logical scalar (TRUE or FALSE)")

    # --- Determine effective alpha for dsnp_limits ---
    alpha_for_limits <- if(!is.null(alpha)) alpha else 1 / arl0_min

    # --- Search ---
    all_results <- list()
    failures <- data.frame(
        n1 = integer(0),
        n2 = integer(0),
        message = character(0),
        stringsAsFactors = FALSE
    )

    n_pairs <- length(n1_range) * length(n2_range)
    pair_idx <- 0
    n_failed <- 0L

    for(n1 in n1_range)
    {
        for(n2 in n2_range)
        {
            pair_idx <- pair_idx + 1L

            if(progress)
                message("Evaluating pair ", pair_idx, "/", n_pairs,
                        " (n1=", n1, ", n2=", n2, ")")

            lim <- tryCatch(
                dsnp_limits(
                    p0 = p0,
                    n1 = n1,
                    n2 = n2,
                    alpha = alpha_for_limits,
                    p1 = p1,
                    conservative = TRUE,
                    allow_empty_warning = allow_empty_warning,
                    max_results = .Machine$integer.max
                ),
                error = function(e)
                {
                    n_failed <<- n_failed + 1L
                    failures <<- rbind(failures, data.frame(
                        n1 = n1,
                        n2 = n2,
                        message = conditionMessage(e),
                        stringsAsFactors = FALSE
                    ))
                    NULL
                }
            )

            if(is.null(lim))
                next

            cand <- lim$candidates

            if(nrow(cand) == 0)
                next

            # Add n1 and n2 columns
            cand$n1 <- n1
            cand$n2 <- n2

            all_results[[length(all_results) + 1]] <- cand
        }
    }

    # --- Check total failures ---
    n_candidates_total <- pair_idx

    if(n_failed == n_candidates_total)
        stop("dsnp_limits() failed for all (n1, n2) pairs. ",
             n_failed, " failure(s) recorded.")

    if(length(all_results) == 0)
        stop("No candidates found for any (n1, n2) pair. ",
             n_failed, " pair(s) failed.")

    # --- Combine all candidates ---
    combined <- do.call(rbind, all_results)
    rownames(combined) <- NULL

    # --- Global feasibility ---
    feasible <- rep(TRUE, nrow(combined))

    if(!is.null(arl0_min))
        feasible <- feasible & combined$arl0 >= arl0_min

    if(!is.null(alpha))
        feasible <- feasible & combined$p_signal0 <= alpha

    combined$feasible <- feasible
    n_feasible <- sum(feasible)

    if(n_feasible == 0)
        stop("No feasible candidates found. ",
             "Consider relaxing arl0_min or alpha, ",
             "or expanding n1_range / n2_range. ",
             "Best approximation: arl0 = ", max(combined$arl0),
             ", p_signal0 = ", min(combined$p_signal0), ".")

    # --- Extract feasible candidates only ---
    candidates <- combined[feasible, , drop = FALSE]
    rownames(candidates) <- NULL

    # --- Sort by objective ---
    if(objective == "arl1")
    {
        candidates <- candidates[order(
            candidates$arl1,
            candidates$ass0,
            -candidates$arl0,
            candidates$n1 + candidates$n2,
            candidates$n1,
            candidates$n2,
            candidates$wl_accept,
            candidates$ucl1_reject,
            candidates$ucl2_accept
        ), ]
    }
    else if(objective == "ass0")
    {
        candidates <- candidates[order(
            candidates$ass0,
            candidates$arl1,
            -candidates$arl0,
            candidates$n1 + candidates$n2,
            candidates$n1,
            candidates$n2,
            candidates$wl_accept,
            candidates$ucl1_reject,
            candidates$ucl2_accept
        ), ]
    }
    else if(objective == "weighted")
    {
        candidates$arl1_scaled <- .scale_minimize(candidates$arl1)
        candidates$ass0_scaled <- .scale_minimize(candidates$ass0)

        candidates$score <- weights["arl1"] * candidates$arl1_scaled +
                            weights["ass0"] * candidates$ass0_scaled

        candidates <- candidates[order(
            if(weights["arl1"] > 0)
                !is.finite(candidates$arl1)
            else
                FALSE,
            candidates$score,
            candidates$n1 + candidates$n2,
            candidates$n1,
            candidates$n2,
            candidates$wl_accept,
            candidates$ucl1_reject,
            candidates$ucl2_accept
        ), ]
    }

    candidates$objective <- objective
    rownames(candidates) <- NULL

    # --- Limit output ---
    n_candidates_evaluated <- nrow(combined)
    candidates_out <- head(candidates, max_results)

    # --- Build return object ---
    result <- list(
        best = candidates_out[1, , drop = FALSE],
        candidates = candidates_out,
        parameters = list(
            p0 = p0,
            p1 = p1,
            n1_range = n1_range,
            n2_range = n2_range,
            arl0_min = arl0_min,
            alpha = alpha,
            objective = objective,
            weights = weights,
            allow_empty_warning = allow_empty_warning,
            max_results = max_results
        ),
        search = list(
            n_pairs_evaluated = as.integer(n_candidates_total),
            n_candidates_evaluated = as.integer(n_candidates_evaluated),
            n_feasible = as.integer(n_feasible),
            n_failed_pairs = as.integer(n_failed)
        ),
        failures = failures
    )

    class(result) <- "dsnp_design"
    result
}

#' @rdname dsnp_design
#' @param x An object of class \code{"dsnp_design"}.
#' @param ... Additional arguments (ignored).
#' @export
print.dsnp_design <- function(x, ...)
{
    cat("DS-np Complete Design Search\n")
    cat("============================\n\n")

    cat("Parameters:\n")
    cat("  p0 =", x$parameters$p0, "\n")
    cat("  p1 =", x$parameters$p1, "\n")
    cat("  arl0_min =", x$parameters$arl0_min, "\n")
    cat("  alpha =", x$parameters$alpha, "\n")
    cat("  objective =", x$parameters$objective, "\n")
    cat("  n1_range:", min(x$parameters$n1_range), "-",
        max(x$parameters$n1_range), "\n")
    cat("  n2_range:", min(x$parameters$n2_range), "-",
        max(x$parameters$n2_range), "\n\n")

    cat("Search summary:\n")
    cat("  Pairs evaluated:", x$search$n_pairs_evaluated, "\n")
    cat("  Candidates evaluated:", x$search$n_candidates_evaluated, "\n")
    cat("  Feasible candidates:", x$search$n_feasible, "\n")
    if(x$search$n_failed_pairs > 0)
        cat("  Failed pairs:", x$search$n_failed_pairs, "\n")
    cat("\n")

    cat("Best design:\n")
    b <- x$best
    cat("  n1 =", b$n1, ", n2 =", b$n2, "\n")
    cat("  wl =", b$wl, ", ucl1 =", b$ucl1, ", ucl2 =", b$ucl2, "\n")
    cat("  wl_accept =", b$wl_accept, ", ucl1_reject =", b$ucl1_reject,
        ", ucl2_accept =", b$ucl2_accept, "\n")
    cat("  p_signal0 =", b$p_signal0, "\n")
    cat("  arl0 =", b$arl0, "\n")
    cat("  ass0 =", b$ass0, "\n")
    cat("  p_signal1 =", b$p_signal1, "\n")
    cat("  arl1 =", b$arl1, "\n")
    cat("  ass1 =", b$ass1, "\n")
    if(x$parameters$objective == "weighted")
    {
        cat("  score =", b$score, "\n")
        cat("  arl1_scaled =", b$arl1_scaled, "\n")
        cat("  ass0_scaled =", b$ass0_scaled, "\n")
    }

    invisible(x)
}
