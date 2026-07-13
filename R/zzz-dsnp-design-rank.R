# Internal wrapper that preserves the public dsnp_design() API while enforcing
# the weighted-ranking rule that any finite ARL1 precedes +Inf whenever the
# ARL1 objective has positive weight.
.dsnp_design_unranked <- dsnp_design

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
    objective <- match.arg(objective)

    # Preserve the validation contract of the original implementation before
    # requesting the full candidate set internally.
    if(length(max_results) != 1 || !is.finite(max_results) ||
       max_results != floor(max_results) || max_results < 1)
        stop("max_results must be a positive integer")

    # Ask the validated implementation for the full ranked set so a finite
    # candidate cannot be lost before the additional weighted-order rule is
    # applied.
    result <- .dsnp_design_unranked(
        p0 = p0,
        p1 = p1,
        n1_range = n1_range,
        n2_range = n2_range,
        arl0_min = arl0_min,
        alpha = alpha,
        objective = objective,
        weights = weights,
        allow_empty_warning = allow_empty_warning,
        max_results = .Machine$integer.max,
        progress = progress
    )

    candidates <- result$candidates

    if(objective == "weighted" && weights["arl1"] > 0)
    {
        candidates <- candidates[order(
            !is.finite(candidates$arl1),
            candidates$score,
            candidates$n1 + candidates$n2,
            candidates$n1,
            candidates$n2,
            candidates$wl_accept,
            candidates$ucl1_reject,
            candidates$ucl2_accept
        ), , drop = FALSE]
        rownames(candidates) <- NULL
    }

    candidates <- utils::head(candidates, max_results)
    result$candidates <- candidates
    result$best <- candidates[1, , drop = FALSE]
    result$parameters$max_results <- max_results
    result
}
