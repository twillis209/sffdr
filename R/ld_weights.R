#' Convert LD Scores to Weights
#'
#' @param ld_scores Numeric vector of LD scores (number of LD partners or summed r²)
#' @param normalize Logical; normalize weights to mean = 1? (default: TRUE)
#'
#' @return Numeric vector of weights
#'
#' @details
#' Computes weights as reciprocal of LD scores: weight = 1 / ld_score
#' This downweights SNPs that tag many other SNPs.
#' Normalization to mean = 1 maintains effective sample size.
#'
#' @examples
#' # Example: SNPs with varying LD
#' ld_scores <- c(1, 5, 10, 20, 100)
#' weights <- ld_score_to_weights(ld_scores)
#' # SNP in high LD (score=100) gets weight of ~0.05
#' # Isolated SNP (score=1) gets weight of ~5
#'
#' @export
ld_score_to_weights <- function(ld_scores, normalize = TRUE) {
  
  if (any(ld_scores <= 0, na.rm = TRUE)) {
    stop("All LD scores must be positive")
  }
  
  weights <- 1 / ld_scores
  
  if (normalize) {
    weights <- weights / mean(weights, na.rm = TRUE)
  }
  
  return(weights)
}

#' Validate Weight Vector
#'
#' @param weights Numeric vector of weights
#' @param n Expected length
#' 
#' @return Logical; TRUE if valid, stops otherwise
#' @keywords internal
validate_weights <- function(weights, n) {
  if (!is.numeric(weights)) {
    stop("'weights' must be numeric")
  }
  if (length(weights) != n) {
    stop("Length of 'weights' (", length(weights), 
         ") must match data (", n, ")")
  }
  if (any(weights <= 0, na.rm = TRUE)) {
    stop("All weights must be positive")
  }
  if (any(is.infinite(weights))) {
    stop("Weights cannot be infinite")
  }
  return(TRUE)
}
