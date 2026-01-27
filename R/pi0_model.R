#' Formulates the model for the proportion of null tests
#'
#' \code{pi0_model} helps generate the model for the proportion of truly null tests.
#'  For more details, refer to the vignette.
#'
#' @param z \code{matrix}: informative variables that impact the power of the
#' p-values (rows are tests and columns are different informative variables).
#' Currently, there must be no missing values.
#' @param basis.df \code{integer}: the degrees of freedom for the natural cubic spline on each variable.
#' Default is 3 at equally space intervals.
#' @param indep_snps \code{vector} Boolean indicating the set of independent SNPs
#' @param knots \code{vector}: Specify the location of the knots in natural cubic spline. Note
#' that the knots are specified using quantiles by default. Default is NULL and uses basis.df at equally space intervals.
#'
#' @details
#' We note that this function is specifically designed for informative p-values and other
#' complex models should be created outside this function.
#'
#' @return
#' A list with the following entries:
#' \enumerate{
#'  \item fmod: model formula
#'  \item zt: matrix of rank-transformed informative variables
#'  }
#'
#' @examples
#' data(bmi)
#'
#' p <- sumstats$bmi
#' z <- as.matrix(sumstats[, -1])
#'
#' # For p-values, you want to specify the lower quantiles
#' fmod <- pi0_model(z, knots = c(0.005, 0.01, 0.025, 0.05, 0.1))
#'
#' @author Andrew Bass
#' @seealso \code{\link{sffdr}}
#' @keywords pi0_model
#' @aliases pi0_model
#' @export
pi0_model <- function(z,
                      indep_snps = NULL,
                      basis.df = 3,
                      knots = NULL) {
  if (!is.matrix(z)) {
    stop("informative variables must be a matrix")
  }

  # rank transform
  NA_IND <- is.na(z)
  for (i in 1:ncol(z)) {
    ind <- !NA_IND[, i]
    ztmp <- z[ind, i]
    z[ind, i] <- rank(ztmp) / length(ztmp)
  }

  # remove any NA's
  z_out <- z
  z_na <- apply(z, 1, anyNA)
  z <- z[!z_na,, drop = F]
  indep_snps <- indep_snps[!z_na]
  n <- ncol(z)
  m <- nrow(z)

  if (is.null(colnames(z))) {
    colnames(z) <- colnames(z_out) <- paste0("z", 1:ncol(z))
  }
  terms <- NULL
  cn <- colnames(z)
  for (i in 1:ncol(z)) {
    if (is.null(knots)) {
      temp <- paste0("ns(",  cn[i],  ", df=", basis.df, ")")
    } else {
      if (!is.null(indep_snps)) {
        z0 <- z[indep_snps,, drop = F]
      } else {
        z0 <- z
      }
      k = quantile(z0[, i], knots)
      temp <- paste0("ns(",  cn[i],  ", knots=c(", paste0(k, collapse = ","), "))")
    }
    terms <- c(temp, terms)
  }

  fmod <- formula(paste("~", paste(terms, collapse = "+")))
  z_out[!z_na,] <- z
  list(fmod = fmod,
       zt = as_tibble(z_out))
}

#' Construct model formula using FDR-based knot selection
#'
#' \code{construct_model} creates a model formula with knots placed adaptively
#' based on FDR estimates. Knots are concentrated in regions where signal exists.
#'
#' @param x A vector of p-values for a single trait
#'
#' @return A formula object for use in modeling
#'
#' @details
#' This function uses qvalue to identify regions with FDR <= 0.5 and places
#' knots adaptively:
#' \itemize{
#'   \item If minimal signal (< 50 tests with FDR <= 0.5): returns intercept-only model
#'   \item If moderate signal (50-99 tests): uses single knot at maximum
#'   \item If strong signal (>= 100 tests): uses quartile knots within signal region
#' }
#'
#' @examples
#' \dontrun{
#' data(bmi)
#' # Create model for BFP
#' bfp_model <- construct_model(sumstats$bfp)
#' }
#'
#' @author Andrew J. Bass
#' @seealso \code{\link{pi0_model}}
#' @keywords construct_model
#' @aliases construct_model
#' @export
construct_model <- function(x) {
  ecdf <- ecdf(x)
  fdr <- qvalue::qvalue(x)$qvalue
  
  if (min(fdr) > 0.5 | sum(fdr <= .5) < 50) {
    fdr_knots <- 0.5
    warning("Very little signal in study")
    my_form <- paste0("~1")
    return(my_form)
  } else {
    tot <- sum(fdr <= 0.5)
    if (tot < 100) {
      fdr_knots <- quantile(x[fdr <= 0.5], 1)
    } else {
      fdr_knots <- quantile(x[fdr <= 0.5], seq(0.25, 1, 0.25))
    } 
    fdr_knots <- unique(ecdf(fdr_knots))
  }
  
  my_form <- paste0(
    "~ns(z1, knots =c(", paste0(fdr_knots, collapse = ","), "))"
  )
  return(my_form)
}
