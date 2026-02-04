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
#' @importFrom splines ns
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
      temp <- paste0("splines::ns(",  cn[i],  ", df=", basis.df, ")")
    } else {
      if (!is.null(indep_snps)) {
        z0 <- z[indep_snps,, drop = F]
      } else {
        z0 <- z
      }
      k = quantile(z0[, i], knots)
      temp <- paste0("splines::ns(",  cn[i],  ", knots=c(", paste0(k, collapse = ","), "))")
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
#' \code{construct_fdr_formula} creates a model formula with knots placed adaptively
#' based on FDR estimates. Knots are concentrated in regions where signal exists.
#'
#' @param x A vector of p-values for a single trait, or a matrix where columns are
#' different informative variables (p-values)
#' @param indep_snps Optional boolean vector indicating independent SNPs to use for
#' FDR calculation
#' @param fdr_threshold FDR threshold for identifying signal. Default is 0.5.
#' @param min_signal Minimum number of tests with FDR <= threshold. Default is 50.
#' @param moderate_signal Threshold between moderate and strong signal. Default is 100.
#'
#' @return A formula string for use in modeling
#'
#' @details
#' This function uses qvalue to identify regions with FDR <= 0.5 and places
#' knots adaptively:
#' \itemize{
#'   \item If minimal signal (< 50 tests with FDR <= 0.5): excludes variable from model
#'   \item If moderate signal (50-99 tests): uses single knot at maximum
#'   \item If strong signal (>= 100 tests): uses quartile knots within signal region
#' }
#' 
#' When multiple variables are provided, applies this logic to each variable independently
#' and combines them into a single formula.
#'
#' @examples
#' \dontrun{
#' data(bmi)
#' # Single trait
#' bfp_model <- construct_fdr_formula(sumstats$bfp)
#' 
#' # Multiple traits
#' z <- as.matrix(sumstats[, -1])
#' multi_model <- construct_fdr_formula(z)
#' }
#'
#' @author Andrew J. Bass
#' @seealso \code{\link{pi0_model}}
#' @keywords construct_fdr_formula
#' @aliases construct_fdr_formula
#' @export
construct_fdr_formula <- function(x, 
                            indep_snps = NULL,
                            fdr_threshold = 0.5,
                            min_signal = 50,
                            moderate_signal = 100) {
  
  # Handle single vector case (original behavior)
  if (is.vector(x) || ncol(as.matrix(x)) == 1) {
    x_vec <- as.vector(x)
    if (!is.null(indep_snps)) {
      x_vec <- x_vec[indep_snps]
    }
    
    ecdf_func <- ecdf(x_vec)
    fdr <- qvalue::qvalue(x_vec)$qvalue
    
    if (min(fdr) > fdr_threshold | sum(fdr <= fdr_threshold) < min_signal) {
      warning("Very little signal in study")
      return("~1")
    } else {
      tot <- sum(fdr <= fdr_threshold)
      if (tot < moderate_signal) {
        fdr_knots <- quantile(x_vec[fdr <= fdr_threshold], 1)
      } else {
        fdr_knots <- quantile(x_vec[fdr <= fdr_threshold], seq(0.25, 1, 0.25))
      } 
      fdr_knots <- unique(ecdf_func(fdr_knots))
    }
    
    return(paste0(
      "~splines::ns(z1, knots=c(", paste0(fdr_knots, collapse = ","), "))"
    ))
  }
  
  # Handle matrix case (multiple variables)
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("z", 1:ncol(x))
  }
  
  cn <- colnames(x)
  terms <- NULL
  
  # Process each variable
  for (i in 1:ncol(x)) {
    pvals <- x[, i]
    var_name <- cn[i]
    
    # Use independent SNPs if specified
    if (!is.null(indep_snps)) {
      pvals_indep <- pvals[indep_snps]
    } else {
      pvals_indep <- pvals
    }
    
    # Remove NAs
    pvals_indep <- pvals_indep[!is.na(pvals_indep)]
    
    if (length(pvals_indep) == 0) {
      warning(sprintf("Variable '%s' has no valid p-values. Skipping.", var_name))
      next
    }
    
    # Compute FDR
    fdr <- tryCatch({
      qvalue::qvalue(pvals_indep)$qvalue
    }, error = function(e) {
      warning(sprintf("Could not compute qvalue for variable '%s': %s. Skipping.", 
                      var_name, e$message))
      return(NULL)
    })
    
    if (is.null(fdr)) {
      next
    }
    
    # Check signal strength
    n_signal <- sum(fdr <= fdr_threshold)
    
    if (n_signal < min_signal) {
      warning(sprintf("Variable '%s' has minimal signal (%d tests with FDR <= %.2f). Excluding from model.", 
                      var_name, n_signal, fdr_threshold))
      next
    }
    
    # Compute ECDF for knot placement
    ecdf_func <- ecdf(pvals_indep)
    
    # Determine knot placement based on signal strength
    if (n_signal < moderate_signal) {
      # Moderate signal: single knot at maximum signal p-value
      signal_pvals <- pvals_indep[fdr <= fdr_threshold]
      fdr_knots <- max(signal_pvals)
    } else {
      # Strong signal: quartile knots within signal region
      signal_pvals <- pvals_indep[fdr <= fdr_threshold]
      fdr_knots <- quantile(signal_pvals, seq(0.25, 1, 0.25))
    }
    
    # Convert to ECDF values (quantiles)
    fdr_knots <- unique(ecdf_func(fdr_knots))
    
    # Create spline term
    temp <- paste0("splines::ns(", var_name, ", knots=c(", 
                   paste0(fdr_knots, collapse = ","), ")")
    terms <- c(terms, temp)
  }
  
  # Build formula
  if (length(terms) == 0) {
    warning("No variables with sufficient signal. Returning intercept-only model.")
    return("~1")
  }
  
  return(paste0("~", paste(terms, collapse = " + ")))
}

