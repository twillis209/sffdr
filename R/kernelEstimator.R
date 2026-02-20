#' Estimate a density on the unit interval or unit square via
#' local regression
#'
#' Provide density estimates that are needed by \code{sffdr}
#'
#' @param x Either a vector or a 2-column matrix
#' @param transformation Either probit (default), complementary log-log, or
#' identity (not recommended)
#' @param eval.points Points at which to evaluate the estimate, default x
#' @param subsample Number of points that are randomly subsampled for
#' computing the fit; useful for computational efficiency and for ensuring
#' the density estimation does not run out of memory. NULL means no the
#' fit is performed on all points
#' @param weights Optional numeric vector of weights for kernel density estimation. Downweights observations in high-density/high-LD regions. Default is NULL (equal weights).
#' @param epsilon How close values are allowed to come to 0
#' @param epsilon.max How close values are allowed to come to 1
#' @param maxk maxk argument passed to locfit
#' @param nn nearest neighbor parameter
#' @param trim In one-dimensional fitting, the very edges often have high
#' variance. This parameter fixes the estimate on the intervals
#' (0, trim) and (1 - trim, 1).
#' @param ... additional arguments to be passed to lp in locfit, used only
#' if cv=FALSE
#'
#' @description This function is adapted from the fFDR package.
#' @export
kernelEstimator <- function(x,
                            transformation = "probit",
                            eval.points = x,
                            subsample = 1e+07,
                            weights = NULL,
                            epsilon = 1e-15,
                            epsilon.max = 0.999,
                            maxk = 10000,
                            trim = 1e-15,
                            nn = NULL,
                            indep_snps = NULL, ...) {
  . <- NULL
  
  # Get training data using shared preprocessing
  train_data <- get_kde_training_data(
    x = x,
    transformation = transformation,
    subsample = subsample,
    weights = weights,
    epsilon = epsilon,
    epsilon.max = epsilon.max,
    indep_snps = indep_snps
  )
  
  # Extract transformed training points and weights
  is_matrix <- attr(train_data, "is_2d")
  if (is_matrix) {
    s <- cbind(train_data$s1, train_data$s2)
    train_weights <- train_data$weight
  } else {
    s <- train_data$s
    train_weights <- train_data$weight
  }
  
  # Set up transformation functions for evaluation
  transformation <- match.arg(as.character(transformation),
                              c("ident", "cloglog", "probit"))
  trans <- switch(transformation, ident = identity, cloglog = function(x) -log(-log(x)),
                  probit = qnorm)
  inv <- switch(transformation, ident = identity, cloglog = function(x) exp(-exp(-x)),
                probit = pnorm)
  dens <- switch(transformation, ident = function(x) 1, cloglog = function(x) exp(-x) *
                   exp(-exp(-x)), probit = dnorm)
  
  process.vals <- function(vals) {
    vals <- pmax(vals, epsilon)
    vals <- pmin(vals, epsilon.max)
  }

  # Fit locfit
  if (is_matrix) {
    fitfunc <- function(...) locfit(~lp(s[, 1], s[, 2], ...),
                                    weights = train_weights,
                                    maxk = maxk)
  } else {
    fitfunc <- function(...) locfit(~lp(s, ...),
                                    weights = train_weights,
                                    maxk = maxk)
  }

  lfit <- fitfunc(..., nn = nn)

  # Process and transform evaluation points
  eval.points.processed <- process.vals(eval.points)
  eval.s <- trans(eval.points.processed)

  fs.hat <- predict(lfit, newdata = eval.s)
  if (is_matrix) {
    corrector <- apply(dens(eval.s), 1, prod)
  } else {
    corrector <- dens(eval.s)
  }

  fx.hat <- fs.hat / corrector
  if (is_matrix) {
    colnames(eval.points.processed) <- c("x1", "x2")
    colnames(eval.s) <- c("s1", "s2")
  }

  ret <- cbind(x = eval.points.processed, fx = fx.hat, eval.s, fs = fs.hat) %>%
    as.data.frame() %>%
    tibble::as_tibble()
  if (trim && !is_matrix) {
    ret$fx[ret$x < trim] <- ret %>%
      dplyr::slice(which.min(abs(x - trim))) %>%
      .$fx
    ret$fx[ret$x > 1 - trim] <- ret %>%
      dplyr::slice(which.min(abs(x - (1 - trim)))) %>%
      .$fx
  }
  attr(ret, "lfit") = lfit
  return(ret)
}

#' Extract KDE Training Data
#' @param x Vector or 2-column matrix
#' @param transformation "probit", "cloglog", or "ident"
#' @param subsample Subsample size
#' @param weights Optional weights vector
#' @param epsilon Lower bound
#' @param epsilon.max Upper bound
#' @param indep_snps Optional logical vector or indices indicating independent SNPs to use
#' @export
get_kde_training_data <- function(x,
                                   transformation = "probit",
                                   subsample = 1e+07,
                                   weights = NULL,
                                   epsilon = 1e-15,
                                   epsilon.max = 0.999,
                                   indep_snps = NULL) {
  # Filter to independent SNPs first if provided
  if (!is.null(indep_snps)) {
    if (is.logical(indep_snps)) {
      # Logical vector
      if (is.matrix(x)) {
        x <- x[indep_snps, , drop = FALSE]
      } else {
        x <- x[indep_snps]
      }
      if (!is.null(weights)) {
        weights <- weights[indep_snps]
      }
    } else {
      # Numeric indices
      if (is.matrix(x)) {
        x <- x[indep_snps, , drop = FALSE]
      } else {
        x <- x[indep_snps]
      }
      if (!is.null(weights)) {
        weights <- weights[indep_snps]
      }
    }
  }
  
  if (!is.null(weights)) {
    n_obs <- if (is.matrix(x)) nrow(x) else length(x)
    validate_weights(weights, n_obs)
  }
  
  transformation <- match.arg(as.character(transformation),
                              c("ident", "cloglog", "probit"))
  trans <- switch(transformation, ident = identity, cloglog = function(x) -log(-log(x)),
                  probit = qnorm)
  
  process.vals <- function(vals) {
    vals <- pmax(vals, epsilon)
    vals <- pmin(vals, epsilon.max)
  }
  
  x <- process.vals(x)
  s <- trans(x)
  
  if (!is.null(subsample)) {
    if (is.matrix(s) && subsample < nrow(s)) {
      idx <- sample(nrow(s), subsample)
      s <- s[idx, ]
      if (!is.null(weights)) {
        weights <- weights[idx]
      }
    }
    else if (!is.matrix(s) && subsample < length(s)) {
      idx <- sample(length(s), subsample)
      s <- s[idx]
      if (!is.null(weights)) {
        weights <- weights[idx]
      }
    }
  }
  
  if (is.matrix(s)) {
    result <- data.frame(
      s1 = s[, 1],
      s2 = s[, 2],
      weight = if (is.null(weights)) 1.0 else weights
    )
  } else {
    result <- data.frame(
      s = s,
      weight = if (is.null(weights)) 1.0 else weights
    )
  }
  
  attr(result, "transformation") <- transformation
  attr(result, "epsilon") <- epsilon
  attr(result, "epsilon.max") <- epsilon.max
  attr(result, "n_effective") <- sum(result$weight)
  attr(result, "is_2d") <- is.matrix(s)
  
  return(result)
}

#' Plot KDE Training Data
#' @param data Data frame from get_kde_training_data()
#' @param title Plot title
#' @import ggplot2
#' @export
plot_kde_training_data <- function(data, title = "KDE Training Data") {
  is_2d <- attr(data, "is_2d")
  trans <- attr(data, "transformation")
  epsilon.max <- attr(data, "epsilon.max")
  n_eff <- attr(data, "n_effective")
  
  if (is_2d) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = s1, y = s2, color = weight, size = weight)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::scale_color_viridis_c(option = "plasma") +
      ggplot2::scale_size_continuous(range = c(0.5, 3)) +
      ggplot2::labs(
        x = if (trans == "probit") "Φ⁻¹(covariate)" else trans,
        y = if (trans == "probit") "Φ⁻¹(p-value)" else trans,
        title = title,
        subtitle = sprintf("n = %d, n_eff = %.0f", nrow(data), n_eff)
      ) +
      ggplot2::theme_minimal()
    
    if (trans == "probit") {
      p <- p + ggplot2::geom_hline(yintercept = qnorm(epsilon.max), 
                                    linetype = "dashed", color = "gray40")
    }
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = s, weight = weight)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                              bins = 50, fill = "#377EB8", alpha = 0.7) +
      ggplot2::geom_density(color = "#E41A1C", linewidth = 1) +
      ggplot2::labs(
        x = if (trans == "probit") "Φ⁻¹(p-value)" else trans,
        y = "Density",
        title = title,
        subtitle = sprintf("n = %d, n_eff = %.0f", nrow(data), n_eff)
      ) +
      ggplot2::theme_minimal()
    
    if (trans == "probit") {
      p <- p + ggplot2::geom_vline(xintercept = qnorm(epsilon.max), 
                                    linetype = "dashed", color = "gray40")
    }
  }
  
  return(p)
}

#' Plot KDE with Training Data Overlay
#'
#' Visualizes the fitted KDE density surface together with the training data points.
#' For 2D, shows contours with points overlaid. For 1D, shows density curve with points.
#'
#' @param kde_result Result from kernelEstimator() (includes fitted locfit object)
#' @param train_data Result from get_kde_training_data() (training points)
#' @param title Optional plot title
#' @param n_grid Number of grid points for density evaluation (default 50)
#' @param show_points Whether to overlay training points (default TRUE)
#' @return A ggplot2 object
#' @import ggplot2
#' @export
plot_kde_with_data <- function(kde_result, train_data, title = NULL, n_grid = 50, show_points = TRUE) {
  is_2d <- attr(train_data, "is_2d")
  transformation <- attr(train_data, "transformation")
  epsilon <- attr(train_data, "epsilon")
  epsilon.max <- attr(train_data, "epsilon.max")
  lfit <- attr(kde_result, "lfit")
  
  if (is.null(lfit)) {
    stop("kde_result must include locfit object (returned by kernelEstimator)")
  }
  
  trans <- switch(transformation, 
                  ident = identity, 
                  cloglog = function(x) -log(-log(x)),
                  probit = qnorm)
  
  if (is_2d) {
    # Create evaluation grid in transformed space
    s1_range <- range(train_data$s1)
    s2_range <- range(train_data$s2)
    s1_grid <- seq(s1_range[1], s1_range[2], length.out = n_grid)
    s2_grid <- seq(s2_range[1], s2_range[2], length.out = n_grid)
    grid_df <- expand.grid(s1 = s1_grid, s2 = s2_grid)
    
    # Evaluate density on grid (predict expects matrix for 2D)
    grid_matrix <- as.matrix(grid_df)
    grid_df$density <- predict(lfit, newdata = grid_matrix)
    
    # Create contour plot
    p <- ggplot2::ggplot(grid_df, ggplot2::aes(x = s1, y = s2, z = density)) +
      ggplot2::geom_contour_filled(alpha = 0.7) +
      ggplot2::scale_fill_viridis_d(option = "viridis") +
      ggplot2::labs(
        x = if (transformation == "probit") "Φ⁻¹(covariate)" else transformation,
        y = if (transformation == "probit") "Φ⁻¹(p-value)" else transformation,
        title = title %||% "KDE Density with Training Data",
        fill = "Density"
      ) +
      ggplot2::theme_minimal()
    
    # Overlay training points if requested
    if (show_points) {
      p <- p + ggplot2::geom_point(
        data = train_data, 
        ggplot2::aes(x = s1, y = s2, size = weight),
        color = "white", alpha = 0.4, inherit.aes = FALSE
      ) +
      ggplot2::scale_size_continuous(range = c(0.3, 2))
    }
    
    # Add boundary lines
    if (transformation == "probit") {
      boundary_val <- qnorm(epsilon.max)
      p <- p + 
        ggplot2::geom_vline(xintercept = boundary_val, linetype = "dashed", color = "red") +
        ggplot2::geom_hline(yintercept = boundary_val, linetype = "dashed", color = "red")
    }
    
  } else {
    # 1D case: evaluate density on grid
    s_range <- range(train_data$s)
    s_grid <- seq(s_range[1], s_range[2], length.out = n_grid * 5)  # More points for smooth curve
    density_vals <- predict(lfit, newdata = s_grid)
    grid <- data.frame(s = s_grid, density = density_vals)
    
    # Create plot
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = grid, ggplot2::aes(x = s, y = density), 
                        color = "darkblue", linewidth = 1.2) +
      ggplot2::labs(
        x = if (transformation == "probit") "Φ⁻¹(p-value)" else transformation,
        y = "Density",
        title = title %||% "KDE Density with Training Data"
      ) +
      ggplot2::theme_minimal()
    
    # Overlay training points if requested
    if (show_points) {
      p <- p + ggplot2::geom_rug(
        data = train_data,
        ggplot2::aes(x = s, alpha = weight),
        sides = "b", length = ggplot2::unit(0.05, "npc")
      ) +
      ggplot2::scale_alpha_continuous(range = c(0.2, 0.8))
    }
    
    # Add boundary line
    if (transformation == "probit") {
      boundary_val <- qnorm(epsilon.max)
      p <- p + 
        ggplot2::geom_vline(xintercept = boundary_val, linetype = "dashed", color = "red")
    }
  }
  
  return(p)
}
