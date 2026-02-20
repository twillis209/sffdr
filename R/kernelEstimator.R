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
                            nn = NULL, ...) {
  . <- NULL
  
  if (!is.null(weights)) {
    n_obs <- if (is.matrix(x)) nrow(x) else length(x)
    validate_weights(weights, n_obs)
  }
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

  # two groups
  x <- process.vals(x)
  eval.points <- process.vals(eval.points)
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
    fitfunc <- function(...) locfit(~lp(s[, 1], s[, 2], ...),
                                    weights = weights,
                                    maxk = maxk)
  } else {
    fitfunc <- function(...) locfit(~lp(s, ...),
                                    weights = weights,
                                    maxk = maxk)
  }

  lfit <- fitfunc(..., nn = nn)

  eval.s <- trans(eval.points)

  fs.hat <- predict(lfit, newdata = eval.s)
  if (is.matrix(eval.points)) {
    corrector <- apply(dens(eval.s), 1, prod)
  } else {
    corrector <- dens(eval.s)
  }

  fx.hat <- fs.hat / corrector
  if (is.matrix(x)) {
    colnames(eval.points) <- c("x1", "x2")
    colnames(eval.s) <- c("s1", "s2")
  }

  ret <- cbind(x = eval.points, fx = fx.hat, eval.s, fs = fs.hat) %>%
    as.data.frame() %>%
    tibble::as_tibble()
  if (trim && !is.matrix(x)) {
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
#'
#' Returns the transformed data points that kernelEstimator would use for fitting,
#' after all preprocessing (clamping, transformation, subsampling, weighting).
#'
#' @param x Numeric vector of p-values (1D) or 2-column matrix with 
#'   (covariate, p-values) for 2D case.
#' @param weights Optional numeric vector of weights (e.g., LD score weights).
#' @param subsample Maximum number of points to subsample. NULL = no subsampling.
#' @param epsilon Lower bound for clamping. Default: 1e-15.
#' @param epsilon.max Upper bound for clamping. Default: 0.999.
#' @param transformation One of "probit", "cloglog", or "ident". Default: "probit".
#'
#' @return Data frame with transformed coordinates and weights.
#'   For 2D: columns s1, s2, weight
#'   For 1D: columns s, weight
#'
#' @export
get_kde_training_data <- function(
  x,
  weights = NULL,
  subsample = 1e7,
  epsilon = 1e-15,
  epsilon.max = 0.999,
  transformation = "probit"
) {
  # Validate weights
  if (!is.null(weights)) {
    n_obs <- if (is.matrix(x)) nrow(x) else length(x)
    validate_weights(weights, n_obs)
  }
  
  # Check dimensionality
  is_2d <- is.matrix(x) && ncol(x) >= 2
  
  # Set up transformation
  trans <- switch(transformation,
                  probit = qnorm,
                  cloglog = function(v) -log(-log(v)),
                  ident = identity)
  
  # Process values (clamp)
  process_vals <- function(vals) {
    vals <- pmax(vals, epsilon)
    vals <- pmin(vals, epsilon.max)
  }
  
  x_processed <- process_vals(x)
  
  # Transform
  s <- trans(x_processed)
  
  # Subsample if needed
  if (!is.null(subsample)) {
    if (is_2d && nrow(s) > subsample) {
      idx <- sample(nrow(s), subsample)
      s <- s[idx, , drop = FALSE]
      if (!is.null(weights)) {
        weights <- weights[idx]
      }
    } else if (!is_2d && length(s) > subsample) {
      idx <- sample(length(s), subsample)
      s <- s[idx]
      if (!is.null(weights)) {
        weights <- weights[idx]
      }
    }
  }
  
  # Build result
  if (is_2d) {
    result <- data.frame(
      s1 = s[, 1],
      s2 = s[, 2],
      weight = if (is.null(weights)) rep(1.0, nrow(s)) else weights
    )
  } else {
    result <- data.frame(
      s = s,
      weight = if (is.null(weights)) rep(1.0, length(s)) else weights
    )
  }
  
  # Add attributes for plotting
  attr(result, "transformation") <- transformation
  attr(result, "epsilon") <- epsilon
  attr(result, "epsilon.max") <- epsilon.max
  attr(result, "n_effective") <- sum(result$weight)
  attr(result, "is_2d") <- is_2d
  attr(result, "has_weights") <- !is.null(weights)
  
  return(result)
}


#' Plot KDE Training Data
#'
#' Visualize the transformed data landscape that kernelEstimator fits to.
#'
#' @param data Data frame from get_kde_training_data()
#' @param title Optional plot title
#'
#' @return ggplot2 object
#'
#' @import ggplot2
#' @export
plot_kde_training_data <- function(data, title = "KDE Training Data") {
  
  is_2d <- attr(data, "is_2d")
  trans <- attr(data, "transformation")
  epsilon <- attr(data, "epsilon")
  epsilon.max <- attr(data, "epsilon.max")
  n_eff <- attr(data, "n_effective")
  has_weights <- attr(data, "has_weights")
  
  if (is_2d) {
    # 2D scatterplot
    if (has_weights) {
      # Color by weight if available
      p <- ggplot2::ggplot(data, ggplot2::aes(x = s1, y = s2, 
                                               color = weight, size = weight)) +
        ggplot2::geom_point(alpha = 0.6) +
        ggplot2::scale_color_viridis_c(name = "Weight", option = "plasma") +
        ggplot2::scale_size_continuous(range = c(0.5, 3), guide = "none")
    } else {
      # Uniform color if no weights
      p <- ggplot2::ggplot(data, ggplot2::aes(x = s1, y = s2)) +
        ggplot2::geom_point(alpha = 0.6, color = "#377EB8", size = 1)
    }
    
    p <- p +
      ggplot2::labs(
        x = sprintf("%s(covariate)", if (trans == "probit") "Φ⁻¹" else trans),
        y = sprintf("%s(p-value)", if (trans == "probit") "Φ⁻¹" else trans),
        title = title,
        subtitle = sprintf("n = %d, n_eff = %.0f%s", 
                          nrow(data), n_eff,
                          if (has_weights) ", LD-weighted" else "")
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "right",
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 10, color = "gray30")
      )
    
    # Add reference lines if probit
    if (trans == "probit") {
      p <- p +
        ggplot2::geom_hline(yintercept = qnorm(epsilon.max), 
                   linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::geom_hline(yintercept = qnorm(epsilon), 
                   linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::annotate("text", x = Inf, y = qnorm(epsilon.max),
                 label = sprintf("ε.max = %.3f", epsilon.max),
                 hjust = 1.1, vjust = -0.5, size = 3, color = "gray40")
    }
    
  } else {
    # 1D density plot
    p <- ggplot2::ggplot(data, ggplot2::aes(x = s, weight = weight)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                     bins = 50, fill = "#377EB8", alpha = 0.7, color = "white") +
      ggplot2::geom_density(color = "#E41A1C", linewidth = 1) +
      ggplot2::labs(
        x = sprintf("%s(p-value)", if (trans == "probit") "Φ⁻¹" else trans),
        y = "Density",
        title = title,
        subtitle = sprintf("n = %d, n_eff = %.0f%s", 
                          nrow(data), n_eff,
                          if (has_weights) ", weighted" else "")
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 10, color = "gray30")
      )
    
    # Add reference lines if probit
    if (trans == "probit") {
      p <- p +
        ggplot2::geom_vline(xintercept = qnorm(epsilon.max), 
                   linetype = "dashed", color = "gray40") +
        ggplot2::geom_vline(xintercept = qnorm(epsilon), 
                   linetype = "dashed", color = "gray40")
    }
  }
  
  return(p)
}


#' Extract KDE Training Data
#'
#' Returns the transformed data points that kernelEstimator would use for fitting,
#' after all preprocessing (clamping, transformation, downsampling, jitter).
#'
#' @param x Numeric vector of p-values (1D) or 2-column matrix with 
#'   (surrogate, p-values) for 2D case.
#' @param indep_snps Logical vector indicating independent SNPs (optional).
#' @param epsilon Lower bound for clamping. Default: 1e-15.
#' @param epsilon.max Upper bound for clamping. Default: 1 - 1e-4.
#' @param target_null Max null SNPs to keep (2D only). Default: 100000.
#' @param tail_threshold Z-score threshold for signal region (2D only). Default: -2.
#' @param transformation One of "probit", "cloglog", or "ident". Default: "probit".
#'
#' @return Data frame with transformed coordinates, weights, and metadata.
#'   For 2D: columns s1 (surrogate), s2 (p-value), weight, region
#'   For 1D: columns s (p-value), weight
#'
#' @export
get_kde_training_data <- function(
  x,
  indep_snps = NULL,
  epsilon = 1e-15,
  epsilon.max = 1 - 1e-4,
  target_null = 100000,
  tail_threshold = -2,
  transformation = "probit"
) {
  # Check dimensionality
  is_2d <- is.matrix(x) && ncol(x) >= 2
  
  # Apply independence filter
  if (!is.null(indep_snps)) {
    x <- if (is_2d) x[indep_snps, , drop = FALSE] else x[indep_snps]
  }
  
  # Set up transformation
  trans <- switch(transformation,
                  probit = qnorm,
                  cloglog = function(v) -log(-log(v)),
                  ident = identity)
  
  # Clamp and transform
  clamp_transform <- function(v) {
    trans(pmin(pmax(v, epsilon), epsilon.max))
  }
  
  train_s <- if (is_2d) {
    cbind(clamp_transform(x[, 1]), clamp_transform(x[, 2]))
  } else {
    clamp_transform(x)
  }
  
  # Add jitter
  jitter_mag <- 1e-6
  if (is_2d) {
    train_s[, 2] <- train_s[, 2] + runif(nrow(train_s), -jitter_mag, jitter_mag)
  } else {
    train_s <- train_s + runif(length(train_s), -jitter_mag, jitter_mag)
  }
  
  # Adaptive downsampling (2D only)
  if (is_2d) {
    z <- train_s[, 1]
    idx_signal <- which(z < tail_threshold)
    idx_null <- which(z >= tail_threshold)
    
    n_signal <- length(idx_signal)
    n_null_full <- length(idx_null)
    
    if (n_null_full > target_null) {
      # Downsample nulls
      idx_null_keep <- sample(idx_null, target_null)
      idx_final <- c(idx_signal, idx_null_keep)
      weights <- c(rep(1.0, n_signal), rep(n_null_full / target_null, target_null))
      region <- c(rep("Signal", n_signal), rep("Null", target_null))
    } else {
      # Keep all
      idx_final <- seq_len(nrow(train_s))
      weights <- rep(1.0, length(idx_final))
      region <- ifelse(seq_along(idx_final) <= n_signal, "Signal", "Null")
    }
    
    # Build result
    result <- data.frame(
      s1 = train_s[idx_final, 1],
      s2 = train_s[idx_final, 2],
      weight = weights,
      region = region
    )
    
  } else {
    # 1D case
    result <- data.frame(
      s = train_s,
      weight = rep(1.0, length(train_s))
    )
  }
  
  # Add attributes for plotting
  attr(result, "transformation") <- transformation
  attr(result, "epsilon") <- epsilon
  attr(result, "epsilon.max") <- epsilon.max
  attr(result, "tail_threshold") <- if (is_2d) tail_threshold else NULL
  attr(result, "n_effective") <- sum(result$weight)
  attr(result, "is_2d") <- is_2d
  
  return(result)
}


#' Plot KDE Training Data
#'
#' Visualize the transformed data landscape that kernelEstimator fits to.
#'
#' @param data Data frame from get_kde_training_data()
#' @param title Optional plot title
#'
#' @return ggplot2 object
#'
#' @import ggplot2
#' @export
plot_kde_training_data <- function(data, title = "KDE Training Data") {
  
  is_2d <- attr(data, "is_2d")
  trans <- attr(data, "transformation")
  epsilon <- attr(data, "epsilon")
  epsilon.max <- attr(data, "epsilon.max")
  tail_thresh <- attr(data, "tail_threshold")
  n_eff <- attr(data, "n_effective")
  
  if (is_2d) {
    # 2D scatterplot
    p <- ggplot2::ggplot(data, ggplot2::aes(x = s1, y = s2, color = region, size = weight)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::scale_color_manual(
        values = c("Signal" = "#E41A1C", "Null" = "#377EB8"),
        name = "Region"
      ) +
      ggplot2::scale_size_continuous(range = c(0.5, 3), name = "Weight") +
      ggplot2::labs(
        x = sprintf("%s(surrogate)", if (trans == "probit") "Φ⁻¹" else trans),
        y = sprintf("%s(p-value)", if (trans == "probit") "Φ⁻¹" else trans),
        title = title,
        subtitle = sprintf("n = %d, n_eff = %.0f", nrow(data), n_eff)
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "right",
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 10, color = "gray30")
      )
    
    # Add reference lines if probit
    if (trans == "probit") {
      p <- p +
        ggplot2::geom_hline(yintercept = qnorm(epsilon.max), 
                   linetype = "dashed", color = "gray40", alpha = 0.7) +
        ggplot2::geom_hline(yintercept = qnorm(epsilon), 
                   linetype = "dashed", color = "gray40", alpha = 0.7)
      
      if (!is.null(tail_thresh)) {
        p <- p +
          ggplot2::geom_vline(xintercept = tail_thresh, 
                     linetype = "dashed", color = "orange", linewidth = 1) +
          ggplot2::annotate("text", x = tail_thresh, y = Inf, 
                   label = "signal threshold", 
                   hjust = -0.1, vjust = 1.5, color = "orange", size = 3.5)
      }
    }
    
  } else {
    # 1D density plot
    p <- ggplot2::ggplot(data, ggplot2::aes(x = s, weight = weight)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), 
                     bins = 50, fill = "#377EB8", alpha = 0.7, color = "white") +
      ggplot2::geom_density(color = "#E41A1C", linewidth = 1) +
      ggplot2::labs(
        x = sprintf("%s(p-value)", if (trans == "probit") "Φ⁻¹" else trans),
        y = "Density",
        title = title,
        subtitle = sprintf("n = %d, n_eff = %.0f", nrow(data), n_eff)
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(size = 10, color = "gray30")
      )
    
    # Add reference lines if probit
    if (trans == "probit") {
      p <- p +
        ggplot2::geom_vline(xintercept = qnorm(epsilon.max), 
                   linetype = "dashed", color = "gray40") +
        ggplot2::geom_vline(xintercept = qnorm(epsilon), 
                   linetype = "dashed", color = "gray40")
    }
  }
  
  return(p)
}
