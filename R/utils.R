make_feature_histogram_grid <- function(x, bins = 20L) {
  if (!is.matrix(x) || !is.numeric(x) || ncol(x) == 0L) {
    stop("x must be a non-empty numeric matrix.")
  }

  d <- ncol(x)
  if (length(bins) != 1L && length(bins) != d) {
    stop("bins must have length 1 or one value per column of x.")
  }
  if (length(bins) == 1L) bins <- rep(bins, d)
  if (any(!is.finite(bins)) || any(bins < 1) || any(bins != floor(bins))) {
    stop("bins must contain positive integers.")
  }
  bins <- as.integer(bins)

  # Use only rows with finite values in every dimension.
  x <- x[apply(is.finite(x), 1L, all), , drop = FALSE]
  if (nrow(x) == 0L) stop("x has no rows with finite values in every column.")

  breaks <- lapply(seq_len(d), function(j) {
    limits <- range(x[, j])
    if (limits[1L] == limits[2L]) limits <- limits + c(-0.5, 0.5)
    seq(limits[1L], limits[2L], length.out = bins[j] + 1L)
  })

  centers <- lapply(breaks, function(b) (b[-1L] + b[-length(b)]) / 2)


  list(
    breaks = breaks,
    centers = centers
  )
}