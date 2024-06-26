moment <- function(data, k = 5:(sum(data>0)-1)) {

  X <- sort(data[data>0], decreasing=TRUE)
  n <- length(X)
  if (!all(k < n)) {
    k <- k[k < n]
    warning("Only those k for which X_{n-k:n} is positive are retained.", call. = FALSE)
  }
  std.err <- numeric(length(k))

  # --- Moment estimates

  l <- log(X[1:(max(k)+1)])
  s1 <- cumsum(l[1:max(k)])[k]
  s2 <-	cumsum((l[1:max(k)])^2)[k]
  M1 <- s1 / k - l[k+1]
  M2 <- s2 / k - 2 * l[k+1] * s1 / k + (l[k+1])^2
  Moment <- M1 + 1 - 0.5 / (1 - M1^2 / M2)

  # --- standard errors

  if (any(Moment >= 0)) {
    I <- Moment >= 0
    g <- Moment[I]
    std.err[I] <- 1 + g^2
  }
  if (any(Moment < 0)) {
    I <- Moment < 0
    g <- Moment[I]
    std.err[I] <- (1-g)^2 * (1-2*g) * (6*g^2 - g + 1) / ((1-3*g) * (1-4*g))
  }
  std.err <- sqrt(std.err/k)

  # --- output list

  out <- list(n = n, k = k, threshold = X[k+1], estimate = Moment,
              std.err = std.err)

  invisible(out)

}

### example
#
# normal.data <- rnorm(1000)
# plot(normal.data)
#
# mom.result <- moment(normal.data, k=c(10:500))
# plot(mom.result$k, mom.result$estimate, type="l", lwd=1.5)
# abline(h=0, col="red")
