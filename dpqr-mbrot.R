library(VGAM)


dmbrot <- function(x, shape, start = 1, log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1)
    stop("bad input for argument 'log'")
  rm(log)
  
  LLL <- max(length(shape), length(x), length(start)) # length of ans
  if (length(x)     != LLL) x     <- rep_len(x,     LLL)
  if (length(shape) != LLL) shape <- rep_len(shape, LLL)
  if (length(start) != LLL) start <- rep_len(start, LLL)
  
  ox <- !is.finite(x) # F = no problems, they are all finite
  zero <- ox | round(x) != x | x < start # any problems? F = No
  ans <- rep_len(if (log.arg) log(0) else 0, LLL) # replicate
  if (any(!zero)) { # assign value to where TRUEs are
    ans[!zero] <- (
      ((shape[!zero]-1)*gamma(start[!zero])*gamma(x[!zero]+1-shape[!zero])) /
        (gamma(start[!zero]+1-shape[!zero])*gamma(x[!zero]+1))
    )
    if (log.arg) # if log = TRUE then log the previous answers
      ans[!zero] <- log(ans[!zero])
  }
  if (any(ox)) # if any x has a problem (not Inf)
  ans[ox] <- if (log.arg) log(0) else 0 # handle Inf x: log(0) = -Inf 
  ans[shape <= 1 | shape >= 2] <- NaN # When shape out of bounds, give NaN
  ans[start != round(start) | start < 1] <- NaN # bad start, give NaA
  ans
} # dmbrot

# When the input = Inf, the output = -Inf?? Maybe check with the formula
# using lgamma rather than gamma, said Thomas. errors when I use lgamma..


pmbrot <- function(q, shape, start = 1, lower.tail = TRUE) {
  if (!is.logical(lower.tail) || length(lower.tail ) != 1)
    stop("bad input for argument 'lower.tail'")
  
  LLL <- max(length(shape), length(q), length(start)) # length of ans
  if (length(q)     != LLL) q     <- rep_len(q,     LLL)
  if (length(shape) != LLL) shape <- rep_len(shape, LLL)
  if (length(start) != LLL) start <- rep_len(start, LLL)
  
  if (lower.tail) { # upper or lower tail
    ans <- 1 - ((gamma(start)*gamma(q+1-shape)) / (gamma(q)*gamma(start+1-shape)))
  } else {
    ans <-     ((gamma(start)*gamma(q+1-shape)) / (gamma(q)*gamma(start+1-shape)))
  }
  
 # if (log.p) { # log of cdf
 #    negv <- ans < 0
 #   ans[negv] <- NaN
 #    ans[!negv] <- log(ans)
 #  }
  
  ans[q < start] <- if (lower.tail) 0 else 1
  ans[shape <= 1 | shape >= 2] <- NaN
  ans[start != round(start) | start < 1] <- NaN
  ans
}  # pmbrot


qmbrot <- function(p, shape, start = 1) {
  
  # be the inverse of pmbrot
  
} # qmbrot


rmbrot <- function(n, shape, start = 1) {
  rr <- runif(n)
  qmbrot(rr, shape, start = start)
} # rmbrot

