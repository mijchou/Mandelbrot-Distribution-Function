# library(VGAM)


dmbrot <- function(x, shape, start = 1, log = FALSE) {
  if (!is.logical(log.arg <- log) || length(log) != 1)
    stop("bad input for argument 'log'")
  rm(log)
  
  LLL <- max(length(shape), length(x), length(start)) # manual recycle
  if (length(x)     != LLL) x     <- rep_len(x,     LLL)
  if (length(shape) != LLL) shape <- rep_len(shape, LLL)
  if (length(start) != LLL) start <- rep_len(start, LLL)
  
  ox <- !is.finite(x) # F = no problems, x are all finite
  zero <- ox | round(x) != x | x < 1 # x any problems? F = No
  ans <- rep_len(if (log.arg) log(0) else 0, LLL) # replicate
  if (any(!zero)) { # assign value to where TRUEs are
    ans[!zero] <- (
       log(shape[!zero]-1) + 
         lgamma(start[!zero]) +
         lgamma(x[!zero]+1-shape[!zero]) -
         lgamma(start[!zero]+1-shape[!zero]) -
         lgamma(x[!zero]+1)
    )
    ans <- exp(ans)
    if (log.arg) # if log = TRUE then log the previous answers
      ans[!zero] <- log(ans[!zero])
  }
  
  if (any(ox)) # if any x is Inf
  ans[ox] <- if (log.arg) log(0) else 0 # handle Inf: log(0) = -Inf
  ans[x != round(x) | x < 1 | is.na(x)] <- NaN
  ans[shape <= 1 | shape >= 2] <- NaN # When shape out of bounds, give NaN
  ans[start != round(start) | start < 1] <- NaN # bad start, give NaN
  ans
  
} # dmbrot

# if (any(ox)) ans[ox] <- if (log.arg) log(0) else 0 
# When x = Inf & log = T, return log(0)?
# Why not log(Inf)

pmbrot <- function(q, shape, start = 1, lower.tail = TRUE) {
  if (!is.logical(lower.tail) || length(lower.tail ) != 1)
    stop("bad input for argument 'lower.tail'")
  
  LLL <- max(length(shape), length(q), length(start)) # length of ans
  if (length(q)     != LLL) q     <- rep_len(q,     LLL)
  if (length(shape) != LLL) shape <- rep_len(shape, LLL)
  if (length(start) != LLL) start <- rep_len(start, LLL)
  
  q <- q + 1 ## TEMPORARY: FIXING PROBLEMS... ans[1] always = 1
  
  if (lower.tail) { # upper or lower tail
    ans <- -expm1((lgamma(start) + lgamma(q+1-shape)) -
                    (lgamma(q) + lgamma(start+1-shape)))
  } else {
    ans <-     exp((lgamma(start) + lgamma(q+1-shape)) -
                     (lgamma(q) + lgamma(start+1-shape)))
  }
  
 # if (log.p) {         # log of cdf -- add in later
 #    negv <- ans < 0
 #   ans[negv] <- NaN
 #    ans[!negv] <- log(ans)
 #  }
  
  ans[q == Inf] <- 1
  ans[q == -Inf | q != round(q) | q < 1 | is.na(q)] <- NaN
  ans[q < start] <- if (lower.tail) 0 else 1 # Not too sure
  ans[shape <= 1 | shape >= 2] <- NaN
  ans[start != round(start) | start < 1] <- NaN
  ans

}  # pmbrot

qmbrot <- function(p, shape, start = 1) {
  
  LLL <- max(length(p), length(shape), length(start))
  if (length(p)     != LLL) p     <- rep_len(p,     LLL)
  if (length(shape) != LLL) shape <- rep_len(shape, LLL)
  if (length(start) != LLL) start <- rep_len(start, LLL)
  
  lo <- rep_len(start, LLL) # manual recycle
  approx.ans <- lo  # True at lhs
  hi <- 2 * lo + 10 # ??
  dont.iterate <- p == 1 | shape <= 1 | shape >= 2 | # problems detection
    start != round(start) | start < 1
  done <- p <= pmbrot(hi, shape, start = start) | dont.iterate # FALSE = not yet done
  max.iter <- 100 # iterate 100 times
  iter <- 0       # from 0
  while (!all(done) && iter < max.iter) { # keep looping while done = FALSE
    hi.save <- hi[!done]
    hi[!done] <- 2 * lo[!done] + 10 # update hi
    lo[!done] <- hi.save
    done[!done] <- is.infinite(hi[!done]) |
      (p[!done] <= pmbrot(hi[!done], shape[!done], start[!done]))
    iter <- iter + 1
  }
  
  foo <- function(q, shape, start, p)
    pmbrot(q, shape, start) - p
  
  lhs <- (p <= dmbrot(start, shape, start = start)) | dont.iterate
  
  approx.ans[!lhs] <- bisection.basic(foo, lo[!lhs], hi[!lhs], tol = 1/16, # 1/16?
                                      shape = shape[!lhs],
                                      start = start[!lhs], p = p[!lhs])
  faa <- floor(approx.ans)
  ans <- ifelse(pmbrot(faa, shape, start = start) <  p &
                  p <= pmbrot(faa+1, shape, start = start), faa+1, faa)
  
  ans[p == 1] <- Inf
  ans[p == 0] <- 0
  ans[shape <= 1 | shape >= 2] <- NaN
  ans[start != round(start) | start < 1] <- NaN
  
  ans
  
} # qmbrot


rmbrot <- function(n, shape, start = 1) {
  rr <- runif(n)
  qmbrot(rr, shape, start = start)
} # rmbrot

