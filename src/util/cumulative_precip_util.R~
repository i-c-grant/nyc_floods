calc_decay_sum <- function(x, decay) {
  ans <- numeric(length(x))
  ans[1] <- x[1]
  
  for (i in 2:length(x)) {
    ans[i] <- max(0, ans[i - 1] + x[i] - decay)
  }
  
  return(ans)
}

calc_decay_mult <- function(x, decay) {
  ans <- numeric(length(x))
  ans[1] <- x[1]
  
  for (i in 2:length(x)) {
    ans[i] <- max(0, decay * (ans[i - 1] + x[i]))
  }
  
  return(ans)
}
