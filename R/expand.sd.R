
expand.sd <- function(x, ensemble, fiv=5)
{
  sdf <- c(sd(x), sd(ensemble))
  sdfa <- sdf/sdf[1]  # ratio of actual sd to that of original data
  sdfd <- sdf[1]/sdf  # ratio of desired sd to actual sd

  # expansion is needed since some of these are <1 due to attenuation
  mx <- 1+(fiv/100)
  # following are expansion factors
  sdfa[sdfa<1] <- runif(n=length(sdfa[sdfa<1]), min=1, max=mx)

  # multiply each column by expansion factor
  out <- ensemble %*% diag(sdfd[-1]*sdfa[-1])

  if(any(is.ts(ensemble))){
    out <- ts(out, frequency=frequency(ensemble), start=start(ensemble))
    dimnames(out)[[2]] <- dimnames(ensemble)[[2]]
  }  
  out
}
