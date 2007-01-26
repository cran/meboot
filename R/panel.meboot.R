
panel.meboot  <- function(x, reps, colsubj, coldata, coltimes,...)
{
  ref1 <- x[,colsubj]
  ref2 <- unique(ref1)
  
  xens <- NULL
  for(i in seq(along=ref2)){
    ir <- which(ref1==ref2[i])
    xs <- x[ir,coldata]  
    bxs <- meboot(xs, reps=reps,...)
    xens <- rbind(xens, bxs$ensemble)
  }

  xens <- data.frame(xens)
  dimnames(xens)[[2]] <- paste("Pseries", 1:reps)
  if(!missing(coltimes))
    dimnames(xens)[[1]] <- paste("Series", 1:reps, x[,coltimes])

  xens
}
  
