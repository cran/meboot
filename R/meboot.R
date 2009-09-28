
meboot <- function(x, reps=999, trim=0.10, reachbnd=TRUE,
  expand.sd=TRUE, force.clt=TRUE, elaps=FALSE,
  colsubj, coldata, coltimes, ...){
  UseMethod("meboot", x)
}

meboot.default <- function(x, reps=999, trim=0.10, reachbnd=TRUE,
  expand.sd=TRUE, force.clt=TRUE, elaps=FALSE, ...)
{
  ptm1 <- proc.time()

  n <- length(x)

  # Sort the original data in increasing order and
  # store the ordering index vector.

  xx <- sort(x)
  ordxx <- order(x)
  #ordxx <- sort.int(x, index.return=TRUE)

  # Compute intermediate points on the sorted series.

  z <- rowMeans(embed(xx, 2))

  # Compute lower limit for left tail ('xmin') and
  # upper limit for right tail ('xmax').
  # This is done by computing the 'trim' (e.g. 10%) trimmed mean
  # of deviations among all consecutive observations ('dv').
  # Thus the tails are uniform distributed.

  dv <- abs(diff(as.numeric(x)))
  dvtrim <- mean(dv, trim=trim)
  xmin <- xx[1] - dvtrim
  xmax <- xx[n] + dvtrim

  # Compute the mean of the maximum entropy density within each
  # interval in such a way that the 'mean preserving constraint'
  # is satisfied. (Denoted as m_t in the reference paper.)
  # The first and last interval means have distinct formulas.
  # See Theil and Laitinen (1980) for details.

  aux <- colSums( t(embed(xx, 3))*c(0.25,0.5,0.25) )
  desintxb <- c(0.75*xx[1]+0.25*xx[2], aux, 0.25*xx[n-1]+0.75*xx[n])

  # Generate random numbers from the [0,1] uniform interval and
  # compute sample quantiles at those points.

  # Generate random numbers from the [0,1] uniform interval.

  ensemble <- matrix(x, nrow=n, ncol=reps)
  ensemble <- apply(ensemble, 2, meboot.part,
                n, z, xmin, xmax, desintxb, reachbnd)

  # So far the object 'ensemble' contains the quantiles.
  # Now give them time series dependence and heterogeneity.

  qseq <- apply(ensemble, 2, sort)

  # 'qseq' has monotonic series, the correct series is obtained
  # after applying the order according to 'ordxx' defined above.

  ensemble[ordxx,] <- qseq
  #ensemble[ordxx$ix,] <- qseq

  if(expand.sd)
    ensemble <- expand.sd(x=x, ensemble=ensemble, ...)
  if(force.clt)
    ensemble <- force.clt(x=x, ensemble=ensemble)

  #ensemble <- cbind(x, ensemble)
  if(is.ts(x)){
    ensemble <- ts(ensemble, frequency=frequency(x), start=start(x))
    dimnames(ensemble)[[2]] <- paste("Series", 1:reps)
    #dimnames(ensemble)[[2]] <- c("original", paste("Series", 1:reps))
  }

  # Computation time
  ptm2 <- proc.time(); elapsr <- elapsedtime(ptm1, ptm2)
  if(elaps)
    cat("\n  Elapsed time:", elapsr$elaps, 
                             paste(elapsr$units, ".", sep=""), "\n")

  list(x=x, ensemble=ensemble, xx=xx, z=z, dv=dv, dvtrim=dvtrim, xmin=xmin,
       xmax=xmax, desintxb=desintxb, ordxx=ordxx, elaps=elapsr)
}

meboot.part <- function(x, n, z, xmin, xmax, desintxb, reachbnd)
{
  # Generate random numbers from the [0,1] uniform interval

  p <- runif(n, min=0, max=1)

  # Compute sample quantiles by linear interpolation at
  # those 'p'-s (if any) ...

  # ... 'p'-s within the (i1/n, (i1+1)/n)] interval (i1=1,...,n-2).

  q <- .C("mrapprox", p=as.double(p), n=as.integer(n), z=as.double(z),
      desintxb=as.double(desintxb[-1]), ref23=double(n), qq=double(1), 
      q=double(n), PACKAGE="meboot")$q

  # ... 'p'-s within the [0, (1/n)] interval. (Left tail.)

  ref1 <- which(p <= (1/n))
  if(length(ref1) > 0){
    qq <- approx(c(0,1/n), c(xmin,z[1]), p[ref1])$y
    q[ref1] <- qq
    if(!reachbnd)  q[ref1] <- qq + desintxb[1]-0.5*(z[1]+xmin)
  }

  # ... 'p'-s equal to (n-1)/n.

  ref4 <- which(p == ((n-1)/n))
  if(length(ref4) > 0)
    q[ref4] <- z[n-1]

  # ... 'p'-s greater than (n-1)/n. (Right tail.)

  ref5 <- which(p > ((n-1)/n))
  if(length(ref5) > 0){
    # Right tail proportion p[i]
    qq <- approx(c((n-1)/n,1), c(z[n-1],xmax), p[ref5])$y
    q[ref5] <- qq   # this implicitly shifts xmax for algorithm
    if(!reachbnd)  q[ref5] <- qq + desintxb[n]-0.5*(z[n-1]+xmax)
    # such that the algorithm gives xmax when p[i]=1
    # this is the meaning of reaching the bounds xmax and xmin
  }

  q
}

elapsedtime <- function(ptm1, ptm2)
{
  elaps <- (ptm2 - ptm1)[3]

  if(elaps < 60)
         units <- "seconds"
  else if(elaps < 3600){
         elaps <- elaps/60
         units <- "minutes" }
  else if(elaps < 86400){
         elaps <- elaps/3600
         units <- "hours" }
  else { elaps <- elaps/86400
         units <- "days" }

  list(elaps=as.numeric(elaps), units=units)
}
