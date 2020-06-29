
mebootSpear <- function(x,
                        reps=999,
                        setSpearman=1,
                        drift=TRUE,
                        trim=0.10,
                        xmin=NULL,
                        xmax=NULL,
                        reachbnd=TRUE,
                        expand.sd=TRUE, force.clt=TRUE,
                        scl.adjustment = FALSE, sym = FALSE, elaps=FALSE,
                        colsubj, coldata, coltimes,...)
{
  if ("pdata.frame" %in% class(x))
  {
    res <- meboot::meboot.pdata.frame (x, reps, trim$trim, reachbnd,
                                       expand.sd, force.clt, scl.adjustment, sym, elaps,
                                       colsubj, coldata, coltimes, ...)
    return(res)
  }
  
  
  if(is.null(setSpearman)) setSpearman <- -99
  
  trim <- list(trim=trim, xmin=xmin, xmax=xmax)
  
  trimval <- if (is.null(trim$trim)) 0.1 else trim$trim
  
  
  ptm1 <- proc.time()
  
  n <- length(x)
  
  # Sort the original data in increasing order and
  # store the ordering index vector.
  
  xx <- sort(x)
  ordxx <- order(x)
  
  
  ### Fred Viole SUGGESTION PART 1 of 2
  
  if(setSpearman <1){
    if(setSpearman < -0.5) ordxx_2 <- rev(ordxx) else ordxx_2 <- order(ordxx)
  }
  
  #ordxx <- sort.int(x, index.return=TRUE)
  
  # symmetry
  
  if (sym)
  {
    xxr <- rev(xx) #reordered values
    xx.sym <- mean(xx) + 0.5*(xx - xxr) #symmetrized order stats
    xx <- xx.sym #replace order stats by symmetrized ones
  }
  
  # Compute intermediate points on the sorted series.
  
  z <- rowMeans(embed(xx, 2))
  
  # Compute lower limit for left tail ('xmin') and
  # upper limit for right tail ('xmax').
  # This is done by computing the 'trim' (e.g. 10%) trimmed mean
  # of deviations among all consecutive observations ('dv').
  # Thus the tails are uniform distributed.
  
  dv <- abs(diff(as.numeric(x)))
  dvtrim <- mean(dv, trim=trimval)
  
  if (is.list(trim))
  {
    if (is.null(trim$xmin))
    {
      xmin <- xx[1] - dvtrim
    } else
      xmin <- trim$xmin
    
    if (is.null(trim$xmax))
    {
      xmax <- xx[n] + dvtrim
    } else
      xmax <- trim$xmax
    
    if (!is.null(trim$xmin) || !is.null(trim$xmax))
    {
      if (isTRUE(force.clt))
      {
        expand.sd <- FALSE
        force.clt <- FALSE
        warning("expand.sd and force.clt were set to FALSE in order to ",
                "enforce the limits xmin/xmax.")
      }
    }
  } else {
    xmin <- xx[1] - dvtrim
    xmax <- xx[n] + dvtrim
  }
  
  # do this here so that this warnings are printed after
  # the above warnings (if necessary)
  
  if (is.list(trim))
  {
    if (!is.null(trim$xmin) && trim$xmin > min(x))
      warning("the lower limit trim$xmin may not be satisfied in the replicates ",
              "since it is higher than the minimum value observed ",
              "in the input series x")
    if (!is.null(trim$xmax) && trim$xmax < max(x))
      warning("the upper limit trim$xmax may not be satisfied in the replicates ",
              "since it is lower than the maximum value observed ",
              "in the input series x")
  }
  
  
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
  ensemble <- apply(ensemble, 2, meboot.part.spear,
                    n, z, xmin, xmax, desintxb, reachbnd)
  
  # So far the object 'ensemble' contains the quantiles.
  # Now give them time series dependence and heterogeneity.
  
  qseq <- apply(ensemble, 2, sort)
  
  
  # 'qseq' has monotonic series, the correct series is obtained
  # after applying the order according to 'ordxx' defined above.
  
  ensemble[ordxx,] <- qseq
  
  
  ### Pilot Spearman
  if(setSpearman==-99){
    y <- mebootSpear(x, reps = 30, setSpearman = 1)$ensemble
    pilot <- cbind(x,y)
    setSpearman <-  fivenum(apply(pilot, 2, function(z) (cor(pilot[,1],z)))[-1])[2]
  }
  
  
  
  ### Fred Viole SUGGESTION  PART 2 of 2
  ### Average two ordxx ensemble matrices
  
  if(setSpearman<1){
    matrix2 = matrix(, nrow=length(x), ncol = reps)
    matrix2[ordxx_2,] = qseq
    
    # Intial search
    
    e <- c(ensemble)
    m <- c(matrix2)
    l <- length(e)
    
    func <- function(ab, d=drift){
      a <- ab[1]
      b <- ab[2]
      
      ifelse(d,
             (abs(cor((a*m + b*e)/(a + b), e, method = "spearman") - setSpearman) +
                abs(mean((a*m + b*e))/mean(e) - 1) +
                abs( cor((a*m + b*e)/(a + b), 1:l) - cor(e, 1:l))
             ),
             abs(cor((a*m + b*e)/(a + b), e, method = "spearman") - setSpearman) +
               abs(mean((a*m + b*e))/mean(e) - 1)
      )
      
    }
    
    res <- optim(c(.01,.01), func, control=list(abstol = .01))
    
    ensemble <- (res$par[1]*matrix2 +
                   res$par[2]*ensemble) / (sum(abs(res$par)))
    
    if(identical(ordxx_2, ordxx)){
      if(reps>1) ensemble <- t(apply(ensemble, 1, 
                                     function(x) sample(x, size = reps, replace = TRUE)))
    }
    
  }
  
  
  if(expand.sd)
    ensemble <- expand.sd(x=x, ensemble=ensemble, ...)
  
  if(force.clt && reps > 1)
    ensemble <- force.clt(x=x, ensemble=ensemble)
  
  # scale adjustment
  
  if (scl.adjustment)
  {
    zz <- c(xmin,z,xmax) #extended list of z values
    v <- diff(zz^2) / 12
    xb <- mean(x)
    s1 <- sum((desintxb - xb)^2)
    uv <- (s1 + sum(v)) / n
    desired.sd <- sd(x)
    actualME.sd <- sqrt(uv)
    if (actualME.sd <= 0)
      stop("actualME.sd<=0 Error")
    out <- desired.sd / actualME.sd
    kappa <- out - 1
    
    ensemble <- ensemble + kappa * (ensemble - xb)
  } else
    kappa <- NULL
  
  
  # Force min / max values
  if(!is.null(trim[[2]])) ensemble <- apply(ensemble, 2, function(x) pmax(trim[[2]], x))
  if(!is.null(trim[[3]])) ensemble <- apply(ensemble, 2, function(x) pmin(trim[[3]], x))
  
  if(is.ts(x)){
    ensemble <- ts(ensemble, frequency=frequency(x), start=start(x))
    if(reps>1) dimnames(ensemble)[[2]] <- paste("Series", 1:reps)
  } else {
    if(reps>1) dimnames(ensemble)[[2]] <- paste("Replicate", 1:reps)
  }
  
  
  # Computation time
  ptm2 <- proc.time(); elapsr <- meboot::elapsedtime(ptm1, ptm2)
  if(elaps)
    cat("\n  Elapsed time:", elapsr$elaps,
        paste(elapsr$units, ".", sep=""), "\n")
  
  list(x=x, ensemble=ensemble, rowAvg=rowMeans(ensemble), xx=xx,
       z=z, dv=dv,
       dvtrim=dvtrim, xmin=xmin,
       xmax=xmax, desintxb=desintxb, ordxx=ordxx, 
       kappa = kappa, elaps = elapsr)
}


##################################################################


meboot.part.spear <- function(x, n, z, xmin, xmax, desintxb, reachbnd)
{
  # Generate random numbers from the [0,1] uniform interval
  p <- runif(n, min=0, max=1)
  
  # Assign quantiles of x from p
  td <- tdigest::tdigest(x, compression = max(100, log(n,10)*100))
  
  q <- tryCatch(tdigest::tquantile(td, p) ,
                error = quantile(x, p))
  
  
  ref1 <- which(p <= (1/n))
  if(length(ref1) > 0){
    qq <- approx(c(0,1/n), c(xmin,z[1]), p[ref1])$y
    q[ref1] <- qq
    if(!reachbnd)  q[ref1] <- qq + desintxb[1]-0.5*(z[1]+xmin)
  }
  
  ref4 <- which(p == ((n-1)/n))
  if(length(ref4) > 0)
    q[ref4] <- z[n-1]
  
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
