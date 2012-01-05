pkgname <- "meboot"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('meboot')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("checkConv")
### * checkConv

flush(stderr()); flush(stdout())

### Name: checkConv
### Title: Check Convergence
### Aliases: checkConv
### Keywords: ts

### ** Examples

library(strucchange)
data("PhillipsCurve")

uk <- window(PhillipsCurve, start = 1948)
attach(data.frame(uk))
y <- uk[,5]
bigx <- cbind(dp1,du,u1) # collect all regressors
reg1 <- lm(y ~ bigx)
coef(reg1)
nover <- 16 # choose range allowing n=25, 26, .. , 40

# the following step will take some time to run

sC <- checkConv(y, bigx, trueb=0, n999=999, nover=nover, seed1=294)

dim(sC) # n999 x nover x length(key)
j <- 3  # choose key coefficient no. 3 for lagged income
epsilon <- 0.1  #needed for p.as.plot command below
nb.sp <- 10 # needed for p.as.plot command below 
mode <- "p"  # needed for p.as.plot command below

# criterion function in ConvergenceConcepts package wants 999 rows
dat <- sC[,,j] # 999 sample paths for diff inflation n=25,26,..40
critp <- criterion(data = dat, epsilon = epsilon, mode = "p")$crit
critas <- criterion(data = dat, epsilon = epsilon, mode = "as")$crit

p.as.plot(dat, critp, critas, epsilon, nb.sp, mode = mode)  
nstart <- length(y) - nover + 1
nn <- seq(nstart, length(y))  # choose the range of n the sample size

opar <- par(mfrow = c(2,1)) #plot 2 plots in one
plot(nn,critp, typ="l", 
  main="Convergence in probability: Diff(Inflation) Coefficient",
  xlab="Sample size", ylab="Criterion using 999 sample paths")
plot(nn,critas, typ="l", 
  main="Almost sure convergence:  Diff(Inflation) Coefficient",
  xlab="Sample size", ylab="Criterion using 999 sample paths")
par(opar)

regp <- lm(critp ~ nn) # OLS of conv. in prob. criterion
sup <- summary(regp) # regressed on sample size
sup$coef
# slope coeff. should be negative in sign for convergence
# the t statistic on the slope coefficient should be large
regas <- lm(critas ~ nn) #OLS of almost sure conv criterion
suas <- summary(regas)#regressed on sample size
suas$coef
# slope coeff. should be negative in sign for convergence
# the t statistic on the slope coefficient should be large



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("expand.sd")
### * expand.sd

flush(stderr()); flush(stdout())

### Name: expand.sd
### Title: Expand the Standard Deviation of Resamples
### Aliases: expand.sd
### Keywords: ts

### ** Examples
    
    set.seed(345)
    out <- meboot(x=AirPassengers, reps=100, trim=0.10, reachbnd=FALSE, elaps=TRUE) 
    exp.ens <- expand.sd(x=AirPassengers, out$ensemble)
  


cleanEx()
nameEx("flexMeboot")
### * flexMeboot

flush(stderr()); flush(stdout())

### Name: flexMeboot
### Title: Flexible Extension of the Maximum Entropy Bootstrap Procedure
### Aliases: flexMeboot
### Keywords: ts

### ** Examples

set.seed(235)
myseq <- seq(-1, 1, by = 0.5)
xx <- flexMeboot(x = AirPassengers, myseq = myseq, reps = 3)
matplot(cbind(AirPassengers, xx), type = "l")



cleanEx()
nameEx("force.clt")
### * force.clt

flush(stderr()); flush(stdout())

### Name: force.clt
### Title: Enforce Central Limit Theorem
### Aliases: force.clt
### Keywords: ts

### ** Examples

    set.seed(345)
    out <- meboot(x=AirPassengers, reps=100, trim=0.10, reachbnd=FALSE, elaps=TRUE)
    cm1 <- colMeans(out$ensemb)
    # Note that the column means are somewhat non-normal
    qqnorm(cm1)

    clt.ens <- force.clt(x=AirPassengers, ensemble=out$ensemble) 
    cm2 <- colMeans(clt.ens)
    # Note that the columns are closer to being normal
    qqnorm(cm2)

  


cleanEx()
nameEx("meboot")
### * meboot

flush(stderr()); flush(stdout())

### Name: meboot
### Title: Generate Maximum Entropy Bootstrapped Time Series Ensemble
### Aliases: meboot
### Keywords: ts

### ** Examples

    ## Ensemble for the AirPassenger time series data
    set.seed(345)
    out <- meboot(x=AirPassengers, reps=100, trim=0.10, elaps=TRUE)

    ## Ensemble for T=5 toy time series used in Vinod (2004)
    set.seed(345)
    out <- meboot(x=c(4, 12, 36, 20, 8), reps=999, trim=0.25, elaps=TRUE)
    mean(out$ens)  # ensemble mean should be close to sample mean 16
  


cleanEx()
nameEx("meboot.pdata.frame")
### * meboot.pdata.frame

flush(stderr()); flush(stdout())

### Name: meboot-methods
### Title: Maximum Entropy Bootstrap for Panel Time Series Data
### Aliases: 'meboot, pdata.frame, meboot.method' meboot.pdata.frame
###   meboot.default
### Keywords: ts

### ** Examples

    ## Ensemble for a panel of series of stock prices  
    data("ullwan")
    out <- meboot(ullwan, reps=99, colsubj=2, coldata=4)
  


cleanEx()
nameEx("null.ci")
### * null.ci

flush(stderr()); flush(stdout())

### Name: null.ci
### Title: Get Confidence Interval Around Specified NullZero Total
### Aliases: null.ci
### Keywords: ts

### ** Examples

    x <- runif(25, 0, 1)
    null.ci(x)
  


cleanEx()
nameEx("olsHALL.b")
### * olsHALL.b

flush(stderr()); flush(stdout())

### Name: olsHALL.b
### Title: OLS regression model for consumption
### Aliases: olsHALL.b

### ** Examples

    data("USconsum")
    USconsum <- log(USconsum)

    # lm interface 
    lmcf1 <- lm(USconsum[-1,1] ~ USconsum[-51,1] + USconsum[-51,2])
    coefficients(lmcf1)
 
    # dynlm interface 
    library("dynlm")
    lmcf2 <- dynlm(consum ~ L(consum, 1) + L(dispinc, 1), data=USconsum)
    coefficients(lmcf2)

    # olsHALL.b
    olsHALL.b(y=USconsum[,1], x=USconsum[,2])
  


cleanEx()
nameEx("slider.mts")
### * slider.mts

flush(stderr()); flush(stdout())

### Name: slider.mts
### Title: Slider for Selecting a Single Time Series to Plot from a 'mts'
###   Object
### Aliases: slider.mts
### Keywords: ts

### ** Examples

    ## Ensemble for the AirPassenger time series data
    set.seed(345)
    out <- meboot(x=AirPassengers, reps=100)
    slider.mts(ts.union(AirPassengers, out$ensemble), 
      ylim=range(out$ensemble))
  


cleanEx()
nameEx("zero.ci")
### * zero.ci

flush(stderr()); flush(stdout())

### Name: zero.ci
### Title: Get Confidence Interval Around Zero
### Aliases: zero.ci
### Keywords: ts

### ** Examples

    x <- runif(25, 0, 1)
    zero.ci(x)
  


### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
