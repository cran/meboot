### R code from vignette source 'Toy_Example_Exposition.Rnw'

###################################################
### code chunk number 1: Toy_Example_Exposition.Rnw:79-94
###################################################
xx <- c(4,12,36,20,8) #original time series up and down shape
trimprop <- 0.10 #trimming proportion
reachbnd <- FALSE #reaching the bound of the range forced or  not?

# uniform draws used as an example in Table 1 of the paper

p <- c(0.12, 0.83, 0.53, 0.59, 0.11)

n <- length(xx)

x <- sort(xx)
ordxx <- sort(xx, index.return=TRUE)
print(c("ordxx=",ordxx)) #without the dollar ix appending

print(c("ordxx$ix=",ordxx$ix))


###################################################
### code chunk number 2: Toy_Example_Exposition.Rnw:105-121
###################################################
x <- sort(xx)
x #sorted magnitudes original xx data in values domain
#embed good for getting a matrix with lagged values in the second column
embed(1:4,2) #allows no worry about missing values with lags
embed(x, 2) #apply embed to our sorted xx
z <- rowMeans(embed(x, 2))
z #these are intermediate values
dv <- abs(diff(xx))
dv #vector of absolute differences
dvtrim <- mean(dv, trim=trimprop)
dvtrim #trimmed mean of dv
xmin <- x[1]-dvtrim
xmax <- x[n]+dvtrim

xmin #ultimate minimum for resampled data gives z_0
xmax #ultimate maximum for resampled data gives z_T


###################################################
### code chunk number 3: Toy_Example_Exposition.Rnw:191-198
###################################################
embed(1:5,3) #embeding 1:5 gives 3 by 3 matrix
#Note j-th column has lag=j-1 values.  Col.2 has lag 1
#Note embed retains only non-missing lag values
x
t(embed(x,3))# transpose embed matrix
t(embed(x, 3))*c(0.25,0.5,0.25) #multiply by weights
t(t(embed(x, 3))*c(0.25,0.5,0.25)) #transpose twice to get back


###################################################
### code chunk number 4: Toy_Example_Exposition.Rnw:209-218
###################################################
aux <- rowSums( t( t(embed(x, 3))*c(0.25,0.5,0.25) ) )

aux #these are only 3
#append the means of two extreme intervals at the two ends
desintxb <- c(0.75*x[1]+0.25*x[2], aux, 0.25*x[n-1]+0.75*x[n])
desintxb# des=desired, int=interval, xb=xbar=means
desintxb  #desired means  5  8 13 22 32,  Now 5 as desired
print( "mean(xx),mean(desintxb),mean(x)") #all=16
print(c(mean(xx),mean(desintxb), mean(x)))


###################################################
### code chunk number 5: Toy_Example_Exposition.Rnw:242-245
###################################################
q=rep(0,n) #place holder for q
pp=sort(p) #sorted random draws
print(c("sorted random draws", pp))


###################################################
### code chunk number 6: Toy_Example_Exposition.Rnw:270-275
###################################################
z[1] #first intermediate value
xmin #smallest
0.5*(z[1]+xmin) #average for the first interval
desintxb[1] #des=desired, int=interval, xb=xbar=mean
desintxb[1]-0.5*(z[1]+xmin)#adjustment for first interval


###################################################
### code chunk number 7: Toy_Example_Exposition.Rnw:289-305
###################################################
ref1 <- which(pp <= (1/n)) #how many are less than or equal to 1/5 if n=T=5
ref1
# approx. returns list of points which linearly interpolate given data points,
#first interval ref1

if(length(ref1)>0){
  qq <- approx(c(0,1/n), c(xmin,z[1]), pp[ref1])$y
  qq #interpolated values
adj= desintxb[1]-0.5*(z[1]+xmin)
print(c("qq=",qq,"adj=",adj))
  q[ref1] <- qq
  if(!reachbnd)  q[ref1] <- qq + desintxb[1]-0.5*(z[1]+xmin)
}

print(c("qq=",qq))
print(c("q",q))


###################################################
### code chunk number 8: Toy_Example_Exposition.Rnw:334-351
###################################################
for(i1 in 1:(n-2)){
  ref2 <- which(pp > (i1/n))
print(c("ref2",ref2,"pp[ref2]", pp[ref2]))
  ref3 <- which(pp <= ((i1+1)/n))
print(c("ref3",ref3))
  ref23 <- intersect(ref2, ref3)
print(c("ref23",ref23,"sorted draw pp[ref23]=",pp[ref23]))

  if(length(ref23)>0){
    qq <- approx(c(i1/n,(i1+1)/n), c(z[i1], z[i1+1]), pp[ref23])$y
print(c("interpolated value qq=",qq))
adj= desintxb[-1][i1]-0.5*(z[i1]+z[i1+1])
print(c("qq=",qq,"adj=",adj))
    q[ref23] <- qq + desintxb[-1][i1]-0.5*(z[i1]+z[i1+1])
print(c("q",q))
  }
}


###################################################
### code chunk number 9: Toy_Example_Exposition.Rnw:357-362
###################################################
ref4 <- which(pp == ((n-1)/n))
print(c("ref4", ref4))
if(length(ref4)>0)
  q[ref4] <- z[n-1]
q


###################################################
### code chunk number 10: Toy_Example_Exposition.Rnw:371-382
###################################################
ref5 <- which(pp > ((n-1)/n))
if(length(ref5)>0){
print(c("ref5",ref5,"pp[ref5]=",pp[ref5]))
  qq <- approx(c((n-1)/n,1), c(z[n-1],xmax), pp[ref5])$y
print(c("interpolated value qq in last interval",qq))
  q[ref5] <- qq   # this implicitly shifts xmax for algorithm
adj=desintxb[n]-0.5*(z[n-1]+xmax)
print(c("qq=",qq,"adj=",adj))

  if(!reachbnd)  q[ref5] <- qq + desintxb[n]-0.5*(z[n-1]+xmax)
}


###################################################
### code chunk number 11: Toy_Example_Exposition.Rnw:394-401
###################################################
prel=q #preliminary quantile values
qseq <- sort(q)
print(c("sorted q",qseq))
q[ordxx$ix] <- qseq
print(c("after mapping to time domain",q))

print(q)


###################################################
### code chunk number 12: Toy_Example_Exposition.Rnw:407-416
###################################################
Tim=1:5
xt=xx #notation xt for original data
xordstat=x #order stats
ord1=ordxx$ix #output of sort
intermed=c(z,xmax)  #these are zt
prel
qseq #sorted quantiles
final=q #final quantiles of ME density
cb=cbind(Tim,xt,xordstat,ord1,intermed,desintxb, p,pp,prel,final)


###################################################
### code chunk number 13: Toy_Example_Exposition.Rnw:421-424 (eval = FALSE)
###################################################
## require(xtable)
## options(xtable.comment = FALSE)
## print(xtable(cb))


