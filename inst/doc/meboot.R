### R code from vignette source 'meboot.Rnw'

###################################################
### code chunk number 1: meboot.Rnw:49-50
###################################################
options(prompt = "R> ", digits = 4, continue = "+  ")


###################################################
### code chunk number 2: meboot.Rnw:391-401
###################################################
library("meboot")
library("car")
library("lmtest")
library("dynlm")
data("USconsum")
USconsum <- log(USconsum)
lmcf <- dynlm(consum ~ L(consum, 1) + L(dispinc, 1), data = USconsum)
coeftest(lmcf)
set.seed(135)
durbinWatsonTest(model = lmcf, max.lag = 4)


###################################################
### code chunk number 3: meboot.Rnw:461-466
###################################################
theta <- function(y, x) {
  reg <- dynlm(y ~ L(y, 1) + L(x, 1))
  thet <- coef(reg)[3]
  return(thet)
}


###################################################
### code chunk number 4: meboot.Rnw:488-493
###################################################
theta.cobbd <- function(y, x1, x2) {
  reg <- lm(y ~ x1 + x2)
  thet <- coef(reg)[2] + coef(reg)[3]
  return(thet)
}


###################################################
### code chunk number 5: meboot.Rnw:523-539
###################################################
bstar.consu <- function(y, x, theta,
  level = 0.95, bigJ = 999, seed1 = 135) {
  set.seed(seed1)
  semy <- meboot(x = y, reps = bigJ)$ensemble
  semx <- meboot(x = x, reps = bigJ)$ensemble
  n <- NROW(y)
  m <- length(theta(y, x))
  if(m!=1) stop("too many parameters in theta")
  bb <- matrix(NA, bigJ)
  for(j in 1:bigJ) {
    yy <- semy[,j]
    xx <- semx[,j]
    bb[j] <- theta(yy, xx)
  }
  return(bb)
}


###################################################
### code chunk number 6: meboot.Rnw:558-576
###################################################
bstar.cobbd <- function(y, x1, x2, theta = theta.cobbd,
  level = 0.95, bigJ = 999, seed1 = 135) {
  set.seed(seed1)
  semy <- meboot(x = y, reps = bigJ)$ensemble
  semx1 <- meboot(x = x1, reps = bigJ)$ensemble
  semx2 <- meboot(x = x2, reps = bigJ)$ensemble
  n <- NROW(y)
  m <- length(theta.cobbd(y, x1, x2))
  if(m!=1) stop("too many parameters in theta")
  bb <- matrix(NA, bigJ)
  for(j in 1:bigJ) {
    yy <- semy[,j]
    xx1 <- semx1[,j]
    xx2 <- semx2[,j]
    bb[j] <- theta.cobbd(yy, xx1, xx2)
  }
  return(bb)
}


###################################################
### code chunk number 7: meboot.Rnw:593-610
###################################################
y <- USconsum[,2]
x <- USconsum[,1]
reg <- dynlm(y ~ L(y, 1) + L(x, 1))
su <- summary(reg)
se <- su$coefficients[3,2]
t0 <- theta(y, x)
b3s <- bstar.consu(y, x, theta)
simple.percentile <- quantile(b3s, c(0.025, 0.975), type = 8)
asymmetric.around.0 <- null.ci(b3s)
out <- list(t = b3s, t0 = t0, var.t0 = se^2, R = 999)
class(out) <- "boot"
library("boot")
boot.percentile <- boot.ci(out, type = "perc")$percent[4:5]
boot.norm <- boot.ci(out, type = "norm")$normal[2:3]
boot.basic <- boot.ci(out, type = "basic")$basic[4:5]
rbind(simple.percentile, asymmetric.around.0, boot.percentile,
  boot.norm, boot.basic)


###################################################
### code chunk number 8: hdrcde
###################################################
library("hdrcde")
hdr.den(b3s, main = expression(Highest ~ density ~ region ~
  of ~ beta [3] ~ estimates :  ~  Hall ~ model))


###################################################
### code chunk number 9: meboot.Rnw:752-760
###################################################
library("plm")
data("ullwan")
attach(ullwan)
LMV <- log(MktVal)
ullwan[,3] <- LMV
names(ullwan)[3] <- "LMV"
head(ullwan)
tail(ullwan)


###################################################
### code chunk number 10: meboot.Rnw:775-776
###################################################
summary(lm(Price ~ LMV + Tb3))


###################################################
### code chunk number 11: meboot.Rnw:818-823
###################################################
jboot <- 999
set.seed(567)
LMV.ens <- meboot(x = ullwan, reps = jboot, colsubj = 1, coldata = 3)
Price.ens <- meboot(x = ullwan, reps = jboot, colsubj = 1, coldata = 4)
Tb3.ens <- meboot(x = ullwan, reps = jboot, colsubj = 1, coldata = 6)


###################################################
### code chunk number 12: meboot.Rnw:830-844
###################################################
slopeTb3 <- slopeLMV <- rep(0, jboot)
for(j in 1:jboot) {
  frm <- data.frame(Subj = ullwan[,1], Time = ullwan[,2],
    Price = Price.ens[,j], Tb3 = Tb3.ens[,j], LMV = LMV.ens[,j])
  frm <- pdata.frame(frm, 7)
  gip <- coef(plm(Price ~ LMV + Tb3, model = "pooling", data = frm))
  slopeTb3[j] <- gip[3]
  slopeLMV[j] <- gip[2]
}
Percentile.Tb3 <- quantile(slopeTb3, c(0.025, 0.975), type = 8)
Refined.Tb3 <- null.ci(slopeTb3)
Percentile.LMV <- quantile(slopeLMV, c(0.025, 0.975), type = 8)
Refined.LMV <- null.ci(slopeLMV)
rbind(Percentile.Tb3, Refined.Tb3, Percentile.LMV, Refined.LMV)


###################################################
### code chunk number 13: meboot.Rnw:864-876
###################################################
thetp <- plm(Price ~ LMV + Tb3, model = "pooling", data = ullwan)
varTb3 <- thetp$vcov[3,3]
plm1 <- coef(thetp)
t0Tb3 <- plm1[3]
t0LMV <- plm1[2]
out2 <- list(t = as.matrix(slopeTb3), t0 = t0Tb3,
  var.t0 = varTb3, R = 999)
class(out2) <- "boot"
boot.percentile <- boot.ci(out2, type = "perc")$percent[4:5]
boot.norm <- boot.ci(out2, type = "norm")$normal[2:3]
boot.basic <- boot.ci(out2, type = "basic")$basic[4:5]
rbind(Percentile.Tb3, boot.percentile, boot.norm, boot.basic)


###################################################
### code chunk number 14: meboot.Rnw:882-885
###################################################
znp <- pvcm(Price ~ LMV + Tb3, data = ullwan, model = "within")
zplm <- plm(Price ~ LMV + Tb3, data = ullwan)
pooltest(zplm, znp)


###################################################
### code chunk number 15: meboot.Rnw:901-903
###################################################
gir <- plm(Price ~ LMV + Tb3, data = ullwan, model = "random")
coef(gir)


###################################################
### code chunk number 16: meboot.Rnw:910-924
###################################################
slopeTb3 <- slopeLMV <- rep(0, jboot)
for(j in 1:jboot) {
  frm <- data.frame(Subj = ullwan[,1], Tim = ullwan[,2],
    Price = Price.ens[,j], Tb3 = Tb3.ens[,j], LMV = LMV.ens[,j])
  frm <- pdata.frame(frm, 7)
  gip <- coef(plm(Price ~ LMV + Tb3, model = "random", data = frm))
  slopeTb3[j] <- gip[3]
  slopeLMV[j] <- gip[2]
}
Percentile.Tb3 <- quantile(slopeTb3, c(0.025, 0.975), type = 8)
Refined.Tb3 <- null.ci(slopeTb3)
Percentile.LMV <- quantile(slopeLMV, c(0.025, 0.975), type = 8)
Refined.LMV <- null.ci(slopeLMV)
rbind(Percentile.Tb3, Refined.Tb3, Percentile.LMV, Refined.LMV)


###################################################
### code chunk number 17: meboot.Rnw:933-943
###################################################
thetr <- plm(Price ~ LMV + Tb3, model = "random", data = ullwan)
varTb3 <- thetr$vcov[3,3]
plm1 <- coef(thetr)
t0Tb3 <- plm1[3]
out3 <- list(t = as.matrix(slopeTb3), t0 = t0Tb3, var.t0 = varTb3, R = 999)
class(out3) <- "boot"
boot.percentile <- boot.ci(out3, type = "perc")$percent[4:5]
boot.norm <- boot.ci(out3, type = "norm")$normal[2:3]
boot.basic <- boot.ci(out3, type = "basic")$basic[4:5]
rbind(boot.percentile, boot.norm, boot.basic)


