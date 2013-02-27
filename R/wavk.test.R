wavk.test <-
function(x, factor.length=c("user.defined", "adaptive.selection"), 
window=round(0.1*length(x)), q=3/4, range=c(8:11), B=1000, 
H0=c("no trend","linear"), method=c("boot", "asympt"), 
ARorder=NULL, BIC=TRUE, out=FALSE)
{
if (NCOL(x) > 1 | !is.numeric(x)) {
          stop("x is not a vector or univariate time series")
}
if (any(is.na(x))) {
          stop("x contains missing values")
}
if (any(ls() == "factor.length")) {
    factor.length <- match.arg(factor.length)
}
else {
    factor.length <- "user.defined"
}
if (NCOL(q) > 1 | !is.numeric(q) | NROW(q) > 1) {
          stop("q is not a scalar")
}
if (q >= 1 | q <= 0) {
          stop("q is out of bounds from 0 to 1")
}
if (!is.vector(range) | !is.numeric(range)) {
          stop("range is not a numeric vector")
}
if (factor.length == "user.defined") {
    kn <- window[1]
}
else {
    kn <- length(x)*q^range
}
kn <- unique(sort(floor(kn)))
kn <- kn[kn > 1]
kn <- kn[kn < length(x)]
if (length(kn) == 0) {
          stop("set a proper window")
}
if (factor.length == "adaptive.selection" && length(kn) < 3) {
          stop("Number of possible windows is not enough for adaptive selection. Change parameters 'q' and/or 'range'.")
}
if (factor.length == "adaptive.selection") {
    method <- "boot"
}
B <- round(B)
if (B <= 0) {
          stop("number of bootstrap samples B must be positive")
}
H0 <- match.arg(H0)
method <- match.arg(method)
if (!is.null(ARorder) && ARorder < 0) {
          stop("ARorder must be non-negative")
}
if (!is.null(ARorder) & (NCOL(ARorder) > 1 | !is.numeric(ARorder) | NROW(ARorder) > 1)) {
    stop("ARorder is not a scalar")
}

WAVK <- function(y, kn, pvalue=FALSE) 
{
    T <- length(y)
    tmp <- matrix(0, T-kn+1, kn)
    for(j in 1:(T-kn+1)) {
  tmp[j,] <- y[j:(j+kn-1)]
    }
    ave_all <- mean(tmp)
    ave_group <- apply(tmp, 1, mean)
    MST <- sum((ave_group - ave_all)^2)*(kn / (T - kn))
    MSE <- 0
    for (j in 1:kn) {
  MSE <- MSE+sum((tmp[,j] - ave_group)^2)
    }
    MSE <- MSE/((T-kn+1)*(kn-1))
    Tn <- MST - MSE
    sigma2 <- sum(diff(y)^2)/(2*(T-1))
    st <- sqrt(T/kn)*Tn/(sqrt(4/3)*sigma2)
    crit <- pnorm(sqrt(T/kn)*Tn, mean=0, sd=sqrt(4/3)*sigma2)
    if (crit < 0.5) {
        pv <- crit*2
    }
    else {
        pv <- (1-crit)*2
    }
    if (pvalue) {
        list(st=st, pv=pv)
    } 
    else {
        return(st)
    }
}
DNAME <- deparse(substitute(x))
n <- length(x)
t <- (1:n)/n
result <- matrix(NA, length(kn), 2)
res <- matrix(NA, 1, 2)

      if (H0 == "linear") {
    ALTERNATIVE <- "presence of a nonlinear trend"
    mod <- lm(x ~ t)
          linear <- mod$coefficients
          beta0 <- mod$coefficients[1]
          beta1 <- mod$coefficients[2]
    n_tmp <- length(mod$residuals)
          if (is.null(ARorder)) {
        bic <- rep(NA, floor(10*log10(n_tmp)))
        for (i in 1:length(bic)) {
            a <- ar(mod$residuals, aic=FALSE, order.max=i, demean=TRUE)
            bic[i] <- n_tmp*log(a$var.pred) + i*log(n_tmp)
        }
        a <- ar(mod$residuals, aic=FALSE, order.max=which.min(bic), demean=TRUE)
              pheta <- a$ar
        names(pheta) <- paste(rep("a", length(pheta)), c(1:length(pheta)), sep="")
          }
    else if (ARorder > 0) {
        if (BIC) {
            bic <- rep(NA, ARorder)
            for (i in 1:length(bic)) {
                a <- ar(mod$residuals, aic=FALSE, order.max=i, demean=TRUE)
                bic[i] <- n_tmp*log(a$var.pred) + i*log(n_tmp)
            }
            a <- ar(mod$residuals, aic=FALSE, order.max=which.min(bic), demean=TRUE)
              }
        else {
            a <- ar(mod$residuals, aic=FALSE, order.max=ARorder, demean=TRUE)
        }
        pheta <- a$ar
        names(pheta) <- paste(rep("a", length(pheta)), c(1:length(pheta)), sep="")
          }
    else {
        pheta <- numeric(0)
    }
          p <- length(pheta)
          if (p > 0) {
              tmp <- filter(x, pheta, sides=1)
              tmp2 <- filter(mod$fitted.values, pheta, sides=1)
              Z <- (x[(p+1):n] - tmp[p:(n-1)]) - (mod$fitted.values[(p+1):n] - tmp2[p:(n-1)])
          } 
    else {
        Z <- mod$residuals
    }
    ESTIMATE <- list(linear, pheta)
    names(ESTIMATE) <- c("linear_trend_coefficients", "AR_coefficients")
}
else {
    H0 <- "no trend"
    ALTERNATIVE <- "presence of a trend" 
          n_tmp <- length(x)
          if (is.null(ARorder)) {
        bic <- rep(NA, floor(10*log10(n_tmp)))
        for (i in 1:length(bic)) {
            a <- ar(x, aic=FALSE, order.max=i, demean=TRUE)
            bic[i] <- n_tmp*log(a$var.pred) + i*log(n_tmp)
        }
        a <- ar(x, aic=FALSE, order.max=which.min(bic), demean=TRUE)
              pheta <- a$ar
        names(pheta) <- paste(rep("a", length(pheta)), c(1:length(pheta)), sep="")
              Z <- na.omit(a$resid)
          } 
    else if (ARorder > 0) {
        if (BIC) {
            bic <- rep(NA, ARorder)
            for (i in 1:length(bic)) {
                a <- ar(x, aic=FALSE, order.max=i, demean=TRUE)
                bic[i] <- n_tmp*log(a$var.pred) + i*log(n_tmp)
            }
            a <- ar(x, aic=FALSE, order.max=which.min(bic), demean=TRUE)
              }
        else {
            a <- ar(x, aic=FALSE, order.max=ARorder, demean=TRUE)
        }
              pheta <- a$ar
        names(pheta) <- paste(rep("a", length(pheta)), c(1:length(pheta)), sep="")
              Z <- na.omit(a$resid)
          } 
    else {
              pheta <- numeric(0)
              Z <- x
          }
    ESTIMATE <- list(pheta)
    names(ESTIMATE) <- c("AR_coefficients")
}
Z <- Z - mean(Z)
      sigma <- sqrt(sum(diff(Z)^2)/(2*(length(Z)-1)))
      if (method == "asympt") {
    for (i in 1:length(kn)) {
        tmp <- WAVK(Z, kn[i], pvalue=TRUE)
              result[i,] <- c(tmp$st, tmp$pv)
    }
          STATISTIC <- result[1,1]
    P.VALUE <- result[1,2]
    PARAMETER <- kn
    names(PARAMETER) <- "user-defined window"
      }
else {
          boot <- array(data=rnorm(n*B), c(n, B))*sigma
          s <- array(data=NA, c(length(kn), B))
          for (i in 1:length(kn)) {
              s[i,] <- apply(boot, 2, WAVK, kn=kn[i])
              result[i,1] <- WAVK(Z, kn[i])
              crit <- sum(result[i,1] < s[i,])/B
        if (crit < 0.5) {
                  result[i,2] <- 2*crit
              }
        else {
            result[i,2] <- 2*(1-crit)
        }
          }
          if (length(kn) < 3) {
              STATISTIC <- result[1,1]
        P.VALUE <- result[1,2]
        PARAMETER <- kn
        names(PARAMETER) <- "user-defined window"
          } 
    else {
              distance <- rep(NA, length(kn)-1)
              for (i in 1:length(distance)) {
                  distance[i] <- dist(rbind(sort(s[i,]), sort(s[i+1,])))
              }
              kn_opt <- kn[which.min(distance)]
              res[1,] <- result[which.min(distance),]
        STATISTIC <- res[1,1]
        P.VALUE <- res[1,2]
        PARAMETER <- kn_opt
        names(PARAMETER) <- "adaptively selected window"
        if (out) {
            tmp <- names(ESTIMATE)
            ESTIMATE <- c(ESTIMATE, NA)
            names(ESTIMATE) <- c(tmp, "all_considered_windows")
ESTIMATE[[length(ESTIMATE)]] <- cbind(kn, result)
dimnames(ESTIMATE[[length(ESTIMATE)]]) <- list(rep("", length(kn)), c("window", "WAVK-statistic", "p-value"))
        }
          }
}
METHOD <- "Trend test by Wang, Akritas and Van Keilegom"
names(STATISTIC) <- "WAVK test statistic"
if (out) {
    structure(list(method = METHOD, data.name = DNAME, statistic = STATISTIC, p.value = P.VALUE, alternative = ALTERNATIVE, parameter = PARAMETER, estimate = ESTIMATE), class = "htest") 
}
else {
    structure(list(method = METHOD, data.name = DNAME, statistic = STATISTIC, p.value = P.VALUE, alternative = ALTERNATIVE, parameter = PARAMETER), class = "htest") 
}
}
