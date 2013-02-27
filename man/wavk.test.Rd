\name{wavk.test}
\alias{wavk.test}
\title{WAVK test}
\description{
This function performs a nonparametric test to detect possibly nonmonotonic parametric trend in time series.
}

\usage{
wavk.test(x, factor.length=c("user.defined", "adaptive.selection"), 
	window=round(0.1*length(x)), q=3/4, range=c(8:11), B=1000, 
	H0=c("no trend","linear"), method=c("boot", "asympt"), 
	ARorder=NULL, BIC=TRUE, out=FALSE)
}
\arguments{
  \item{x}{a numeric vector of time series values. NA or character is not allowed.}

  \item{factor.length}{the method to define the length of local windows (factors). The option "user.defined" (default) 
	allows to set one value of the argument 'window'. The option "adaptive.selection" employs the heuristic 
	m-out-of-n subsampling algorithm (Bickel and Sakov, 2008) to select an optimal window among the set of possible 
	windows defined as length(x)*q^range (see range for details).}

  \item{window}{the length of the local window (factor), default value is round(0.1*length(x)). This argument is ignored 
	if factor.length="adaptive.selection".}

  \item{q}{is a scalar from 0 to 1 to define the vector of possible windows when the factor.length="adaptive.selection". 
	Default is 3/4. This argument is ignored if factor.length="user.defined".}

  \item{range}{a numeric vector to define the set of possible windows when the factor.length="adaptive.selection". 
	Default is c(8:11). This argument is ignored if factor.length="user.defined".}

  \item{B}{number of bootstrap simulations to obtain empirical critical values. Default is 1000.}

  \item{H0}{the testing hypothesis, must be "no trend" (default) for testing the presence of no trend vs. 
	any possibly nonmonotonic trend or "linear" for testing the presence of linear trend vs. nonlinear trend.}

  \item{method}{method of obtaining critical values: from bootstrap distribution ("boot"), i.e. the default option, 
	or asymptotic distribution of the test statistic ("asympt").}

  \item{ARorder}{the order of autoregressive filter when BIC is FALSE, or the maximal order for BIC-based filtering.}

  \item{BIC}{logical value indicates whether the order of autoregressive filter should be selected by BIC.}

  \item{out}{logical value indicates whether the sample estimates should be shown in the output. Default is FALSE.}
}


\value{
A list with class htest containing the following components:

  \item{method}{the name of the method.}

  \item{data.name}{a character string giving the name of the data.}

  \item{statistic}{the value of the test statistic.}

  \item{p.value}{the p-value of the test.}

  \item{alternative}{the alternative hypothesis.}

  \item{parameter}{the selected window.}

  \item{estimate}{a list, containing the estimated coefficients of linear trend (if H0="linear"); estimated AR coefficients; test results for all considered windows (if the factor.length="adaptive.selection").}
}

\references{
Bickel, P.J., Sakov, A. 2008. \emph{On the choice of m in the m out of n bootstrap and confidence bounds for extrema}, Statistica Sinica 18, 967-985.

Lyubchich, V., Gel, Y.,  El-Shaarawi, A., 2013. On Detecting Non-Monotonic Trends in Environmental Time Series: a Fusion of Local Regression and Bootstrap. 
\emph{Proceedings of the New Directions in Monte Carlo Methods}, University of Florida, January 2013.

Wang, L., Akritas, M. G. and Van Keilegom, I. 2008. \emph{An ANOVA-type nonparametric diagnostic test for heteroscedastic regression models}, Journal of Nonparametric Statistics, 20(5), 365-382.

Wang, L. and Van Keilegom, I. 2007. \emph{Nonparametric test for the form of parametric regression with time series errors}, Statistica Sinica 17, 369-386.
}

\author{Vyacheslav Lyubchich, Yulia R. Gel, Abdel El-Shaarawi}

\examples{

## Simulate autoregressive time series process with linear trend.
n=100
x <- 1:n; t <- x/n
U <- 1+2*t + arima.sim(n=n, list(order = c(2,0,0), ar = c(-0.7, -0.1)))


## Testing for presence of the trend.
wavk.test(U, factor.length = "adaptive.selection", B=100)

## Sample Output
##
##          Trend test by Wang, Akritas and Van Keilegom
##
## data:  U 
## WAVK test statistic = -2.0502, adaptively selected window = 11, p-value = 0.01
## alternative hypothesis: presence of a trend 


## Using the asymptotic distribution.
wavk.test(U, window=11, method="asympt")

## Sample Output
##
##        Trend test by Wang, Akritas and Van Keilegom
##
## data:  U 
## WAVK test statistic = -2.0502, user-defined window = 11, p-value = 0.04034
## alternative hypothesis: presence of a trend 


## Testing for linear trend with output of all results.
wavk.test(U, factor.length = "adaptive.selection", H0="linear", out=TRUE, B=100)
	
## Sample Output
##
##         Trend test by Wang, Akritas and Van Keilegom
## 
## data:  U 
## WAVK test statistic = -1.2729, adaptively selected window = 15, p-value = 0.254
## alternative hypothesis: presence of a nonlinear trend 
## sample estimates:
## $linear_trend_coefficients
## (Intercept)           t 
##   0.8842473   2.1631535 
## 
## $AR_coefficients
##         a1 
## -0.5814447 
## 
## $all_considered_windows
##  window WAVK-statistic p-value
##       8      -1.741544   0.062
##      11      -1.601250   0.100
##      15      -1.272851   0.254
##      20      -1.091175   0.406



}
\keyword{trend}

