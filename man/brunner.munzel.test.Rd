\name{brunner.munzel.test}
\alias{brunner.munzel.test}
\title{The Brunner-Munzel Test for Stochastic Equality}
\description{
This function performs the Brunner-Munzel test for stochastic
equality of two samples, which is also known as the Generalized Wilcoxon
Test. NAs from the data are omitted.
}
\usage{
brunner.munzel.test(x, y, alternative = c("two.sided", "greater",
"less"), alpha=0.05)
}
\arguments{
    \item{x}{the numeric vector of data values from the sample 1.}
    \item{y}{the numeric vector of data values from the sample 2.}
    \item{alpha}{confidence level, default is 0.05 for 95% confidence
          interval.}
    \item{alternative}{a character string specifying the alternative
         hypothesis, must be one of 'two.sided' (default), 'greater' or
            'less'. User can specify just the initial letter.}
}

\value{
A list containing the following components:
    \item{statistic}{the Brunner-Munzel test statistic.}
    \item{parameter}{the degrees of freedom.}
    \item{conf.int}{the confidence interval.}
    \item{p.value}{the p-value of the test.}
    \item{data.name}{a character string giving the name of the data.}
    \item{estimate}{an estimate of the effect size, i.e. P(X<Y)+.5*P(X=Y)}
}
\references{
Brunner, E. and Munzel, U. (2000) \emph{The Nonparametric
Behrens-Fisher Problem: Asymptotic Theory and a Small-Sample
Approximation}, Biometrical Journal 42, 17-25.\cr

Reiczigel, J., Zakarias, I. and Rozsa, L. (2005) \emph{A
Bootstrap Test of Stochastic Equality of Two Populations}, The
American Statistician 59, 1-6.
}
\author{
Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao
This function was updated with the help of Dr. Ian Fellows
}
\seealso{\code{wilcox.test}, \code{pwilcox}}
\examples{
## Pain score on the third day after surgery for 14 patients under
## the treatment \emph{Y} and 11 patients under the treatment \emph{N}
## (see Brunner and Munzel (2000))

Y<-c(1,2,1,1,1,1,1,1,1,1,2,4,1,1)
N<-c(3,3,4,3,1,2,3,1,1,5,4)

brunner.munzel.test(Y, N)

##       Brunner-Munzel Test
## data: Y and N
## Brunner-Munzel Test Statistic = 3.1375,  df = 17.683, p-value = 0.005786
## 95 percent confidence interval:
##  0.5952169 0.9827052
## sample estimates:
## P(X<Y)+.5*P(X=Y)
##        0.788961
}
\keyword{htest}
\keyword{nonparametric}

