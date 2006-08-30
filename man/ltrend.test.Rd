\name{ltrend.test}
\alias{ltrend.test}
\title{Test for a linear trend in variances}
\description{
 The function performs a test for a linear trend in variances. The test statistic
is based on the classical Levene procedure (using the group means), 
the modified Brown-Forsythe Levene-type procedure (using the group medians)
or the modified Levene-type procedure (using the group trimmed means). 
}
\usage{
ltrend.test(y, group, option = c("mean", "median", 
            "trim.mean"), trim.alpha = 1)
}

\arguments{
  \item{y}{our data}
  \item{group}{factor of the data}
  \item{option}{the option must be "mean" corresponding to classical Levene's procedure 
                (default), "median" corresponding to the robust 
                Brown-Forsythe Levene-type procedure or "trim.mean" corresponding to the 
                robust Levene-type procedure using the group trimmed means.}
  \item{trim.alpha}{the fraction (0 to 0.5) of observations to be trimmed from
          each end of 'x' before the mean is computed.}
}

\value{
A list with the following numeric components.
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test was performed.}
  \item{data.name}{a character string giving the name of the data.}
}
\references{

Brown, M.~B. and Forsythe, A.~B. (1974). \emph{Robust tests for equality of 
variances.} Journal of the American Statistical Association, 69, 364-367.\cr

Gastwirth, J.~L., Gel, Y.~R., and Miao, W. (2006). \emph{The Impact of Levene's Test of Equality 
of Variances on Statistical Theory and Practice}. Working paper, 
Department of Statistics, George Washington University.\cr

Levene, H. (1960). \emph{Robust Testes for Equality of Variances}, in Contributions 
to Probability and Statistics, ed. I. Olkin, Palo Alto, CA: Stanford Univ. Press.\cr

Lim,T.-S., Loh, W.-Y. (1996) \emph{A comparison of tests of equality of variances}
Computational Statistical \& Data Analysis 22, 287-301.

}
\author{Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{\code{\link{levene.test}}}
\examples{
data(pot)
ltrend.test(pot[,"obs"], pot[,"type"], option="trim.mean", trim.alpha=0.25)


##      ltrend test based on the modified Levene-type procedure using the
##      group trimmed means
##
## data:  pot[,"obs"] 
## Test Statistic = -1.683, p-value = 0.0004288


}
\keyword{htest}
