\name{levene.test}
\alias{levene.test}
\title{Levene's Test of Equality of Variances}
\description{
The function performs the following tests for equality of the k population variances:
classical Levene's test, the robust Brown-Forsythe Levene-type 
test using the group medians and the robust Levene-type test using 
the group trimmed mean. NAs from the data are omitted.
}
\usage{
levene.test(y, group, option = c("mean", "median",
            "trim.mean"), trim.alpha = 0.25)
}
\arguments{
  \item{y}{our data}
  \item{group}{factor of the data}
  \item{option}{The option must be "mean" corresponding to classical Levene's test 
                (default), "median" corresponding to the robust 
                Brown-Forsythe Levene-type test or "trim.mean" corresponding to the 
                robust Levene-type test using the trimmed mean.}
  \item{trim.alpha}{the fraction (0 to 0.5) of observations to be trimmed from
          each end of 'x' before the mean is computed.}
}
\details{
  Levene (1960) proposed a test for homogeneity of variances in k groups which
is based on the ANOVA statistic applied to absolute deviations of observations 
from the corresponding group mean. The robust Brown-Forsythe version
of the Levene-type test substites the group mean by the group median in the classical 
Levene statistic. The third option is to consider ANOVA applied to the 
absolute deviations of observations from the group trimmed mean instead of 
the group means.  

}
\value{
A list with the following numeric components.
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{type of test was performed.}
  \item{data.name}{a character string giving the name of the data.}
}

\note{modified from a response posted by Brian Ripley to the R-help e-mail list.}

\references{

Brown, M.~B. and Forsythe, A.~B. (1974). \emph{Robust tests for equality of 
variances.} Journal of the American Statistical Association, 69, 364-367.\cr

Gastwirth, J.~L., Gel, Y.~R., and Miao, W. (2006). \emph{The Impact of Levene's Test of Equality 
of Variances on Statistical Theory and Practice}. Working paper, 
Department of Statistics, George Washington University.\cr

Levene, H. (1960). \emph{Robust Tests for Equality of Variances}, in Contributions 
to Probability and Statistics, ed. I. Olkin, Palo Alto, CA: Stanford Univ. Press.\cr

Lim,T.-S., Loh, W.-Y. (1996) \emph{A comparison of tests of equality of variances}
Computational Statistical \& Data Analysis 22, 287-301

}


\author{W. Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\seealso{\code{\link{ltrend.test}}}
\examples{

data(pot)
levene.test(pot[,"obs"], pot[,"type"], option="trim.mean", trim.alpha=0.25)

##        Modified Robust Levene's test based on the absolute deviations from
##        the trimmed mean
##
## data:  pot[,"obs"] 
## Test Statistic = 6.5673, p-value = 0.001591

}
\keyword{htest}
