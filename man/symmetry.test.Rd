\name{symmetry.test}
\alias{symmetry.test}
\title{Test of Symmetry}
\description{
This function performs the Bartels test for randomness which is
based on the ranked version of von Neumann's ratio (RVN). Users can
choose whether to test against two-sided, negative or positive
correlation.
}
\usage{
symmetry.test(x, option = c("mgg.test", "cabilio.masaro.test", 
              "mira.test"))
}
\arguments{
  \item{x}{the input data to be tested for symmetry.}
  \item{option}{give the choice to test statistics.}
}
\value{
A list with the following numeric components.
  \item{p.value}{the p-value for the test.}
  \item{statisitc}{observed test statistic.}
  \item{data.name}{a character string giving the name of the data.}
}
\references{
Cabilio, P. and Masaro, J. (1996) A simple test of symmetry about
an unknown median. \emph{The Canadian Journal of Statistics}, 24,
349-361.\cr

Miao, W., Gel, Y. R., and Gastwirth, J. L. (2006) \emph{A New Test
of Symmetry about an Unknown Median.} Random Walk, Sequential
Analysis and Related Topics - A Festschrift in Honor of Yuan-Shih
Chow. Eds.: Agnes Hsiung, Cun-Hui Zhang, and Zhiliang Ying, World
Scientific Publisher, Singapore.\cr

Mira, A. (1999) Distribution-free test for symmetry based on
Bonferroni's measure. \emph{Journal of Applied Statistics}, 26,
959-972.}
\author{Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao}
\examples{
data(zuni)
symmetry.test(zuni[,"Revenue"])

##        Test of Symmetry - MGG Test
##
## data:  zuni[, "Revenue"] 
## Test Statistic = 5.0321, p-value = 4.851e-07

}
\keyword{htest}
\keyword{distribution}
