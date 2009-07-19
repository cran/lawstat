\name{cmh.test}
\alias{cmh.test}
\title{The Cochran-Mantel-Haenszel Chi-square Test}
\description{
This function performs the Cochran-Mantel-Haenszel (CMH) procedure. The CMH procedure tests independence 
of two variables in each stratum, conditionally on the levels of a third variable. This procedure is 
widely used in various law cases, in particular, on equal employment and discrimination, as well in 
biopharmaceutical studies (Agresti, 2002; Gastwirth, 1984; Hall et al., 1999).
}

\usage{
cmh.test(x)
}
\arguments{
  \item{x}{a numeric 2 x 2 x k array of data values.}
}

\details{
The data input is in a form of an array of 2 rows x 2 columns x k levels. The output includes the 
Cochran-Mantel-Haenszel (CMH) test statistic which is chi-squared distributed with 1 degree of freedom, 
the corresponding p-value for the CMH statistic, the Mantel-Haenszel odds ratio estimator, the direct 
pooled odd ratio estimator, and the odd ratios between the rows and columns at each level. The null 
hypothesis is that two variables in each stratum are independent, conditionally on the levels of a 
third variable. Hence, under the null hypothesis, estimators of odds ratio, i.e. the Mantel-Haensel and 
direct pooled estimators, are to be 1.
}


\value{
A list with class htest containing the following components:

  \item{cmh}{the Cochran-Mantel-Haenszel test statistic (chi-squared distributed).
}
  \item{df}{degrees of freedom of the Cochran-Mantel-Haenszel test statistic (equal to 1).}
  \item{p.value}{the p-value of the Cochran-Mantel-Haenszel test.}
  \item{MH.ESTIMATE}{the Mantel-Haenszel odds ratio estimator.}
  \item{OR}{Direct Pooled Odds Ratio Estimator of the data.}
  \item{ORK}{vector of Odd Ratios for each level 
data.}
  \item{DNAME}{a character string giving the name of the data.}
  \item{method}{type of the performed test.}
}

\author{Min Qin, W. Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth}
\references{
Agresti, A. (2002). \emph{Categorical data analysis}. (second edition). New York: Wiley.

Gastwirth, J. L.(1984) \emph{Statistical Methods for Analyzing Claims of Employment Discrimination}, 
Industrial and Labor Relations Review, Vol. 38, No. 1. (October 1984), pp. 75-86.\cr

Hall, D.B., Woolson, R.F., Clarke, W.R., Jones, M.F. (1999). \emph{Cochran-Mantel-Haenszel Techniques: 
Applications Involving Epidemiologic Survey Data}. In: Handbook of Statistics Volume 17: 
Bio-Environmental and Public Health Statistics. PK Sen and CR Rao, Eds. New York, NY: John Wiley and Sons.


}

\seealso{mantelhaen.test (in stats)}

\examples{

## Sample Salary Data

data(blackhire)
cmh.test(blackhire)


## Sample Output
##
##        Cochran-Mantel-Haenszel Chi-square Test
##
## data:  blackhire 
## CMH statistic = 145.840, df = 1.000, p-value = 0.000, 
## MH Estimate = 0.477, Pooled Odd Ratio = 0.639 
## Odd Ratio of level 1 = 1.329, Odd Ratio of level 2 = 0.378, 
## Odd Ratio of level 3 = 0.508, Odd Ratio of level 4 = 0.357, 
## Odd Ratio of level 5 = 0.209, Odd Ratio of level 6 = 0.412, 
## Odd Ratio of level 7 = 0.250, Odd Ratio of level 8 = 0.820
## 
## Note: P-value is significant and we should reject the null hypothesis.

}
\keyword{htest}
