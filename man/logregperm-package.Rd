\name{logregperm-package}
\alias{logregperm-package}
\alias{logregperm}
\docType{package}
\title{
Inference in Logistic Regression
}
\description{
A permutation test is used for inference in logistic regression.
The procedure is useful when parameter estimates in ordinary 
logistic regression fail to converge or are unreliable due to
small sample size, or when the conditioning in exact conditional
 logistic regression restricts the sample space too severely 
(as when independent variables are continuous).
}
\details{
\tabular{ll}{
Package: \tab logregperm\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-03-18\cr
License: \tab GPL-3\cr
}
The function prr.test requires that the user identify one independent 
variable for which a test of the null hypothesis (that the regression
coefficient is zero) is to be performed. This variable and the dependent binary
 variable are supplied as vector arguments; the remaining
independent variables are supplied as a matrix.
}
\author{Douglas M. Potter

Maintainer: Douglas M Potter <potter@upci.pitt.edu>
}
\references{
Potter D.M. (2005) \emph{A permutation test for inference in 
logistic regression with small- and moderate-sized datasets}. 
Statistics in Medicine, 24:693-708.
}

\keyword{htest}

\examples{

##40 observations, 3 independent variables

nobs<-40

x1<-rnorm(nobs)
x2<-rnorm(nobs)
xx<-cbind(x1,x2)

x0<-rnorm(nobs)+x1+x2

y<-x0+x1+x2+2*rnorm(nobs)
y<-ifelse(y>0,1,0)

prr.test(y,x0,xx)

##compare prr.test with ordinary logistic regression 
##using a likelihood ratio test

t1<-glm(y~x0+x1+x2,family=binomial)
t2<-glm(y~x1+x2,family=binomial)
1-pchisq(abs(anova(t1,t2)$Deviance[2]),1)


}
