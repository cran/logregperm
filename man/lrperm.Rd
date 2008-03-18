\name{lrperm}
\alias{lrperm}
\title{Deviance for a Logistic Regression with Permuted Variables}
\description{
 lrperm permutes the residuals from a linear regression of the independent
variable of interest on the remaining independent variables, and then
computes the deviance from a logistic regression of these residuals and
the other independent variables on the dependent variable.  This
 function is intended to be used only by prr.test
}
\usage{
lrperm(y, xx, resid, size)
}
\arguments{
  \item{y}{the dependent binary variable; a vector.}
  \item{xx}{independent variables about which inference is not to be
made in a matrix in which rows correspond to observations and each 
column to a variable. xx must include a vector of 1s for the intercept.}
  \item{resid}{a vector of residuals from a linear regression of the 
variable of interest on the remaining variables (in xx).}
  \item{size}{number of observations used.}
}

\value{lrperm returns the deviance as described above}

\references{
Potter D.M. (2005) \emph{A permutation test for inference in 
logistic regression with small- and moderate-sized datasets}. 
Statistics in Medicine, 24:693-708.
}
\author{Douglas M. Potter}

\examples{
nobs<-40
x1<-rnorm(nobs)
x2<-rnorm(nobs)
xx<-cbind(x1,x2)
xint<-rep(1,nobs)

x0<-rnorm(nobs)+x1+x2
y<-x0+x1+x2+2*rnorm(nobs)
y<-ifelse(y>0,1,0)

resid<-lm(x0~x1+x2)$residuals

lrperm(y,cbind(xint,xx),resid,nobs)

## The function is currently defined as
function(y,xx,resid,size){
sel<-sample(1:size,rep=F)
x<-cbind(resid[sel],xx)
return(glm.fit(x,y,family=binomial())$deviance)
  }
}

\keyword{htest}

