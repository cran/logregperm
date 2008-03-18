\name{prr.test}
\alias{prr.test}
\title{Inference in Logistic Regression}
\description{
A permutation test is used for inference in logistic regression.
The procedure is useful when parameter estimates in ordinary 
logistic regression fail to converge or are unreliable due to
small sample size, or when the conditioning in exact conditional
 logistic regression restricts the sample space too severely 
(as when independent variables are continuous).
}
\usage{
prr.test(y, x0, xx, nrep = 1000)
}
\arguments{
  \item{y}{the dependent binary variable; a vector}
  \item{x0}{the independent variable about which inference
 is to be made; a vector}
  \item{xx}{all other independent variables in a matrix in which
rows correspond to observations and each column to a variable}
  \item{nrep}{number of Monte Carlo replicates to be used for the test}
}
\details{The prr test replaces the independent variable of interest 
with its residuals from a linear regression on the other independent
variables. The test statistic for the permutation test is the p-value
based on a likelihood ratio test in an ordinary logistic regression.
 Thus, the permutation p-value is the fraction of the permutations 
that have a likelihood-based p-value less than or equal to that for 
the unpermuted data.  Because the p-values for permutations will be a 
discrete set and round-off errors may occur, prr.test also 
investigates the sensitivity of the permutation p-value to small
 variations in the unpermuted likelihood-based p-value.

Missing values are allowed, and observations with them are eliminated.

prr.test calls lrperm, and prints out the execution time in minutes.}
\value{
  \item{nobs}{number of observations used}
  \item{p0}{permutation p-value for simulated p-values
 less than or equal to the observed p-value.}
  \item{p005}{permutation p-value for simulated p-values
 less than or equal to 1.005 times the observed p-value.}
  \item{p01}{permutation p-value for simulated p-values
 less than or equal to 1.01 times the observed p-value. 
This is a good choice for inference.}
  \item{p02}{permutation p-value for simulated p-values
 less than or equal to 1.02 times the observed p-value.}
  \item{p04}{permutation p-value for simulated p-values
 less than or equal to 1.04 times the observed p-value.}
}
\references{
Potter D.M. (2005) \emph{A permutation test for inference in 
logistic regression with small- and moderate-sized datasets}. 
Statistics in Medicine, 24:693-708.
}
\author{Douglas M. Potter}


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


## The function is currently defined as
function(y,x0,xx,nrep=1000){
sel<-rep(TRUE,length(y))
for(i in 1:length(y)){
sel[i]<-ifelse(is.na(sum(y[i]+x0[i]+xx[i,])==TRUE),FALSE,TRUE)
  }
y<-y[sel]
x0<-x0[sel]
size<-length(y)
len<-length(xx[1,])
xxx<-rep(0,len*size)
xxx<-matrix(xxx,ncol=len)
for(i in 1:len)xxx[,i]<-xx[sel,i]
xint<-rep(1,size)
resid<-lm.fit(cbind(xint,xxx),x0)$residuals
t1<-glm.fit(cbind(xint,xxx,resid),y,family=binomial())
t2<-glm.fit(cbind(xint,xxx),y,family=binomial())
p.value.obs<-1 - pchisq(abs(t1$deviance - t2$deviance), 1)
devi<-rep(0,nrep)
#don't print warning messages from glm.fit. will get when no convergence, or exact fit.
options(warn=-1)
oldtime<-proc.time()[1]
for(i in 1:nrep)devi[i]<-lrperm(y,cbind(xint,xxx),resid,size)
print(c("execution time in minutes",round((proc.time()[1]-oldtime)/60,2)))
options(warn=0)#reset options to default
psim<-1 - pchisq(abs(devi - t2$deviance), 1)#vector of p-values from simulations

#compute prr p-values and explore sensitivity to discreteness

ret.val<-list(nobs=size,
p0=length(psim[psim<=p.value.obs])/nrep,
p005=length(psim[psim<=1.005*p.value.obs])/nrep,
p01=length(psim[psim<=1.01*p.value.obs])/nrep,
p02=length(psim[psim<=1.02*p.value.obs])/nrep,
p04=length(psim[psim<=1.04*p.value.obs])/nrep)

names(ret.val$nobs)<-"number of observations used"
names(ret.val$p0)<-"permutation p-value for simulated p-values <= observed p-value"
names(ret.val$p005)<-"permutation p-value for simulated p-values <= 1.005 observed p-value"
names(ret.val$p01)<-"permutation p-value for simulated p-values <= 1.01 observed p-value"
names(ret.val$p02)<-"permutation p-value for simulated p-values <= 1.02 observed p-value"
names(ret.val$p04)<-"permutation p-value for simulated p-values <= 1.04 observed p-value"

return(ret.val)
  }
}

\keyword{htest}

