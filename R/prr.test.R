`prr.test` <-
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

