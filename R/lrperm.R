`lrperm` <-
function(y,xx,resid,size){
sel<-sample(1:size,rep=FALSE)
x<-cbind(resid[sel],xx)
return(glm.fit(x,y,family=binomial())$deviance)
}

