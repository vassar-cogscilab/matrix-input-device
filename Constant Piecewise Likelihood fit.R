
install.packages('DEoptim')

library('DEoptim')

##likelihood Normal equal sd##

log.likelihood<-function(x,mu){
  sigma=10
  return(dnorm(x,mu,sigma,log=T))
}

likelihood<-function(data, mu){
  log.lik<-sum(mapply(log.likelihood,data,mu))
  return(log.lik)
}




##Model predit##

constant.piecewise.model.predict<-function(t,a,b,c){
  if(t<c){
    rt<-a
  }
  if(t>=c){
    rt<-b
  }
  return(rt)
}


##fit function##
subject.fit.MLE.constant.piecewise<-function(subject.data){
  
  subject.data<-subject.data

  constant.piecewise.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    subject.data$model.predict<-mapply(constant.piecewise.model.predict,t,a,b,c)
    lik<-likelihood(subject.data$difference, subject.data$model.predict)
    return(-lik)
  }

  constant.piecewise<-DEoptim(constant.piecewise.fit,lower=c(-2001,-2001,0),upper=c(2001,2001,72))

  constant.piecewise.values<-list('intercept'=constant.piecewise$optim$bestmem[1],'intercept 2'=constant.piecewise$optim$bestmem[2],'split'=constant.piecewise$optim$bestmem[3], 'Likelihood'=constant.piecewise$optim$bestval)

  return(constant.values)
}


subject.fit.MLE(data.by.subject[[200]])