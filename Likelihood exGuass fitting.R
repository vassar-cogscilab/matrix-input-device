#install.packages("jsonlite")
install.packages('DEoptim')
#install.packages("LSPM")
#install.packages("rgenoud")
install.packages("retimes")
library("retimes")
library("rgenoud")
library("LSPM")
library('DEoptim')
library(jsonlite)


?DEoptim


##likelihood Normal equal sd##

log.likelihood<-function(x,mu,sigma,tau){
  return(log(dexgauss(x,mu,sigma, tau)))
}

likelihood<-function(data, mu, sigma, tau){
  log.lik<-sum(mapply(log.likelihood,data,mu,MoreArgs=list(sigma,tau)))
  return(log.lik)
}


##

##Model predit##
linear.model.predict<-function(t,a,b){
  rt<-a+b*t
  return(rt)
}

constant.model.predict<-function(s){
  rt<-s
  return(rt)
}

exponential.model.predict<-function(t,a,b,c){
  rt<-a+b*exp(c*t)
  return(rt)
}
power.model.predict<-function(t,a,b,c){
  rt<-a+b*t^(c)
  return(rt)
}
constant.piecewise.model.predict<-function(t,a,b,c){
  if(t<c){
    rt<-a
  }
  if(t>=c){
    rt<-b
  }
  return(rt)
}
linear.piecewise.model.predict<-function(t,a,b,c,d,e){
  if(t<c){
    rt<-a + b*t
  }
  if(t>=c){
    rt<-d+e*t
  }
  return(rt)
}
exponential.piecewise.model.predict<-function(t,a,b,c,d,e,f,g){
  if(t<d){
    rt<-a+b*exp(c*t)
  }
  if(t>=d){
    rt<-e+f*exp(g*t)
  }
  return(rt)
}
power.piecewise.model.predict<-function(t,a,b,c,d,e,f,g){
  if(t<d){
    rt<-a + b*t^(c)
  }
  if(t>=d){
    rt<-e + f*t^(g)
  }
  return(rt)
}



##fit function##
subject.fit.MLE.exGuass<-function(subject.data){
  
  subject.data<-subject.data
  
  constant.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    sigma<-params[2]
    tau<-params[3]
    subject.data$model.predict<-mapply(constant.model.predict,a)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma,tau)
    #lik<-mapply(likelihood,subject.data$difference,subject.data$model.predict, MoreArgs=list(sigma=sigma,tau=tau))
    return(-lik)
  }

  linear.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    sigma<-params[3]
    tau<-params[4]
    subject.data$model.predict<-mapply(linear.model.predict,t,a,b)
    #lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma,tau)
    lik<-mapply(likelihood,subject.data$difference,subject.data$model.predict, MoreArgs=list(sigma=sigma,tau=tau))
    return(-lik)
  }
  exponential.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    sigma<-params[4]
    tau<-params[5]
    subject.data$model.predict<-mapply(exponential.model.predict,t,a,b,c)
    #lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma,tau)
    lik<-mapply(likelihood,subject.data$difference,subject.data$model.predict, MoreArgs=list(sigma=sigma,tau=tau))
    return(-lik)
  }
  power.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    sigma<-params[4]
    tau<-params[5]
    subject.data$model.predict<-mapply(power.model.predict,t,a,b,c)
    #lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma,tau)
    lik<-mapply(likelihood,subject.data$difference,subject.data$model.predict, MoreArgs=list(sigma=sigma,tau=tau))
    return(-lik)
  }
  constant.piecewise.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    sigma<-params[4]
    tau<-params[5]
    subject.data$model.predict<-mapply(constant.piecewise.model.predict,t,a,b,c)
    #lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma,tau)
    lik<-mapply(likelihood,subject.data$difference,subject.data$model.predict, MoreArgs=list(sigma=sigma,tau=tau))
    return(-lik)
  }
  linear.piecewise.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    d<-params[4]
    e<-params[5]
    sigma<-params[6]
    tau<-params[7]
    subject.data$model.predict<-mapply(linear.piecewise.model.predict,t,a,b,c,d,e)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma,tau)
    return(-lik)
  }
  exponential.piecewise.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    d<-params[4]
    e<-params[5]
    f<-params[6]
    g<-params[7]
    sigma<-params[8]
    tau<-params[9]
    subject.data$model.predict<-mapply(exponential.piecewise.model.predict,t,a,b,c,d,e,f,g)
    #lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma,tau)
    lik<-mapply(likelihood,subject.data$difference,subject.data$model.predict, MoreArgs=list(sigma=sigma,tau=tau))
    return(-lik)
  }
  
  power.piecewise.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    d<-params[4]
    e<-params[5]
    f<-params[6]
    g<-params[7]
    sigma<-params[8]
    tau<-params[9]
    subject.data$model.predict<-mapply(power.piecewise.model.predict,t,a,b,c,d,e,f,g)
    #lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma,tau)
    lik<-mapply(likelihood,subject.data$difference,subject.data$model.predict, MoreArgs=list(sigma=sigma,tau=tau))
    return(-lik)
  }
  
  constant<-DEoptim(constant.fit, lower=c(-2001,0.01,1),upper=c(2001,100,100))
  linear<-DEoptim(linear.fit,lower=c(-2001, -27,0.01,0.01),upper=c(2001,27,100,100))
  exponential<-DEoptim(exponential.fit,lower=c(-2001,-5,-1,0.01,0.01),upper=c(2001,5,1,100,100))
  power<-DEoptim(power.fit,lower=c(-2001,-5,-1),upper=c(2001,5,1,100,100))
  constant.piecewise<-DEoptim(constant.piecewise.fit,lower=c(-2001,-2001,0,0.01,0.01),upper=c(2001,2001,72,100,100))
  linear.piecewise<-DEoptim(linear.piecewise.fit,lower = c(-2001,-27,0,-2001,-27,0.01,0.01),upper=c(2001,27,72,2001,27,100,100))
  exponential.piecewise<-DEoptim(exponential.piecewise.fit,lower = c(-2001,-5,-1,0,-2001,-1,-.2,0.01,0.01),upper=c(2001,1,.2,72,2001,1,.2,100,100))
  power.piecewise<-DEoptim(power.piecewise.fit,lower = c(-2001,-5,-1,0,-2001,-5,-1,0.01,0.01),upper=c(2001,5,1,72,2001,5,1,100,100))
  
  constant.values<-list('intercept'=constant$member$bestmemit[which.min(constant$member$bestvalit),1], 'sigma'=constant$member$bestmemit[which.min(constant$member$bestvalit),2],'tau'=constant$member$bestmemit[which.min(constant$member$bestvalit),3], 'Likelihood'=constant$member$bestvalit[which.min(constant$member$bestvalit)])
  linear.values<-list ('intercept'=linear$member$bestmemit[which.min(linear$member$bestvalit),1],'slope'=linear$member$bestmemit[which.min(linear$member$bestvalit),2],'sigma'=constant$member$bestmemit[which.min(constant$member$bestvalit),3],'tau'=constant$member$bestmemit[which.min(constant$member$bestvalit),4], 'Likelihood'=linear$member$bestvalit[which.min(linear$member$bestvalit)])
  exponential.values<-list('intercept'=exponential$member$bestmemit[which.min(exponential$member$bestvalit),1], 'base'= exponential$member$bestmemit[which.min(exponential$member$bestvalit),2],'rate'=exponential$member$bestmemit[which.min(exponential$member$bestvalit),3],'sigma'=constant$member$bestmemit[which.min(constant$member$bestvalit),4],'tau'=constant$member$bestmemit[which.min(constant$member$bestvalit),5], 'Likelihood'=exponential$member$bestvalit[which.min(exponential$member$bestvalit)])
  power.values<-list('intercept'=power$member$bestmemit[which.min(power$member$bestvalit),1],'base'=power$member$bestmemit[which.min(power$member$bestvalit),2],'rate'=power$member$bestmemit[which.min(power$member$bestvalit),3],'sigma'=constant$member$bestmemit[which.min(constant$member$bestvalit),4],'tau'=constant$member$bestmemit[which.min(constant$member$bestvalit),5], 'Likelihood'=power$member$bestvalit[which.min(power$member$bestvalit)])
  constant.piecewise.values<-list('intercept'=constant.piecewise$member$bestmemit[which.min(constant.piecewise$member$bestvalit),1],'intercept 2'=constant.piecewise$member$bestmemit[which.min(constant.piecewise$member$bestvalit),2],'split'=constant.piecewise$member$bestmemit[which.min(constant.piecewise$member$bestvalit),3],'sigma'=constant$member$bestmemit[which.min(constant$member$bestvalit),4],'tau'=constant$member$bestmemit[which.min(constant$member$bestvalit),5], 'Likelihood'=constant.piecewise$member$bestvalit[which.min(constant.piecewise$member$bestvalit)])
  linear.piecewise.values<-list('intercept'=linear.piecewise$member$bestmemit[which.min(linear.piecewise$member$bestvalit),1], 'slope'=linear.piecewise$member$bestmemit[which.min(linear.piecewise$member$bestvalit),2], 'split'= linear.piecewise$member$bestmemit[which.min(linear.piecewise$member$bestvalit),3],'intercept 2'=linear.piecewise$member$bestmemit[which.min(linear.piecewise$member$bestvalit),4], 'slope 2'=linear.piecewise$member$bestmemit[which.min(linear.piecewise$member$bestvalit),5],'sigma'=constant$member$bestmemit[which.min(constant$member$bestvalit),6],'tau'=constant$member$bestmemit[which.min(constant$member$bestvalit),7],  'Likelihood'=linear.piecewise$member$bestvalit[which.min(linear.piecewise$member$bestvalit)])
  exponential.piecewise.values<-list('intercept'=exponential.piecewise$member$bestmemit[which.min(exponential.piecewise$member$bestvalit),1],'base'=exponential.piecewise$member$bestmemit[which.min(exponential.piecewise$member$bestvalit),2], 'rate'=exponential.piecewise$member$bestmemit[which.min(exponential.piecewise$member$bestvalit),3],'split'=exponential.piecewise$member$bestmemit[which.min(exponential.piecewise$member$bestvalit),4],'intercept 2'= exponential.piecewise$member$bestmemit[which.min(exponential.piecewise$member$bestvalit),5], 'base 2'=exponential.piecewise$member$bestmemit[which.min(exponential.piecewise$member$bestvalit),6],'rate 2'=exponential.piecewise$member$bestmemit[which.min(exponential.piecewise$member$bestvalit),7], 'sigma'=constant$member$bestmemit[which.min(constant$member$bestvalit),8],'tau'=constant$member$bestmemit[which.min(constant$member$bestvalit),9],  'Likelihood'=exponential.piecewise$member$bestvalit[which.min(exponential.piecewise$member$bestvalit)])
  power.piecewise.values<-list('intercept'=power.piecewise$member$bestmemit[which.min(power.piecewise$member$bestvalit),1], 'base' = power.piecewise$member$bestmemit[which.min(power.piecewise$member$bestvalit),2], 'rate'=power.piecewise$member$bestmemit[which.min(power.piecewise$member$bestvalit),3],'split'=power.piecewise$member$bestmemit[which.min(power.piecewise$member$bestvalit),4],'intercept 2'= power.piecewise$member$bestmemit[which.min(power.piecewise$member$bestvalit),5], 'base 2'=power.piecewise$member$bestmemit[which.min(power.piecewise$member$bestvalit),6], 'rate 2'=power.piecewise$member$bestmemit[which.min(power.piecewise$member$bestvalit),7],'sigma'=constant$member$bestmemit[which.min(constant$member$bestvalit),8],'tau'=constant$member$bestmemit[which.min(constant$member$bestvalit),9],  'Likelihood'=power.piecewise$member$bestvalit[which.min(power.piecewise$member$bestvalit)])
  
  out<-list('constant'=constant.values,'linear'=linear.values,'exponential'=exponential.values,'power'=power.values,'constant piecewise'=constant.piecewise.values,"linear piecewise"=linear.piecewise.values,"power piecewise"=power.piecewise.values,"exponential piecewise"=exponential.piecewise.values)
  
  return(out)
}


subject.fit.MLE.exGuass(data.by.subject[[200]])
