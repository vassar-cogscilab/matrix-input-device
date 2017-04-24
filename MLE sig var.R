
install.packages('DEoptim')

library('DEoptim')



?DEoptim


##likelihood.sigma Normal variable sd##

log.likelihood.sigma<-function(x,mu,sig){
  sigma<-rep(as.numeric(sig), length(x))
  return(dnorm(x,mu,sigma,log=T))
}

likelihood.sigma<-function(data, mu,sigma){
  log.lik<-sum(log.likelihood.sigma(x=data,mu=mu,sig=sigma))
  return(log.lik)
}
?dnorm
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
subject.fit.MLE.sigma<-function(subject.data){
  
  subject.data<-subject.data
  
  constant.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    sigma<-params[2]
    subject.data$model.predict<-mapply(constant.model.predict,a)
    lik<-likelihood.sigma(data=subject.data$difference, mu=subject.data$model.predict,sigma=sigma)
    return(-lik)
  }
  linear.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    sigma<-params[3]
    subject.data$model.predict<-mapply(linear.model.predict,t,a,b)
    lik<-likelihood.sigma(data=subject.data$difference, mu=subject.data$model.predict,sigma=sigma)
    return(-lik)
  }
  exponential.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    sigma<-params[4]
    subject.data$model.predict<-mapply(exponential.model.predict,t,a,b,c)
    lik<-likelihood.sigma(data=subject.data$difference, mu=subject.data$model.predict,sigma=sigma)
    return(-lik)
  }
  power.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    sigma<-params[4]
    subject.data$model.predict<-mapply(power.model.predict,t,a,b,c)
    lik<-likelihood.sigma(data=subject.data$difference, mu=subject.data$model.predict,sigma=sigma)
    return(-lik)
  }
  constant.piecewise.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    sigma<-params[4]
    subject.data$model.predict<-mapply(constant.piecewise.model.predict,t,a,b,c)
    lik<-likelihood.sigma(data=subject.data$difference, mu=subject.data$model.predict,sigma=sigma)
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
    subject.data$model.predict<-mapply(linear.piecewise.model.predict,t,a,b,c,d,e)
    lik<-likelihood.sigma(data=subject.data$difference, mu=subject.data$model.predict,sigma=sigma)
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
    subject.data$model.predict<-mapply(exponential.piecewise.model.predict,t,a,b,c,d,e,f,g)
    lik<-likelihood.sigma(data=subject.data$difference, mu=subject.data$model.predict,sigma=sigma)
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
    subject.data$model.predict<-mapply(power.piecewise.model.predict,t,a,b,c,d,e,f,g)
    lik<-likelihood.sigma(data=subject.data$difference, mu=subject.data$model.predict,sigma=sigma)
    return(-lik)
  }
  
  
  constant<-DEoptim(constant.fit, lower=c(-2001,0.1),upper=c(2001,1000))
  linear<-DEoptim(linear.fit,lower=c(-2001, -27,0.1),upper=c(2001,27,1000))
  exponential<-DEoptim(exponential.fit,lower=c(-2001,-5,-1,0.1),upper=c(2001,5,0,1000))
  power<-DEoptim(power.fit,lower=c(-2001,-5,-1,0.01),upper=c(2001,0,0,1000))
  constant.piecewise<-DEoptim(constant.piecewise.fit,lower=c(-2001,-2001,0,0.1),upper=c(2001,2001,72,1000))
  linear.piecewise<-DEoptim(linear.piecewise.fit,lower = c(-2001,-27,0,-2001,-27,0.1),upper=c(2001,27,72,2001,27,1000))
  exponential.piecewise<-DEoptim(exponential.piecewise.fit,lower = c(-2001,-5,-1,0,-2001,-1,-.2,0.1),upper=c(2001,5,0,72,2001,5,0,1000))
  power.piecewise<-DEoptim(power.piecewise.fit,lower = c(-2001,-5,-1,0,-2001,-5,-1,0.1),upper=c(2001,0,0,72,2001,0,0,1000))

  
  constant.values<-list('intercept'=constant$optim$bestmem[1], 'sigma'=constant$optim$bestmem[2], 'likelihood'=constant$optim$bestval)
  linear.values<-list ('intercept'=linear$optim$bestmem[1],'slope'=linear$optim$bestmem[2],'sigma'=linear$optim$bestmem[3], 'likelihood'=linear$optim$bestval)
  exponential.values<-list('intercept'=exponential$optim$bestmem[1], 'base'= exponential$optim$bestmem[2],'rate'=exponential$optim$bestmem[3],'sigma'=exponential$optim$bestmem[4], 'likelihood'=exponential$optim$bestval)
  power.values<-list('intercept'=power$optim$bestmem[1],'base'=power$optim$bestmem[2],'rate'=power$optim$bestmem[3],'sigma'=power$optim$bestmem[4], 'likelihood'=power$optim$bestval)
  constant.piecewise.values<-list('intercept'=constant.piecewise$optim$bestmem[1],'intercept 2'=constant.piecewise$optim$bestmem[2],'split'=constant.piecewise$optim$bestmem[3],'sigma'=constant.piecewise$optim$bestmem[4], 'likelihood'=constant.piecewise$optim$bestval)
  linear.piecewise.values<-list('intercept'=linear.piecewise$optim$bestmem[1], 'slope'=linear.piecewise$optim$bestmem[2], 'split'= linear.piecewise$optim$bestmem[3],'intercept 2'=linear.piecewise$optim$bestmem[4], 'slope 2'=linear.piecewise$optim$bestmem[5],'sigma'=linear.piecewise$optim$bestmem[6],  'likelihood'=linear.piecewise$optim$bestval)
  exponential.piecewise.values<-list('intercept'=exponential.piecewise$optim$bestmem[1],'base'=exponential.piecewise$optim$bestmem[2], 'rate'=exponential.piecewise$optim$bestmem[3],'split'=exponential.piecewise$optim$bestmem[4],'intercept 2'= exponential.piecewise$optim$bestmem[5], 'base 2'=exponential.piecewise$optim$bestmem[6],'rate 2'=exponential.piecewise$optim$bestmem[7], 'sigma'=exponential.piecewise$optim$bestmem[8],  'likelihood'=exponential.piecewise$optim$bestval)
  power.piecewise.values<-list('intercept'=power.piecewise$optim$bestmem[1], 'base' = power.piecewise$optim$bestmem[2], 'rate'=power.piecewise$optim$bestmem[3],'split'=power.piecewise$optim$bestmem[4],'intercept 2'= power.piecewise$optim$bestmem[5], 'base 2'=power.piecewise$optim$bestmem[6], 'rate 2'=power.piecewise$optim$bestmem[7],'sigma'=power.piecewise$optim$bestmem[8],'likelihood'=power.piecewise$optim$bestval)
  out<-list('constant'=constant.values,'linear'=linear.values,'exponential'=exponential.values,'power'=power.values,'constant piecewise'=constant.piecewise.values,"linear pieceiwhse"=linear.piecewise.values,"power piecewise"=power.piecewise.values,"exponential piecewise"=exponential.piecewise.values)
  
  return(out)
}

?dexgauss
subject.fit.MLE.sigma(data.by.subject[[200]])

