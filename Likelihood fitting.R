
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
subject.fit.MLE<-function(subject.data){
  
  subject.data<-subject.data
  
  constant.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    subject.data$model.predict<-mapply(constant.model.predict,a)
    lik<-likelihood(subject.data$difference, subject.data$model.predict)
    return(-lik)
  }
  linear.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    subject.data$model.predict<-mapply(linear.model.predict,t,a,b)
    lik<-likelihood(subject.data$difference, subject.data$model.predict)
    return(-lik)
  }
  exponential.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    subject.data$model.predict<-mapply(exponential.model.predict,t,a,b,c)
    lik<-likelihood(subject.data$difference, subject.data$model.predict)
    return(-lik)
  }
  power.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    d<-params[4]
    subject.data$model.predict<-mapply(power.model.predict,t,a,b,c)
    lik<-likelihood(subject.data$difference, subject.data$model.predict)
    return(-lik)
  }
  constant.piecewise.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    subject.data$model.predict<-mapply(constant.piecewise.model.predict,t,a,b,c)
    lik<-likelihood(subject.data$difference, subject.data$model.predict)
    return(-lik)
  }
  linear.piecewise.fit<-function(params){
    t<-subject.data[3]
    a<-params[1]
    b<-params[2]
    c<-params[3]
    d<-params[4]
    e<-params[5]
    subject.data$model.predict<-mapply(linear.piecewise.model.predict,t,a,b,c,d,e)
    lik<-likelihood(subject.data$difference, subject.data$model.predict)
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
    subject.data$model.predict<-mapply(exponential.piecewise.model.predict,t,a,b,c,d,e,f,g)
    lik<-likelihood(subject.data$difference, subject.data$model.predict)
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
    h<-params[8]
    i<-params[9]
    subject.data$model.predict<-mapply(power.piecewise.model.predict,t,a,b,c,d,e,f,g)
    lik<-likelihood(subject.data$difference, subject.data$model.predict)
    return(-lik)
  }
  
  constant<-DEoptim(constant.fit, lower=c(-2001),upper=c(2001))
  linear<-DEoptim(linear.fit,lower=c(-2001, -27),upper=c(2001,27))
  exponential<-DEoptim(exponential.fit,lower=c(-2001,-5,-1),upper=c(2001,5,0))
  power<-DEoptim(power.fit,lower=c(-2001,-5,-1),upper=c(2001,0,0))
  constant.piecewise<-DEoptim(constant.piecewise.fit,lower=c(-2001,-2001,0),upper=c(2001,2001,72))
  linear.piecewise<-DEoptim(linear.piecewise.fit,lower = c(-2001,-27,0,-2001,-27),upper=c(2001,27,72,2001,27))
  exponential.piecewise<-DEoptim(exponential.piecewise.fit,lower = c(-2001,-5,-1,0,-2001,-5,-1),upper=c(2001,5,0,72,2001,5,0))
  power.piecewise<-DEoptim(power.piecewise.fit,lower = c(-2001,-5,-1,0,-2001,-5,-1),upper=c(2001,0,0,72,2001,0,0))
  
  constant.values<-list('intercept'=constant$optim$bestmem[1], 'Likelihood'=constant$optim$bestval)
  linear.values<-list ('intercept'=linear$optim$bestmem[1],'slope'=linear$optim$bestmem[2], 'Likelihood'=linear$optim$bestval)
  exponential.values<-list('intercept'=exponential$optim$bestmem[1], 'base'= exponential$optim$bestmem[2],'rate'=exponential$optim$bestmem[3], 'Likelihood'=exponential$optim$bestval)
  power.values<-list('intercept'=power$optim$bestmem[1],'base'=power$optim$bestmem[2],'rate'=power$optim$bestmem[3],'Likelihood'=power$optim$bestval)
  constant.piecewise.values<-list('intercept'=constant.piecewise$optim$bestmem[1],'intercept 2'=constant.piecewise$optim$bestmem[2],'split'=constant.piecewise$optim$bestmem[3], 'Likelihood'=constant.piecewise$optim$bestval)
  linear.piecewise.values<-list('intercept'=linear.piecewise$optim$bestmem[1], 'slope'=linear.piecewise$optim$bestmem[2], 'split'= linear.piecewise$optim$bestmem[3],'intercept 2'=linear.piecewise$optim$bestmem[4], 'slope 2'=linear.piecewise$optim$bestmem[5],  'Likelihood'=linear.piecewise$optim$bestval)
  exponential.piecewise.values<-list('intercept'=exponential.piecewise$optim$bestmem[1],'base'=exponential.piecewise$optim$bestmem[2], 'rate'=exponential.piecewise$optim$bestmem[3],'split'=exponential.piecewise$optim$bestmem[4],'intercept 2'= exponential.piecewise$optim$bestmem[5], 'base 2'=exponential.piecewise$optim$bestmem[6],'rate 2'=exponential.piecewise$optim$bestmem[7],   'Likelihood'=exponential.piecewise$optim$bestval)
  power.piecewise.values<-list('intercept'=power.piecewise$optim$bestmem[1], 'base' = power.piecewise$optim$bestmem[2], 'rate'=power.piecewise$optim$bestmem[3],'split'=power.piecewise$optim$bestmem[4],'intercept 2'= power.piecewise$optim$bestmem[5], 'base 2'=power.piecewise$optim$bestmem[6], 'rate 2'=power.piecewise$optim$bestmem[7],'Likelihood'=power.piecewise$optim$bestval)
  
  out<-list('constant'=constant.values,'linear'=linear.values,'exponential'=exponential.values,'power'=power.values,'constant piecewise'=constant.piecewise.values,"linear pieceiwise"=linear.piecewise.values,"power piecewise"=power.piecewise.values,"exponential piecewise"=exponential.piecewise.values)
  
  return(out)
}


subject.fit.MLE(data.by.subject[[200]])

