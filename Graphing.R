install.packages('ggplot2')
library('ggplot2')
library('jsonlite')
data.fits<-read_json("fitdata.json")
data.fits<-fromJSON(final.data)

data.frame(data.fits$linear)


RMSE.fits<-array(dim=c(258,8))
best.fit<-array(dim=c(258))


for(i in 1:258){
  RMSE.fits[i,]<-c(data.all.fits[[i]]$constant$RMSE,
                   data.all.fits[[i]]$linear$RMSE,
                   data.all.fits[[i]]$exponential$RMSE,
                   data.all.fits[[i]]$power$RMSE,
                   data.all.fits[[i]]$`constant piecewise`$RMSE,
                   data.all.fits[[i]]$`linear pieceiwhse`$RMSE,
                   data.all.fits[[i]]$`exponential piecewise`$RMSE,
                   data.all.fits[[i]]$`power piecewise`$RMSE)
  best.fit[i]<-which.min(RMSE.fits[i,])
}

hist(best.fit)

##We need to make the values numeric for the subject

make.numeric<-function(data){sapply(data,function(x){
  y=as.numeric(x)
  return(y)
})}




visualize.fits<- function(data,fits){
  RT.diff<-as.vector(data$difference)
  t<-as.vector(data$t)
  fits<-make.numeric(fits)
  subject.data<-data.frame('x'=t, 'y'=RT.diff,'type'=rep(0,length(t)))
  plotting.vals<-seq(from=0.1,to=max(t),by=.1)
  ?mapply



  constant<-mapply(constant.model.predict,plotting.vals,MoreArgs= list(s = fits$constant[1]))
  
  linear<-mapply(linear.model.predict,plotting.vals,MoreArgs= list(a=fits$linear[1],b=fits$linear[2]))

  exponential<-mapply(exponential.model.predict,plotting.vals,MoreArgs= list(a=fits$exponential[1],b=fits$exponential[2],c=fits$exponential[3]))

  power<-mapply(power.model.predict,plotting.vals,MoreArgs= list(a=fits$power[1],b=fits$power[2],c=fits$power[3]))

  constant.piecewise<-mapply(constant.piecewise.model.predict, plotting.vals,MoreArgs= list(a=fits$'constant piecewise'[1], b = fits$'constant piecewise'[2], c = fits$'constant piecewise'[3]))
  
  linear.piecewise<-mapply(linear.piecewise.model.predict,plotting.vals,MoreArgs= list(a=fits$'linear pieceiwhse'[1],b=fits$'linear pieceiwhse'[2],c=fits$'linear pieceiwhse'[3],d=fits$'linear pieceiwhse'[4],e=fits$'linear pieceiwhse'[5]))
#### the name for this is messed up. Fixed in fitting program. Run again and change here###
  exponential.piecewise<-mapply(exponential.piecewise.model.predict,plotting.vals,MoreArgs= list(a=fits$'exponential piecewise'[1], b=fits$'exponential piecewise'[2],c=fits$'exponential piecewise'[3],d=fits$'exponential piecewise'[4],e=fits$'exponential piecewise'[5],f=fits$'exponential piecewise'[6], g = fits$'exponential piecewise'[7]))

  power.piecewise<-mapply(power.piecewise.model.predict,plotting.vals,MoreArgs= list(a=fits$'power piecewise'[1],b=fits$'power piecewise'[2],c=fits$'power piecewise'[3],d=fits$'power piecewise'[4],e=fits$'power piecewise'[5],f=fits$'power piecewise'[6],g= fits$'power piecewise'[7]))
print(fits)
  
  
  constant.plot<-data.frame('x'=plotting.vals,'y'=constant,'type'=rep('constant',length(plotting.vals)))
  linear.plot<-data.frame('x'=plotting.vals,'y'=linear,'type'=rep('linear',length(plotting.vals)))
  exponential.plot<-data.frame('x'=plotting.vals,'y'=exponential,'type'=rep('exponential',length(plotting.vals)))
  power.plot<-data.frame('x'=plotting.vals,'y'=power,'type'=rep('power',length(plotting.vals)))
  constant.piecewise.plot<-data.frame('x'=plotting.vals,'y'=constant.piecewise,'type'=rep('constant piecewise',length(plotting.vals)))
  linear.piecewise.plot<-data.frame('x'=plotting.vals,'y'=linear.piecewise,'type'=rep('linear piecewise',length(plotting.vals)))
  exponential.piecewise.plot<-data.frame('x'=plotting.vals,'y'=exponential.piecewise,'type'=rep('exponential piecewise',length(plotting.vals)))
  power.piecewise.plot<-data.frame('x'=plotting.vals,'y'=power.piecewise,'type'=rep('power piecewise',length(plotting.vals)))
  
  
 
  
plot.data<-rbind(subject.data,constant.plot,linear.plot,exponential.plot,power.plot,constant.piecewise.plot,linear.piecewise.plot,exponential.piecewise.plot,power.piecewise.plot)

?rbind
rbind(c(1,2,3),c(4,5,6))

ggplot(data=plot.data,aes(ymin=-2000,ymax=2000))+
  geom_point(data=subset(plot.data,type==0),mapping =  aes(x=x,y=y))+
  geom_line(data=subset(plot.data,type!=0),mapping=aes(x=x,y=y,col=type))
}


?geom_line
?ggplot

visualize.fits(data.by.subject[[1]],data.fits.MLE.sigma[[1]])
  
do.call(linear.model.predict,list(plotting.vals,1,2))




##create data frame with all values and the predicted values with collumn 'type' to indicate what kind of plot.



linear.model.predict<-function(t,a,b){
  rt<-a+b*t
  return(rt)
}

constant.model.predict<-function(t,s){
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

    if(t < c){
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
