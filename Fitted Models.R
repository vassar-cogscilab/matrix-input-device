#Fitting all subjects
install.packages("DEoptim")
install.packages('jsonlite')
library('jsonlite')
library('DEoptim')

data.all.fits.MLE.sigma<-list()
data.all.fits.MLE<-list()
data.all.fits.RMSE<-list()

fit.all.data<-function(data.by.subject){

for(i in 1:258){
  
 data.all.fits.MLE.sigma[[i]]<<-subject.fit.MLE.sigma(data.by.subject[[i]])
 data.all.fits.RMSE[[i]]<<-subject.fit.RMSE(data.by.subject[[i]])
 data.all.fits.MLE[[i]]<<-subject.fit.MLE(data.by.subject[[i]])
}


final.data.MLE.sigma<-toJSON(data.all.fits.MLE.sigma)
final.data.MLE<-toJSON(data.all.fits.MLE)
final.data.RMSE<-toJSON(data.all.fits.RMSE)

write_json(final.data.MLE.sigma,"fit.data.MLE.sigma.json")
write_json(final.data.MLE,"fit.data.MLE.json")
write_json(final.data.RMSE,"fit.data.RMSE.json")

}


fit.all.data(data.by.subject)
