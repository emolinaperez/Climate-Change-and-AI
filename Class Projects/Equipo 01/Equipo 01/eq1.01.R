getwd()

dir.data<-"/Users/ferdinandg.m./Documents/GitHub/Climate-Change-and-AI/Data/Model/"
data<-read.csv(paste0(dir.data,"ModelData.csv"))

summary(data)
#paises con info
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
bad.vars<-names(bad.vars[bad.vars>0.50])

