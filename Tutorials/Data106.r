#This script estimates a linear model using the model data table
 dir.data<-"C:\\Users\\Usuario\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\41. Climate Change and AI\\Data\\Model\\"
 data<-read.csv(paste0(dir.data,"ModelData.csv"))

##let's look at the data first,
  summary(data)

#remove columns with only NA
 bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
 bad.vars<-names(bad.vars[bad.vars>0.50])
 bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#let's select a response of interest

#EN.ATM.CO2E.PC=CO2 emissions (metric tons per capita)
  ids<-c("iso_code3","country")
  response<-"EN.ATM.CO2E.PC"
  predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))

#estimate full model
 data.model<-data[,c(response,predictors)]
 model<-as.formula(paste0(response,"~",paste(predictors,collapse="+")))
 full.model <- lm(model, data = data.model)
 summary(full.model)

#now let's select various smaller models
 #install.packages("leaps")

#load library
 library(leaps)


#first let's divide the sample into a test and a a train set
  set.seed (55555)
  train <- sample (c(TRUE ,FALSE), nrow(data.model ),rep=TRUE)
  test  <- (!train )

#define maximum length of the model
 max.vars<-8

#let's do full search
 regfit.best <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T) #you can choose how large you want the search to be

#let's do forward stepwise selection
 regfit.fwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "forward") #you can choose how large you want the search to be

#let's do backard stepwise selection
 regfit.bwd <- regsubsets (model, data.model[train,], nvmax =max.vars,really.big = T, method = "backward") #you can choose how large you want the search to be


#explore different models
 msize<-8
  coef(regfit.best ,msize)
  coef(regfit.fwd , msize)
  coef(regfit.bwd , msize)


#now how do we select which model is best

predict.regsubsets <-function (object, model ,newdata ,id ){
  #object<-regfit.best
  #newdata<-data.model[test ,]
  #id<-1
  form<-model
  options(na.action='na.pass')
  mat<-model.matrix (form,newdata )
  coefi<-coef(object ,id=id)
  xvars<-names (coefi )
  pred<-mat[,xvars ]%*% coefi
  val.errors<- mean((newdata[,response]-pred)^2,na.rm=TRUE)
  val.errors
  }

#now estimate test error for the different versions of the models
#best subset
 cv.best<-data.frame(subset.type="best",
                     nvars=1,
                     test.mse=predict.regsubsets(regfit.best,model,data.model[test ,],1))

 for(i in 2:max.vars)
 {
   pivot<-data.frame(subset.type="best",
                      nvars=i,
                      test.mse=predict.regsubsets(regfit.best,model,data.model[test ,],i))
  cv.best<-rbind(cv.best,pivot)

 }

#best model
 subset(cv.best,test.mse==min(test.mse))
#actual model
 coef(regfit.best ,subset(cv.best,test.mse==min(test.mse))$nvars)

#forward method
cv.fwd<-data.frame(subset.type="fwd",
                    nvars=1,
                    test.mse=predict.regsubsets(regfit.fwd,model,data.model[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="fwd",
                     nvars=i,
                     test.mse=predict.regsubsets(regfit.fwd,model,data.model[test ,],i))
 cv.fwd<-rbind(cv.fwd,pivot)

}

#best model
subset(cv.fwd,test.mse==min(test.mse))
#actual model
coef(regfit.fwd ,subset(cv.fwd,test.mse==min(test.mse))$nvars)

#
#backward method
cv.bwd<-data.frame(subset.type="bwd",
                    nvars=1,
                    test.mse=predict.regsubsets(regfit.bwd,model,data.model[test ,],1))

for(i in 2:max.vars)
{
  pivot<-data.frame(subset.type="bwd",
                     nvars=i,
                     test.mse=predict.regsubsets(regfit.bwd,model,data.model[test ,],i))
 cv.bwd<-rbind(cv.bwd,pivot)

}

#best model
subset(cv.bwd,test.mse==min(test.mse))
#actual model
coef(regfit.bwd ,subset(cv.bwd,test.mse==min(test.mse))$nvars)

#
