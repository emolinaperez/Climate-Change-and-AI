#This script uses different methods to fit a classification tree to our data set
 dir.data<-"C:\\Users\\Usuario\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\41. Climate Change and AI\\Data\\Model\\"
 data<-read.csv(paste0(dir.data,"ModelData.csv"))

#let's look at the data first,
  summary(data)

#remove columns with only NA
 bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
 bad.vars<-names(bad.vars[bad.vars>0.03])
 bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#let's select a response of interest
#EN.ATM.CO2E.PC=CO2 emissions (metric tons per capita)
  ids<-c("iso_code3","country")
  response<-"EN.ATM.CO2E.PC"
  predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))

# Since we are analyzing this as a classification problem
#let's look at how this response is distributed
  summary(data[,response])

# now let's partition the response of interest into two groups
#focus on the high emissions per capita nations
 threshold<-as.numeric(quantile(data[,response],0.70,na.rm=TRUE))
 data$response.binary<-as.factor(ifelse(data[,response]>threshold,"High","Low"))

# remove NA values in the response
 data<-subset(data,is.na(response.binary)==FALSE)

#now look at how many high emission cases we have
 summary(data$response.binary)

#Now before we start modeling, we subset the data to be used in the model to only complete cases
 data.model<-data[,c("response.binary",predictors)]
 data.model<-data.model[complete.cases(data.model),]
 summary(data.model)
 dim(data.model)

# now we have a clean data set

#define the model we want to estimate
 model<-as.formula(paste0("response.binary","~",paste(predictors,collapse="+")))
 model


#========================
# Approach 1: Classification tree
#========================
#install.packages("tree")
 set.seed (55555)
 train<-sample (1: nrow(data.model ), 100)
 data.model.test<-data.model[-train ,]
 response.binary.test<-data.model$response.binary[-train ]
 tree.model <- tree(model,data.model ,subset =train )
 tree.model.pred<-predict(tree.model ,data.model.test ,type ="class")
 table(tree.model.pred ,response.binary.test)

# So this tree has an error rate of (5+4)/80

#We can use tree prunning to get a better classification tree
 set.seed (55555)
 cv.data.model<-cv.tree(tree.model ,FUN=prune.misclass )
 cv.data.model

# dev corresponds to the cross-validation error rate in this instance, which is then the best tree?

#we can use prune.misclass() to obtain the best tree
  prune.tree.model <- prune.misclass (tree.model ,best =2)
  plot(prune.tree.model )
  text(prune.tree.model ,pretty =0)

#how good is the prune tree ?
  tree.pred<-predict (prune.tree.model , data.model.test ,type ="class")
  table(tree.pred ,response.binary.test)

# So this tree has an error rate of (6+3)/80


#========================
# Approach 2: Regression tree
#========================

#single best
 library (MASS)
 set.seed (55555)
 train <- sample (1: nrow(data.model), 100)
 Rtree.data.model <-tree(model,data.model ,subset =train)
 summary (Rtree.data.model)
 plot(Rtree.data.model  )
 text(Rtree.data.model  ,pretty =0)

#prune tree
 cv.Rtree.data.model <- cv.tree(Rtree.data.model )
 cv.Rtree.data.model

#which is the best tree?

 prune.Rtree.data.model <- prune.misclass(Rtree.data.model  ,best =2)
 plot(prune.Rtree.data.model )
 text(prune.Rtree.data.model ,pretty =0)

 #========================
 # Approach 3: Random Forest
 #========================
#install.packages("randomForest")
 library(randomForest)
 set.seed (55555)
 rf.data.model <- randomForest(model,
                              data=data.model ,
                              subset =train ,
                              mtry=round(length(predictors)^0.5),
                              importance =TRUE
                              )
 rf.data.model

#base on many different trees, which are the most important drivers
 importance (rf.data.model)
 varImpPlot (rf.data.model )
