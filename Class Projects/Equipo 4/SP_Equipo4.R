#Migración
#This script uses different methods to fit a classification tree to our data set
 dir.data<-"/Users/marianamonroy/Desktop/Git Hub/Climate-Change-and-AI/Data/Model/"
 data<-read.csv(paste0(dir.data,"ModelData.csv"))

#let's look at the data first,
  summary(data)

#remove columns with only NA
 bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
 bad.vars<-names(bad.vars[bad.vars>0.03])
 bad.vars<-c(bad.vars,"SE.ENR.PRSC.FM.ZS","SE.PRM.CMPT.ZS","SE.SEC.ENRR","SH.DYN.AIDS.ZS","TT.PRI.MRCH.XD.WD","TX.VAL.TECH.MF.ZS")

#response of interest
#migration_and_displacement in countries for climate change
  ids<-c("iso_code3","country")
  response<-"migration_and_displacement"
  predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))
#Comentes:
#0  to 4 interest response
#predictors 
  
# Since we are analyzing this as a classification problem
#let's look at how this response is distributed
  summary(data[,response])
  
  
#Based on the mean
#interest response 
 #English
 # the first quartile is below 2, the second 50% quantile is below 2.046, the third quantile is above 2.00
  #Spanish
# el primer cuantil se encuentra por debajo del 2 , el segundo cuantil 50% se encuentra por debajo de 2.046,
  #el tercer cuantil se encuentra por arriba del 2.00 
  
  
 #English:
 #Groups with high quantitative variable and low migration and displacement, which predictors are the
 #best predict?
  #Spanish:
#Grupos con variable cuantitativo alto y bajo migración y desplazamiento, que predictores predice mejor?
  
# now let's partition the response of interest into two groups
#focus on the migration and displacement 
 #tresshold based on .10 of the observations that are below that limit
 threshold<-as.numeric(quantile(data[,response],0.10,na.rm=TRUE))
 data$response.binary<-as.factor(ifelse(data[,response]>1.5,"High","Low"))

 #English
 #comments
 #treshold is equivalent to 1.5, we are going to generate a binary response, saving it as a factor
 # based on the following function, if the answer of interest is above
 # of this function has high migration.
 

 #Spanish
 #Comentarios
 #treshold es equivalente a 1.5, sgeneraremos una respuesta binaria, guardandola como un factor
 #en función de la siguiente función, si la respuesta deeinteres esta por arriba
 #de esta función tiene alta migración.
 
 
# remove NA values in the response
 data<-subset(data,is.na(response.binary)==FALSE)

#now look at how many high emission cases we have
 summary(data$response.binary)
 
 #English:
 #Binary interest response as a function of a threshold (1.5)
 #of my sample: 183 countries are above and 11 below.
 #Spanish:
#Respuesta de interes binaria en función de un treshold (1.5)
#de mi muestra: 183 países se encuentran por arriba y 11 por debajo.
 
 
#Now before we start modeling, we subset the data to be used in the model to only complete cases
 data.model<-data[,c("response.binary",predictors)]
 data.model<-data.model[complete.cases(data.model),]
 summary(data.model)
 dim(data.model)

# now we have a clean data set
 # 173 high, 7 low registers countries, I lost countries but I keep 180
 #numeric variables.
 
 
#define the model we want to estimate
 model<-as.formula(paste0("response.binary","~",paste(predictors,collapse="+")))
 model

 #English:
 #Policy what do we want to know?
 # What makes a country have migration as a function of all observables as a function of ndc
 # and socioeconomic characteristics (word bank).
 
 #It was chosen to analyze the classification model, since we will discriminate which ones affect migration and which ones do not,
 # the validation of this model was done through Random Forest and with linear regression comparison.
 
 # In addition, two linear classification approaches were carried out by comparing significant variables
 #and possible relationship between them merely to delve further into the information and further understand the behavior
 # of variables. # numeric variables.
 
 #Español:
 #Policy ¿que queremos saber?
 #Qué hace que un pais tenga migracion en funcion de todos los observables en funcion de ndc 
 #y caracteristicas socioeconomicas world bank.
 
 #Se escogió para analizar el modelo clasificacion, pues discriminaremos cuales inciden en migracion y cuales no,
 #la validación de este modelo se hizo a traves de Random Forest y con comparación de regresión lineal 
 
 #Además, se realizaron dos aproacches de clasificación lineal por comparación de variables significativas
 #y posible relación entre ellas meramente por aondar más en la información y comprender aún más el comportamiento
 #de las variables.
 
 #========================
 # Approach 1: Classification tree
 #========================
 #install.packages("tree")
 library(tree)
 set.seed (55555)
 train<-sample (1: nrow(data.model )/2)
 data.model.test<-data.model[-train ,]
 response.binary.test<-data.model$response.binary[-train ]
 tree.model <- tree(model,data.model ,subset =train )
 tree.model.pred<-predict(tree.model ,data.model.test ,type ="class")
 table(tree.model.pred ,response.binary.test)
 
 #English:
 
 #First arbor the model was trained with half the data
 #train
 #The optimal size of the trees is 3 nodes with a deviation of 2
 
 #The significant variable is EN-ATM-CO2E.PC (Carbon emissions)
 # are below the 8-09133 level, migration and displacement is at a high level,
 # if not, the variable has a low level of migration ...
 
 # c1:
 #The significant variable is EN-ATM-CO2E.PC (Carbon emissions)
 # if it is below the 8.09133 level, migration and displacement is at a high level,
 # if not, the variable is low level ... which leads to c2.
 # c2
 # The second significant variable that is triggered by being EN-ATM-CO2E.PC (Carbon emissions) greater than 8.09133, the countries of
 # low migration, they can migrate by SH.DYN.MORT (Mortality rate, under-5 (per 1,000 live births)),
 #if it is above 8.09133 and below 9.2.
 
 
 #Confusion matrix
 
 #Compute confusion matrix extract response.binary column from test and put it in response.binary.test
 #what is the real value of the binary variable in the test set?
 #decision tree with tree function.

 
 #Español:
 #Primer arbor se entrenó el modelo con la mitad de los datos 
 #train 
 #El tamaño óptimo del arbos es de 3 nodos con desviación de 2
 
 #La variable significativa es EN-ATM-CO2E.PC (Emisiones de carbono)
 #están por debajo del nivel 8-09133, la migración y el desplazameniento es de alto nivel,
 # si no, la variable incide bajo nivel de migración...
 
 #c1: 
 #La variable significativa es EN-ATM-CO2E.PC (Emisiones de carbono)
 # si está está por debajo del nivel 8.09133, la migración y el desplazameniento es de alto nivel,
 # si no, la variable es de bajo nivel... lo cual conduce a c2.
 #c2
 #La segunda variable significativa que se desencadena al ser EN-ATM-CO2E.PC (Emisiones de carbono) mayor de 8.09133, los países de
 #migración baja, podrán migrar por por SH.DYN.MORT (Mortality rate, under-5 (per 1,000 live births)), 
 #si esta está por arriba de 8.09133 y por  debajo de 9.2.
 
 
 #Matriz confusión 
 
 #computar matriz de confusión extraer columna response.bianry de test y ponerlo en response.bianry.test
 #cual es el valor real de la variable binaria en el conjunto de prueba? 
 #arbol de decision con función tree.

 
 
 #We can use tree prunning to get a better classification tree
 set.seed (55555)
 cv.data.model<-cv.tree(tree.model ,FUN=prune.misclass )
 cv.data.model
 
 #English:
 #high is wrong 7 times does not predict migration well
 # how many times is the low category wrong? 5 times
 # So this tree has an error rate of (5 + 5) / 100
 # + -rate error: 0.1
 
 #Español:
 #high se equivoca 7 veces no predice bien migracion
 #cuantas veces se equivoca la categoria bajo? 5 veces
 # So this tree has an error rate of (5+5)/100
 # +-tasa error: 0.1
 
 #English: 
 # Before and after pruning the tree we have the same error rate, 80 times the model
 # I correctly classify high and 0 times the model correctly classify low
 # 10 times the model was wrong to classify
 #Remember the test database has half the data: 90 out of 180.
 
 #Español: 
 #Antes y despues de podar el arbol tenemos la misma tasa de error, 80 veces el modelo
 #clasifico correctamente a high y 0 veces el modelo clasifico correctamente en low
 #10 veces el modelo se equivocó en clasificar
 #se recuerda la base de datos de prueba tiene la mitad de los datos: 90 de 180.
 
 
 # dev corresponds to the cross-validation error rate (in our case 2) in this instance, which is then the best tree?
 
 #we can use prune.misclass() to obtain the best tree
 prune.tree.model <- prune.misclass (tree.model ,best = 2)
 plot(prune.tree.model)
 text(prune.tree.model ,pretty =0)
 
 #how good is the prune tree ?
 tree.pred<-predict (prune.tree.model , data.model.test ,type ="class")
 table(tree.pred ,response.binary.test)
 
 # So this tree has an error rate of (8+8)/90
 #0.15 error. It predicts well HIGH.
 
 #========================
 # Approach 2: Regression tree
 #========================
 
 #single best
 library (MASS)
 set.seed (55555)
 train <- sample (1: nrow(data.model), 100)
 Rtree.data.model <-tree(model,data.model ,subset =train)
 summary (Rtree.data.model)
 #ndc y world bank no dice mucho, 
 #treshold por debajo es... de alta 
 #desviacion del arbol: 0.1235
 #Misclassification error rate: 0.03
 plot(Rtree.data.model,col="rosybrown4")
 text(Rtree.data.model  ,pretty =0)
 
 #prune tree
 cv.Rtree.data.model <- cv.tree(Rtree.data.model )
 cv.Rtree.data.model
 
 #which is the best tree?
 # 5 nodes
 # minor error a node, 59.86056
 
 prune.Rtree.data.model <- prune.misclass(Rtree.data.model  ,best =5)
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
 varImpPlot (rf.data.model, col="rosybrown4")


 #Type of random forest: classification
 #Number of trees: 500
 #No. of variables tried at each split: 6
 #OOB estimate of  error rate: 7%
 
 #========================
 # Lineal regression
 #========================
 
 #migration_and_displacement
 ids<-c("iso_code3","country")
 response<-"migration_and_displacement"
 predictors<-subset(colnames(data),!(colnames(data)%in%c(ids,response,bad.vars)))
 
 #estimate full model for variables second aprroach
 data.model<-data[,c(response,predictors)]
 model<-as.formula(paste0(response,"~",paste(predictors,collapse="+")))
 full.model <- lm(migration_and_displacement~SP.POP.TOTL+M_PL7+a_water+a_urban, data = data.model)
 summary(full.model)
 #estimate full model with variables of Random forest Gini NDC
 ndc.randomf <- lm(migration_and_displacement~m_lulucf + a_water + 
                      m_transport + a_environment + a_health + m_waste + a_agriculture + 
                      m_industries + a_coastal_zone + m_agriculture + a_lulucf + 
                      m_energy + a_cross_cutting_area + m_buildings + a_education + 
                      m_economy.wide + a_drm + a_social_development + a_urban + 
                      a_transport, data = data.model)
 summary(ndc.randomf)
 
 
 #load library
 library(leaps)
 
 #first let's divide the sample into a test and a a train set
 set.seed (55555)
 train <- sample (c(TRUE ,FALSE), nrow(data.model),rep=TRUE)
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
 
 

