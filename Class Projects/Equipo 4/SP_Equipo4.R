#Migración y deesplazamiento
#Situación problema
#Equipo 4
#Mariana y Paola

#Librarys
library(randomForest)
library(tree)
library (MASS)

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
 #el primer cuantil se encuentra por debajo del 2 , el segundo cuantil 50% se encuentra por debajo de 2.046,
 #el tercer cuantil se encuentra por arriba del 2.00 
  
  
 #English:
 #Groups with high quantitative variable and low, action for prevent migration and displacement, which predictors are the
 #best predict?
 #Spanish:
 #Grupos con variable cuantitativo alto y bajo, acciones para prevenir migración y desplazamiento, que predictores predice mejor?
  
 # now let's partition the response of interest into two groups
 #focus on the migration and displacement 
 #tresshold based on .10 of the observations that are below that limit
 threshold<-as.numeric(quantile(data[,response],0.10,na.rm=TRUE))
 data$response.binary<-as.factor(ifelse(data[,response]>1.5,"High","Low"))

 #English
 #treshold is equivalent to 1.5, we are going to generate a binary response, saving it as a factor
 # based on the following function, if the answer of interest is above
 # of this function has high migration.
 

 #Español
 #treshold es equivalente a 1.5, generaremos una respuesta binaria, guardandola como un factor
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
 #What variables make a country take action in the area of migration and displacement caused by climate change?
 
 # It was chosen to analyze the classification model, since we will discriminate which country variables make
 #to take more action on migration and climate displacement,
 # the validation of this model was done through Random Forest and a linear classification tree,
 # In addition, two linear regression models were created with the response variable "migration and displacement"
 #regreesion which was done with the significant variables thrown by the tree and
 #the second model the avriables used returned on migration and displacement were only
 #the ndc variables of the random forest. The above as we seek as an answer which variables
 #they make countries take action, we do not look for sociodemographic ones. Merely to delve further into the information and further understand the behavior
 # of variables.
 
 #Español:
 #Policy ¿que queremos saber?
 #What variables make a country take action in the area of migration and displacement caused by climate change?
 
 #Se escogió para analizar el modelo clasificacion, pues discriminaremos cuales variables de países hacen
 #que tomen más acciones con respecto a la migración y el desplazamiento climatico,
 #la validación de este modelo se hizo a traves de Random Forest y un arbol de clasificación lineal,
 # además se crearon dos modelos de regresión lineal con variable de respuesta "migración y desplazamiento"
 #regreesion la cual se hizo con las variables significativas arrojadas por el arbol y
 #el segundo modelo las avriables utilizadas regresadas sobre migración y desplazamiento fueron solamente
 #las variablees ndc del random forest. Lo anterior pues buscamos como respuesta que variables
 #hacen que los paises tomen acción, no buscamos las sociodemograficas. Meramente por aondar más en la información y comprender aún más el comportamiento
 #de las variables.
 

 #========================
 # Approach 1: Regression tree
 #========================
 
 #single best
 set.seed (55555)
 train <- sample (1: nrow(data.model), 100)
 Rtree.data.model <-tree(model,data.model ,subset =train)
 summary (Rtree.data.model)

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
 plot1 <- plot(prune.Rtree.data.model )
 text(prune.Rtree.data.model ,pretty =0)

 #English:

 
 
 #Español:
 #Primer arbor se entrenó el modelo con la mitad de los datos 
 #train 
 #El tamaño óptimo del arbos es de 5 nodos con desviación de 59.86056
 
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

 

 #========================
 # Approach 2: Random Forest
 #========================
 
#install.packages("randomForest")
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
 plot2 <- varImpPlot (rf.data.model, col="rosybrown4")


 #Type of random forest: classification
 #Number of trees: 500
 #No. of variables tried at each split: 6
 #OOB estimate of  error rate: 7%
 
 #========================
 # Lineal regression 1
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
 
 #========================
 # Lineal regression 2
 #========================
 #estimate full model with variables of Random forest Gini NDC
 ndc.randomf <- lm(migration_and_displacement~m_lulucf + a_water + 
                      m_transport + a_environment + a_health + m_waste + a_agriculture + 
                      m_industries + a_coastal_zone + m_agriculture + a_lulucf + 
                      m_energy + a_cross_cutting_area + m_buildings + a_education + 
                      m_economy.wide + a_drm + a_social_development + a_urban + 
                      a_transport, data = data.model)
 summary(ndc.randomf)
 
 
 #========================
 # Less complicated classification model, proposed.
 #========================
 #library(dplyr)
 #library(tidymodels)
 #library(vip)
 #receta <-
   #recipe(migration_and_displacement~ ., data.model) %>%
   #step_normalize(all_predictors()) %>% 
   #step_naomit(all_predictors()) %>% 
  # prep()
 
 #rf.model <-
   #rand_forest(mtry = round(length(predictors) ^ 0.5)) %>%
   #set_engine("randomForest") %>% set_mode("classification")
 
# flujo <- workflow() %>% add_model(rf.model) %>% add_recipe(receta)
 
 #ajuste <- flujo %>% fit(data.model)
 
 # na_values <- c()
 # for (i in 1:ncol(data.model)) {
 #    na_values[i] <- sum(is.na(data.model[, i]))
 # }
 # na_values
 
# ajuste %>% pull_workflow_fit() %>% vip(geom = "point") + theme_bw()
 

