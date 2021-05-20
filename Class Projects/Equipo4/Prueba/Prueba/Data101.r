#Downloading data from different repositories
# we will first collect and explore the data available at https://www.climatewatchdata.org

#install the required libraries
#install.packages(c("httr", "jsonlite"))

#load the libraries
library(httr)
library(jsonlite)

#NDC
url <- "https://www.climatewatchdata.org/api/v1/data/ndc_content"

#make a get request

resNDC <- GET(url)

#transform this to actual data

NDCdata<-fromJSON(rawToChar(resNDC$content))

#NDCdata is list, let's explore its structure

str(NDCdata)

#so we are getting a data frame with only 50 lines, this is obviously incomplete,
#let's learn more about this by exploring the structure of resNDC
str(resNDC)

#we should also explore what the GET function is doing, try the following

?GET

# in this website we can learn about what parameters we can use to control the data request
#https://www.climatewatchdata.org/data-explorer/ndc-content?ndc-content-categories=unfccc_process&ndc-content-countries=All%20Selected&ndc-content-indicators=All%20Selected&ndc-content-sectors=All%20Selected&page=1
#So it looks like we can refine this, let's try the process again, but now using the countries field to access the data of the European Union
resNDC<-GET(url,query= list(countries = "EUU", page=1))
NDCdata<-fromJSON(rawToChar(resNDC$content))

#what sort of object NDCdata is ?
class(NDCdata)

#what is the structure
str(NDCdata)

#it looks we are only getting 50 observations per hit, let's look at the structure of resNDC again
str(resNDC)

#so under headers, the field total says there 437 rows associated with this country, and the per page header says we are getting 50 rows per packages
#we will need a loop to get all this data, let's try the following
#first, let's set the parameters for the loop
total<-as.numeric(resNDC$headers$total)
per.page<-as.numeric(resNDC$headers['per-page'])
pages<-ceiling(total/per.page)

#one way of doing this is using the for function
#intialize the loop
resNDC<-GET(url,query= list(countries = "EUU", page=1))
NDCdata<-fromJSON(rawToChar(resNDC$content))$data
for(i in 2:pages)
{
  resNDC<-GET(url,query= list(countries = "EUU", page=i))
  pivot<-fromJSON(rawToChar(resNDC$content))$data
  NDCdata<-rbind(NDCdata,pivot)
}

#check the dimmensions of this new object and see if the loop worked
dim(NDCdata)

#it looks like it worked, but maybe we use too many lines to do this, we can do the same process with fewer lines using lapply
NDCdata.best<-lapply(c(1:pages),function(x){
  resNDC<-GET(url,query= list(countries = "EUU", page=x));
  fromJSON(rawToChar(resNDC$content))$data
}
)

NDCdata.best<-do.call(rbind,NDCdata.best)
class(NDCdata.best)
#names(NDCdata.best)
#class(NDCdata[[1]])
# you can see we get the same object
dim(NDCdata.best)
head(NDCdata.best)

#Next we need to do this for all countries and all available data in this repository
#let's first begin with more countries
#

colnames(NDCdata)
names(NDCdata)
summary(NDCdata)
#get unique values of variable
unique(NDCdata$iso_code3)
#unique(NDCdata$country)
#unique(NDCdata$global_category)
#lets do all at once
sapply(NDCdata, unique)
sapply(NDCdata, max)

#En todo el mundo_______________________________

#Proceso/Consejo= ¿Cual es la idea general?
#1) Obtener un vector con las claves de los países
#2) estimar cuantas páginas por país hay
#3) En función de eso, programar mi función lapply para que haga rbind en todas las tablas
#4) Quiero que guarde un  csv de cada país en mi carpeta
#5) Este proceso tiene que ocurrir en todos los países

#segund aopción
#jalo la base completa y  el loop corree 2003
#guardo el archivo 

##NDC
url <- "https://www.climatewatchdata.org/api/v1/data/ndc_content"

#make a get request

resNDC <- GET(url)
NamesC<-
  for(i in 1:lenght(NamesC))
  {
    NDCdata.best<-lapply(c(1:pages),function(x){
      resNDC<-GET(url,query= list(countries = NamesC[i], page=x));
      fromJSON(rawToChar(resNDC$content))$data
    }
    )
    NDCdata.best<-do.call(rbind,NDCdata.best)
  }