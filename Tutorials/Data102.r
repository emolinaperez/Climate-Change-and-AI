#load the libraries
library(httr)
library(jsonlite)


#NDC
 url <- "https://www.climatewatchdata.org/api/v1/data/ndc_content"
 resNDC <- GET(url)
 total<-as.numeric(resNDC$headers$total)
 per.page<-as.numeric(resNDC$headers['per-page'])
 pages<-ceiling(total/per.page)

#get all
 NDCdata.best<-lapply(c(1:pages),function(x){
                             resNDC<-GET(url,query= list(page=x));
                              fromJSON(rawToChar(resNDC$content))$data
                              }
                       )

 NDCdata.best<-do.call(rbind,NDCdata.best)
 # you can see we get the same object
 dim(NDCdata.best)

#Get list of all countries included in the database
 nations<-unique(NDCdata.best$iso_code3)

#save all data in individual tables
 root<-"C:\\Users\\Usuario\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\41. Climate Change and AI\\Data\\NDC\\Raw\\"
 for (i in 1:length(nations))
 {
   pivot<-subset(NDCdata.best,iso_code3==nations[i])
   write.csv(pivot,paste0(root,nations[i],"_ndc.csv"),row.names=FALSE)
 }

#create table fot nations codes
 nations_names<-data.frame(iso_code3=unique(NDCdata.best$iso_code3),country=unique(NDCdata.best$country))
 write.csv(nations_names,paste0(root,"nations_names_ndc.csv"),row.names=FALSE)
