#read all files store in one single location
 root<-"C:\\Users\\Usuario\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\41. Climate Change and AI\\Data\\NDC\\Raw\\"

#first list all files and save these into a character vector
 file.names <- list.files(path =root, pattern = ".csv")

#now read all files and save them in a list

 NDCData<-lapply(file.names,function(x){read.csv(paste0(root,x))})
 NDCData<-do.call("rbind",NDCData)

#QA Note: check my indicator_id for duplicates

#let's focus only on adaptation or mitigation indicators
 target<-c("Adaptation","Mitigation")
 dim(NDCData)
 NDCData<-subset(NDCData,global_category%in%target)
 dim(NDCData)

#which indicators are included here
  indicators<-unique(NDCData$indicator_name)

#clean indicators names
 indicators_names<-gsub("/","_",indicators)
 indicators_names<-gsub(" ","_",indicators_names)
 indicators_names<-gsub("\\(","_",indicators_names)
 indicators_names<-gsub("\\)","_",indicators_names)
 indicators_names<-gsub("%","",indicators_names)
 indicators_names<-gsub(":","",indicators_names)

#indicate folder where the files will be store
 out<-"C:\\Users\\Usuario\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\41. Climate Change and AI\\Data\\NDC\\Indicators\\"

for (i in 1:length(indicators))
{
#  i<-7
  explore<-subset(NDCData,indicator_name==indicators[i])
  explore$count<-1
  explore<-aggregate(list(count=explore$count),list(
                                                        global_category=explore$global_category,
                                                        indicator_name=explore$indicator_name,
                                                        value=explore$value
                                                        ),sum)
  explore<-explore[order(-explore$count),]
  explore

#find out unique values
  cs<-unique(unlist(strsplit(explore$value, ",")))
  cs<-subset(cs,!(cs%in%c("No specified measure","No Document Submitted","Not Specified")))

  for (j in 1:length(cs))
   {
    explore[,cs[j]]<-unlist(lapply(explore$value,function(x) {  ifelse(grepl(cs[j],x,fixed=TRUE)==TRUE,1,0)} ))
   }

#estimate global score
 explore$dummy<-0
 explore$GlobalScore<-rowSums(explore[,c(cs,"dummy")])
 explore$dummy<-NULL
 write.csv(explore,paste0(out,indicators_names[i],".csv"),row.names=FALSE)
}


#now that we have processed all the indicators, we should work in merging global scores to the original data
 file.names <- list.files(path =out, pattern = ".csv")
 ScoresTable<-lapply(file.names,function(x){read.csv(paste0(out,x))[,c("global_category","indicator_name","value","GlobalScore")]})
 ScoresTable<-do.call("rbind",ScoresTable)

#let's merge the global score with NDC Data
 NDCData$Index<-paste(NDCData$global_category,NDCData$indicator_name,NDCData$value,sep="_")
 ScoresTable$Index<-paste(ScoresTable$global_category,ScoresTable$indicator_name,ScoresTable$value,sep="_")
#check dimmensions first
 dim(NDCData)
 dim(ScoresTable)

#make sure all index in the data to add, exist in the data that will be merged
 unique(unique(ScoresTable$Index)%in%unique(NDCData$Index))
#if all ture, then you can make the merged
  NDCData<-merge(NDCData,ScoresTable[c("Index","GlobalScore")],by="Index",all.x=TRUE)
  dim(NDCData)

#remove index
  NDCData$Index<-NULL

#aggregate scores such that we end with a table that has score, indicator id and country

NDCData<-aggregate(
                   list(Score=NDCData$GlobalScore),
                   list(
                        iso_code3=NDCData$iso_code3,
                        country=NDCData$country,
                        indicator_id=NDCData$indicator_id
                       ),
                   sum
                   )

#this data is in long format, to build a model, we need it in wide format
#install.packages("reshape2")
 NDCData<-reshape2::dcast(NDCData, iso_code3+country~indicator_id,value.var="Score")

#Now we need to add contries characteristics to this data
 wbdata<-"C:\\Users\\Usuario\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\41. Climate Change and AI\\Data\\WorldBank\\CountryProfiles\\"
 data.file<-"e2b2cf85-3950-4b71-905e-915606819087_Data.csv"
 wbdata<-read.csv(paste0(wbdata,data.file))

#first let's make sure we can merge this data to our data base
#countries of wb data in NDCData
  unique(unique(wbdata$Country.Code)%in%unique(NDCData$iso_code3))
#we see true and false, so which countries are missing
  subset(unique(wbdata$Country.Code),!(unique(wbdata$Country.Code)%in%unique(NDCData$iso_code3)))

#countries of NDCData in wbdata
  unique(unique(NDCData$iso_code3)%in%unique(wbdata$Country.Code))
  subset(unique(NDCData$iso_code3),!(unique(NDCData$iso_code3)%in%unique(wbdata$Country.Code)))
#let's have a look
  subset(NDCData,!(iso_code3%in%unique(wbdata$Country.Code)))

#we can disregard this missing values


#to merge wbdata into NDCData we have to change the format of this data from wide to long,
 colnames(wbdata)<-c("Country.Name","iso_code3","Series.Name","Series.Code",as.character(2000:2015))
 wbdata<-reshape2::melt(wbdata, id.vars = c("Country.Name","iso_code3","Series.Name","Series.Code"), measure.vars = as.character(2000:2015))
 wbdata$valuen<-as.numeric(wbdata$value)

#let's subset to the last year of the series
 wbdata<-subset(wbdata,variable==2015)

#now reshape to long format
 wbdata<-subset(wbdata,Series.Code!="")
 wbdata<-reshape2::dcast(wbdata, iso_code3~Series.Code,value.var="valuen")

#finally, let's merge the data into the original data set
 dim(NDCData)
 dim(wbdata)
 ModelData<-merge(NDCData,wbdata,by="iso_code3")

#write model data
 dir.out<-"C:\\Users\\Usuario\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\41. Climate Change and AI\\Data\\Model\\"
 write.csv(ModelData,paste0(dir.out,"ModelData.csv"),row.names=FALSE)
