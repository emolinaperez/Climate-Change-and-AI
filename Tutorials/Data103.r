#read all files store in one single location
 root<-"C:\\Users\\Usuario\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\41. Climate Change and AI\\Data\\NDC\\Raw\\"

#first list all files and save these into a character vector
 file.names<-list.files(path =root, pattern = ".csv")

#now read all files and save them in a list

 NDCData<-lapply(file.names,function(x){read.csv(paste0(root,x))})
 NDCData<-do.call("rbind",NDCData)

#firt we need to work on understanding what is inside this data
#let's begin by looking at the different indicators
#remember we need to find a way to quantitively describe an NDC, that is a huge challenge, let's be creative

 indicators<-unique(NDCData$indicator_name)
 indicators

#there are 359 indicators, what is inside each one of them

#let's look at values in the first indicator
 i<-3
 explore<-subset(NDCData,indicator_name==indicators[i])
 explore$count<-1
 explore<-aggregate(list(count=explore$count),list(
                                                       global_category=explore$global_category,
                                                       indicator_name=explore$indicator_name,
                                                       value=explore$value
                                                       ),sum)
 explore<-explore[order(-explore$count),]


#what do we do from here? ideas?




#let's work first the null values to explore more
 null.values<-c("No Document Submitted","No specified measure")
 NDCData$value.numeric<-ifelse(NDCData$value==null.values[1],-1,1)
 NDCData$value.numeric<-ifelse(NDCData$value==null.values[2],0,1)

#policy questions:
#which indicators receive the most global effort ?
 explore<-aggregate(list(value.numeric=NDCData$value.numeric),list(
                                                                   global_category=NDCData$global_category,
                                                                   indicator_name=NDCData$indicator_name
                                                                    ),sum)
#with respect to mitigation
 explore<-subset(explore,global_category=="Adaptation")   #Mitigation
 explore<-explore[order(-explore$value.numeric),]
 explore

#which countries are doing the most
 explore<-aggregate(list(value.numeric=NDCData$value.numeric),list(
                                                                  global_category=NDCData$global_category,
                                                                  country=NDCData$country
                                                                   ),sum)
#with respect to mitigation
 explore<-subset(explore,global_category=="Mitigation")   #Mitigation
 explore<-explore[order(-explore$value.numeric),]
 explore







 out<-"C:\\Users\\Usuario\\OneDrive\\Edmundo-ITESM\\3.Proyectos\\41. Climate Change and AI\\Data\\NDC\\Indicators\\"
 for (i in 1:length(indicators))
 {
   explore<-subset(NDCData,indicator_name==indicators[i])
   explore$count<-1
   explore<-aggregate(list(count=explore$count),list(
                                                         global_category=explore$global_category,
                                                         indicator_name=explore$indicator_name,
                                                         value=explore$value
                                                         ),sum)
   explore<-explore[order(-explore$count),]
   write.csv(pivot,paste0(out,indicators[i],".csv"),row.names=FALSE)
 }

#create table fot nations codes
 nations_names<-data.frame(iso_code3=unique(NDCdata.best$iso_code3),country=unique(NDCdata.best$country))
 write.csv(nations_names,paste0(root,"nations_names_ndc.csv"),row.names=FALSE)
