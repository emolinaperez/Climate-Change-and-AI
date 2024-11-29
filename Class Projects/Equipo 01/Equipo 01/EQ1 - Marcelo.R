getwd()
root<-"/Users/ferdinandg.m./Documents/GitHub/Climate-Change-and-AI/Data/NDC/Raw/"
file.names <- list.files(path =root, pattern = ".csv")
NDCData<-lapply(file.names,function(x){read.csv(paste0(root,x))})
NDCData<-do.call("rbind",NDCData)
indicators<-unique(NDCData$indicator_name)
indicators
alllm<-lm()