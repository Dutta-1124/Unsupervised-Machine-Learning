rm(list = ls())
setwd("C:/Users/shiva/OneDrive/Desktop/R working directory/MODULE-5")

getwd()

library(tidyverse)
library(ggplot2)
library(arules)
install.packages("arulesViz")
library(arulesViz)
library(dplyr)
library(readxl)
library(lubridate)

retail<- read_excel(file.choose())
str(retail)
dim(retail)
str(retail$Description)
retail<- retail[complete.cases(retail),]

retail %>% mutate(Description=as.factor(Description))
retail %>% mutate(Country= as.factor(City))

retail$Date <- as.Date(retail$InvoiceDate)
retail$Time<- format(retail$InvoiceDate,"%H:%M:%S")

retail$InvoiceNo<- as.numeric(as.character(retail$InvoiceNo))

str(retail)

retail$Time<- as.Date(retail$Time)

#--- Time Vs Sales
library(ggplot2)
ggplot(retail,aes(retail$Time))+geom_bar()+xlab("Time")



#-- No. of items per Invoice
retail %>% group_by(InvoiceNo) %>% summarise(n_items= mean(Quantity)) %>% 
  ggplot(aes(n_items))+xlab("No of items")+ geom_histogram(fill="indianred",bins = 10)+ geom_rug())





retail_sorted
#--------
retail %>% group_by(StockCode,Description) %>% arrange(desc(Quantity))




retail %>% group_by(StockCode,Description) %>% arrange(desc(Quantity))

a<-group_by(StockCode, Description) %>% summarise(count= n(Quantity)) %>% 
  arrange(desc(count))


retail %>% ggplot(aes(x = reorder(Description,Quantity),y = Count))+geom_bar(stat = "identity",fill="indian red")+coord_flip()



#Association rules
retail_sorted<- retail[order(retail$CustomerID),] 


itemList<-plyr::ddply(retail,c("CustomerID","Date"),function(df1)paste(df1$Description,collapse = ","))

items<- plyr::ddply(retail,c("CustomerID","Date"),function(df1)paste(df1$Description,collapse = ","))
items

items<- plyr::ddply(retail,c("Description"), function(df1)paste(df1$Description,collapse = ","))


itemList$CustomerID<- NULL
itemList$Date<- NULL

colnames(itemList)<- "items"


write.csv(itemList,"market_basket.csv",quote = F,row.names = T)



tr<- read.transactions("market_basket.csv",format = "basket",sep = ",")

tr
summary(tr)


itemFrequencyPlot(tr,topN=20,type="absolute")

rules<- apriori(tr,parameter = list(supp=0.01,conf=0.7))
 
rules<- sort(rules,by="confidence",decreasing = T)
