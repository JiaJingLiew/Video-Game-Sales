Sys.setenv(LANG="en")
setwd("~/Project 03")
library(tidyverse)
library(skimr)
library(janitor)
library(scales)
library(dplyr)
library(DataExplorer)
library(ggplot2)
salesps4<-read.csv("~/Project 03/PS4_GamesSales.csv")
salesxbox<-read.csv("~/Project 03/XboxOne_GameSales.csv")
glimpse(salesps4)
glimpse(salesxbox)
salesxbox<-salesxbox[,-1]
glimpse(salesxbox)
salesps4 %>% plot_intro()
salesps4 %>% plot_missing()
salesps4 %>% profile_missing()
salesxbox %>% plot_intro()
salesxbox %>% plot_missing()
salesxbox %>% profile_missing()

colSums(is.na(salesps4))
colSums(is.na(salesxbox))

salesps4<-filter(salesps4,!is.na(Global))
salesxbox<-filter(salesxbox,!is.na(Global))
colSums(is.na(salesps4))
colSums(is.na(salesxbox))

salesps4<-subset(salesps4,Year<="2020")
salesxbox<-subset(salesxbox,Year<="2020")

ggplot(salesps4,aes(x=Year,y=Global))+geom_bar(stat='identity',color='lightblue')+labs(x='Year',y='Global Sales',title="Global Sales Volume from 2013 to 2020",tag="PS4")+geom_text(aes(x=1,y=70),label=sum(salesps4$Global))
ggplot(salesxbox,aes(x=Year,y=Global))+geom_bar(stat='identity',color='lightgreen')+labs(x='Year',y='Global Sales',title="Global Sales Volume from 2013 to 2020",tag="XBox")+geom_text(aes(x=1,y=70),label=sum(salesxbox$Global))

salesps4genre<-salesps4 %>% group_by(Genre) %>% summarise(Global,North_America_percent=(North.America/Global)*100,Europe_percent=(Europe/Global)*100,Japan_percent=(Japan/Global)*100, Rest_of_World_percent=(Rest.of.World/Global)*100) %>% arrange(desc(Global))
salesxboxgenre<-salesxbox %>% group_by(Genre) %>% summarise(Global,North_America_percent=(North.America/Global)*100,Europe_percent=(Europe/Global)*100,Japan_percent=(Japan/Global)*100, Rest_of_World_percent=(Rest.of.World/Global)*100) %>% arrange(desc(Global))

ggplot(salesps4genre, aes(x =Genre,y =North.America))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "North.America Sales",title="North.America Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=160),label=sum(salesps4genre$North.America))
ggplot(salesps4genre, aes(x =Genre,y =Europe ))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Europe Sales",title="Europe  Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=160),label=sum(salesps4genre$Europe))
ggplot(salesps4genre, aes(x =Genre,y =Japan))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Japan Sales",title="Japan Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=160),label=sum(salesps4genre$Japan))
ggplot(salesps4genre, aes(x =Genre,y =Rest.of.World))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Rest.of.World Sales",title="Rest.of.World Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=160),label=sum(salesps4genre$Rest.of.World))
ggplot(salesps4genre, aes(x =Genre,y =Global))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Global Sales",title="Global Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=160),label=sum(salesps4genre$Global))

ggplot(salesxboxgenre, aes(x =Genre,y =North.America))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "North.America Sales",title="North.America Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$North.America))
ggplot(salesxboxgenre, aes(x =Genre,y =Europe ))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "Europe Sales",title="Europe  Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$Europe))
ggplot(salesxboxgenre, aes(x =Genre,y =Japan))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "Japan Sales",title="Japan Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$Japan))
ggplot(salesxboxgenre, aes(x =Genre,y =Rest.of.World))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "Rest.of.World Sales",title="Rest.of.World Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$Rest.of.World))
ggplot(salesxboxgenre, aes(x =Genre,y =Global))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "Global Sales",title="Global Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=160),label=sum(salesxboxgenre$Global))

salesps4 %>% group_by(Publisher) %>% summarise(Global,North_America_percent=(North.America/Global)*100,Europe_percent=(Europe/Global)*100,Japan_percent=(Japan/Global)*100, Rest_of_World_percent=(Rest.of.World/Global)*100) %>% arrange(desc(Global))
salesxbox %>% group_by(Publisher) %>% summarise(Global,North_America_percent=(North.America/Global)*100,Europe_percent=(Europe/Global)*100,Japan_percent=(Japan/Global)*100, Rest_of_World_percent=(Rest.of.World/Global)*100) %>% arrange(desc(Global))



videogamessales<-read.csv("~/Project 03/Video_Games_Sales_as_at_22_Dec_2016.csv")
glimpse(videogamessales)
colSums(is.na(videogamessales))
videogamessales %>% dplyr::filter(,!is.na(Global_Sales) & !is.na(Critic_Score) & !is.na(Critic_Count) & !is.na(User_Score) & !is.na(User_Count)) %>% head()
videogamessales<-subset(videogamessales, Year_of_Release<="2020" & Year_of_Release>="2011")
head(videogamessales)

videogamessales %>% group_by(Genre) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))
videogamessales %>% group_by(Platform) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))
videogamessales %>% group_by(Publisher) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))
videogamessales %>% group_by(Developer) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

ggplot(videogamessales, aes(x =Genre,y =North.America))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "North.America Sales",title="North.America Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$North.America))
ggplot(videogamessales, aes(x =Genre,y =Europe ))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Europe Sales",title="Europe  Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$Europe))
ggplot(videogamessales, aes(x =Genre,y =Japan))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Japan Sales",title="Japan Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$Japan))
ggplot(videogamessales, aes(x =Genre,y =Rest.of.World))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Rest.of.World Sales",title="Rest.of.World Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$Rest.of.World))
ggplot(videogamessales, aes(x =Genre,y =Global))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Global Sales",title="Global Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=160),label=sum(salesps4genre$Global))

ggplot(videogamessales, aes(x =Platform,y =North.America))+geom_bar(stat="identity", color='green')+labs(x = "Platform", y = "North.America Sales",title="North.America Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$North.America))
ggplot(videogamessales, aes(x =Platform,y =Europe ))+geom_bar(stat="identity", color='green')+labs(x = "Platform", y = "Europe Sales",title="Europe  Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$Europe))
ggplot(videogamessales, aes(x =Platform,y =Japan))+geom_bar(stat="identity", color='green')+labs(x = "Platform", y = "Japan Sales",title="Japan Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$Japan))
ggplot(videogamessales, aes(x =Platform,y =Rest.of.World))+geom_bar(stat="identity", color='green')+labs(x = "Platform", y = "Rest.of.World Sales",title="Rest.of.World Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=90),label=sum(salesxboxgenre$Rest.of.World))
ggplot(videogamessales, aes(x =Platform,y =Global))+geom_bar(stat="identity", color='green')+labs(x = "Platform", y = "Global Sales",title="Global Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=160),label=sum(salesps4genre$Global))















































































































































































































