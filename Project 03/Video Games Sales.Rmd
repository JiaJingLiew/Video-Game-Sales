---
title: 'Capstone Project: Video Games Sales'
author: "Jia Jing Liew"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
<h1>Introduction</h1>
<h4>
For the capstone project, I have selected the video game sales analysis to work on. For the case study, I will perform the junior data analyst for the marketing team at little game company where in Chicago. Now, my company want creating a new game, I need know which genre of game, who is the greatest consumers group. I will use RStudio for analysing.  
[Link](https://www.kaggle.com/datasets/sidtwr/videogames-sales-dataset)
</h4>
<h1>Ask</h1>
<h4>
1.How many game sales on global from 2013 to 2020?

2.Which area are the most sales volume?

3.Which kind of game is the most popular?Why it is the most popular genre?

4.In the global, is that having the lowest marketing place?

5.Which platform is the most popular?
</h4>
<h1>Prepare</h1>
<h4>Loading some packages.</h4>
```{r class.source = 'fold-hide'}
###Setting langguage and installing packages
Sys.setenv(LANG="en")
setwd("~/Project 03")
library(tidyverse)
library(skimr)
library(janitor)
library(scales)
library(dplyr)
library(DataExplorer)
library(ggplot2)
```

<h1>Process</h1>
<h4>Above we have already prepared the packages. Now, we are starting cleaning data. The data may be have some dirty data will affect analysis, so i using clean_names function. And, using plot_intro, plot_missing, profile_missing to ensure the structure of data. I use RStudio to clean and process data.</h4>
```{r class.source = 'fold-hide'}
###Loading the data
salesps4<-read.csv("~/Project 03/PS4_GamesSales.csv")
salesxbox<-read.csv("~/Project 03/XboxOne_GameSales.csv")
videogamessales<-read.csv("~/Project 03/Video_Games_Sales_as_at_22_Dec_2016.csv")

###Checking the data structure
glimpse(salesps4)
glimpse(salesxbox)
glimpse(videogamessales)
salesxbox<-salesxbox[,-1]
glimpse(salesxbox)

###Checking the data in visualization
salesps4 %>% plot_intro()
salesps4 %>% plot_missing()
salesps4 %>% profile_missing()
salesxbox %>% plot_intro()
salesxbox %>% plot_missing()
salesxbox %>% profile_missing()
videogamessales %>% plot_intro()
videogamessales %>% plot_missing()
videogamessales %>% profile_missing()
```

```{r class.source = 'fold-hide'}
###Cleaning the data
salesps4<-clean_names(salesps4)
salesxbox<-clean_names(salesxbox)
videogamessales<-clean_names(videogamessales)

###Ensure NA in your data
colSums(is.na(salesps4))
colSums(is.na(salesxbox))
colSums(is.na(videogamessales))
```

```{r}
###Filtering NA in data
salesps4<-filter(salesps4,!is.na(global))
salesxbox<-filter(salesxbox,!is.na(global))
videogamessales<-filter(videogamessales,!is.na(global_sales) & !is.na(critic_score) & !is.na(critic_count) & !is.na(user_score) & !is.na(user_count))

###Checking again data structure
salesps4 %>% plot_intro()
salesps4 %>% plot_missing()
salesps4 %>% profile_missing()
salesxbox %>% plot_intro()
salesxbox %>% plot_missing()
salesxbox %>% profile_missing()
videogamessales %>% plot_intro()
videogamessales %>% plot_missing()
videogamessales %>% profile_missing()

###Check NA again
colSums(is.na(salesps4))
colSums(is.na(salesxbox))
colSums(is.na(videogamessales))

###Processing data in period from 2013 to 2020
salesps4<-subset(salesps4,year<="2020")
salesxbox<-subset(salesxbox,year<="2020")
videogamessales<-subset(videogamessales, year_of_release<="2020" & year_of_release>="2013")
```

<h1>Analysis</h1>
<h4>From the above data has been cleaning and processing, now, I plot the barplot to show the games sales which on platform PS4 and XBox respectively, from 2013 to 2020. The graphs are showing below.</h4> 
```{r}
ggplot(salesps4,aes(x=year,y=north_america))+geom_bar(stat='identity',color='lightblue')+labs(x='Year',y='North America Sales',title="North America Sales Volume from 2013 to 2020",tag="PS4")+geom_text(aes(x=1,y=50),label=sum(salesps4$north_america))
ggplot(salesps4,aes(x=year,y=europe))+geom_bar(stat='identity',color='lightblue')+labs(x='Year',y='Europe Sales',title="Europe Sales Volume from 2013 to 2020",tag="PS4")+geom_text(aes(x=1,y=50),label=sum(salesps4$europe))
ggplot(salesps4,aes(x=year,y=japan))+geom_bar(stat='identity',color='lightblue')+labs(x='Year',y='Japan Sales',title="Japan Sales Volume from 2013 to 2020",tag="PS4")+geom_text(aes(x=1,y=20),label=sum(salesps4$japan))
ggplot(salesps4,aes(x=year,y=rest_of_world))+geom_bar(stat='identity',color='lightblue')+labs(x='Year',y='Rest of World Sales',title="Rest of World Sales Volume from 2013 to 2020",tag="PS4")+geom_text(aes(x=1,y=30),label=sum(salesps4$rest_of_world))
ggplot(salesps4,aes(x=year,y=global))+geom_bar(stat='identity',color='lightblue')+labs(x='Year',y='Global Sales',title="Global Sales Volume from 2013 to 2020",tag="PS4")+geom_text(aes(x=1,y=150),label=sum(salesps4$global))

ggplot(salesxbox,aes(x=year,y=north_america))+geom_bar(stat='identity',color='lightgreen')+labs(x='Year',y='North America Sales',title="North America Sales Volume from 2013 to 2020",tag="XBox")+geom_text(aes(x=1,y=50),label=sum(salesxbox$north_america))
ggplot(salesxbox,aes(x=year,y=europe))+geom_bar(stat='identity',color='lightgreen')+labs(x='Year',y='Europe Sales',title="Europe Sales Volume from 2013 to 2020",tag="XBox")+geom_text(aes(x=1,y=30),label=sum(salesxbox$europe))
ggplot(salesxbox,aes(x=year,y=japan))+geom_bar(stat='identity',color='lightgreen')+labs(x='Year',y='Japan Sales',title="Japan Sales Volume from 2013 to 2020",tag="XBox")+geom_text(aes(x=1,y=1.5),label=sum(salesxbox$japan))
ggplot(salesxbox,aes(x=year,y=rest_of_world))+geom_bar(stat='identity',color='lightgreen')+labs(x='Year',y='Rest of World Sales',title="Rest of World Sales Volume from 2013 to 2020",tag="XBox")+geom_text(aes(x=1,y=8),label=sum(salesxbox$rest_of_world))
ggplot(salesxbox,aes(x=year,y=global))+geom_bar(stat='identity',color='lightgreen')+labs(x='Year',y='Global Sales',title="Global Sales Volume from 2013 to 2020",tag="XBox")+geom_text(aes(x=1,y=70),label=sum(salesxbox$global))
```
<h4>Both PS4 and XBox, in the Japan marketing is the lowest. </h4>

<h4>In here, I show the genre game which presented the most popular genre.</h4> 
```{r}
salesps4genre<-salesps4 %>% group_by(genre) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))
salesps4genre

salesxboxgenre<-salesxbox %>% group_by(genre) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))
salesxboxgenre
```
<h4>Above tables are showing the game sales and the genre of game, 'Action' is the most popular genre.</h4> 

```{r}
ggplot(salesps4, aes(x =genre,y = north_america))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "North.America Sales",title="North.America Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=75),label=sum(salesps4$north_america))
ggplot(salesps4, aes(x =genre,y =europe ))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Europe Sales",title="Europe  Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=75),label=sum(salesps4$europe))
ggplot(salesps4, aes(x =genre,y =japan))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Japan Sales",title="Japan Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=13),label=sum(salesps4$japan))
ggplot(salesps4, aes(x =genre,y =rest_of_world))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Rest.of.World Sales",title="Rest.of.World Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=30),label=sum(salesps4$rest_of_world))
ggplot(salesps4, aes(x =genre,y =global))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Global Sales",title="Global Sales Volume Of Games Genre",tag="PS4")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=160),label=sum(salesps4$global))

ggplot(salesxbox, aes(x =genre,y = north_america))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "North.America Sales",title="North.America Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=60),label=sum(salesxbox$north_america))
ggplot(salesxbox, aes(x =genre,y =europe ))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "Europe Sales",title="Europe  Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=30),label=sum(salesxbox$europe))
ggplot(salesxbox, aes(x =genre,y =japan))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "Japan Sales",title="Japan Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=5),label=sum(salesxbox$japan))
ggplot(salesxbox, aes(x =genre,y =rest_of_world))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "Rest.of.World Sales",title="Rest.of.World Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=10),label=sum(salesxbox$rest_of_world))
ggplot(salesxbox, aes(x =genre,y =global))+geom_bar(stat="identity", color='green')+labs(x = "Genre", y = "Global Sales",title="Global Sales Volume Of Games Genre",tag="XBox")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=100),label=sum(salesxbox$global))

salesps4 %>% group_by(publisher) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))
salesxbox %>% group_by(publisher) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))
```
<h4>In the two type platforms, on PS4 the game of action is having the most sales, on XBox the game of 'Shooter' is the best sales.</h4>



```{r}
ggplot(videogamessales, aes(x =genre,y =na_sales))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "North.America Sales",title="North.America Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=100),label=sum(videogamessales$na_sales))
ggplot(videogamessales, aes(x =genre,y =eu_sales ))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Europe Sales",title="Europe  Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=100),label=sum(videogamessales$eu_sales))
ggplot(videogamessales, aes(x =genre,y =jp_sales))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Japan Sales",title="Japan Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=25),label=sum(videogamessales$jp_sales))
ggplot(videogamessales, aes(x =genre,y =other_sales ))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Rest.of.World Sales",title="Rest.of.World Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=30),label=sum(videogamessales$other_sales ))
ggplot(videogamessales, aes(x =genre,y =global_sales))+geom_bar(stat="identity", color='blue')+labs(x = "Genre", y = "Global Sales",title="Global Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=250),label=sum(videogamessales$global_sales))

ggplot(videogamessales, aes(x =platform,y =na_sales))+geom_bar(stat="identity", color='yellow')+labs(x = "Platform", y = "North.America Sales",title="North.America Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=80),label=sum(videogamessales$na_sales))
ggplot(videogamessales, aes(x =platform,y =eu_sales ))+geom_bar(stat="identity", color='yellow')+labs(x = "Platform", y = "Europe Sales",title="Europe  Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=110),label=sum(videogamessales$eu_sales))
ggplot(videogamessales, aes(x =platform,y =jp_sales))+geom_bar(stat="identity", color='yellow')+labs(x = "Platform", y = "Japan Sales",title="Japan Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=25),label=sum(videogamessales$jp_sales))
ggplot(videogamessales, aes(x =platform,y =other_sales ))+geom_bar(stat="identity", color='yellow')+labs(x = "Platform", y = "Rest.of.World Sales",title="Rest.of.World Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=50),label=sum(videogamessales$other_sales ))
ggplot(videogamessales, aes(x =platform,y =global_sales))+geom_bar(stat="identity", color='yellow')+labs(x = "Platform", y = "Global Sales",title="Global Sales Volume Of Games Genre")+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),axis.text.y = element_text(vjust = 1, hjust = 1, size = 12))+geom_text(aes(x=1,y=250),label=sum(videogamessales$global_sales))

videogamessales %>% group_by(platform) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))
videogamessales %>% group_by(genre) %>% summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>% arrange(desc(Count))

```
<h4>
In the all platform, action game is the most popular, and shooter is the second most popular, have percentage from all genre 31.7% and 13.5%, respectively. PS4 is the best sales in global, have the highest percentage from all platforms (25.5%).
</h4>

<h1>Conclusion</h1>
<h4>
From 2013 to 2020, the total game sales which on PS4 and XBox are 594.79 millions and 268.73 millions. For all platform, the total game sales is 714.07 millions. Is a huge number, North America is having the greatest sales in global, is the greatest consumers group in global, but Japan marketing is the smallest consumers group in global. In all genre game, action game and shooter game are the most popular, them have percentage from all genre 31.7% and 13.5%, respectively. The reason of causing this may be action game is easily control for playing, and the more action games have interesting and heroism story. Shooter game have a model, usually, it can choose two players or single player, it is a good design for increasing the game fun. All the platform, PS4 is the best sales in global, have the highest percentage from all platforms 25.5%.
</h4>

<h1>Share</h1>
<h2>[Video Games Sales](https://rpubs.com/blackskyjason1993/919358)</h2>
<h2>[Presentation: Video Games Sales](https://rpubs.com/blackskyjason1993/919377)</h2>