# Clustering data, first tries on cleaned dataset for Xiaomins data
rm(list = ls())

# Load data

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
#install.packages("fastDummies")
library(fastDummies)
library(tibble)
library(cluster)
library(dendextend)

getwd()
setwd("~/Landwirtschaftliche Betriebslehre/Data_clustering")

cgfd_xiao <- read.table("Yumi_merged_final.csv", dec=".", sep=";", head=TRUE)
glimpse(cgfd_xiao)

summary(cgfd_xiao)
head(cgfd_xiao, 10)


test <- cgfd_xiao %>% select(HCODE,maizeyield3,TotalFert_new, PK)
glimpse(test)
test <- test %>% mutate(HCODE=as.factor(HCODE))
str(test)
glimpse(test)


ggplot(test, aes(x=TotalFert_new, y=maizeyield3))+geom_point()
ggplot(test, aes(x=PK, y=maizeyield3))+geom_point()

ggplot(gathered_o, aes(x = year, y = mean_salary, color = factor(cluster))) + 
  geom_line(aes(group = occupation))

###
## First time clustering, based on the fertilized amounts, and the farms

tfertfarm <- cgfd_xiao %>% select(HCODE, TotalFert_new, FertCost_New)
summary(tfertfarm)

tfertfarm_fil <- tfertfarm %>% filter(!is.na(TotalFert_new)) %>% filter(!is.na(FertCost_New))%>% mutate(HCODE=as.factor(HCODE))
summary(tfertfarm_fil)
tfertfarm_fil %>% count(TotalFert_new) %>% arrange(desc(n))

## 106 farms have the sane fertilization level.
dist_farms <- tfertfarm_fil %>% select(TotalFert_new, FertCost_New) %>% dist(method = "euclidean") 
hc_farms <- hclust(dist_farms, method="complete")


dend_farms <- as.dendrogram(hc_farms)
plot(dend_farms)

## what if I standardize first, both are not really comparable 
summary(tfertfarm_fil)
summary(cgfd_xiao)
mean(tfertfarm_fil$FertCost_New)
sd(tfertfarm_fil$FertCost_New)

tfertfarm_fil_stand <-   tfertfarm_fil %>% rowwise() %>% mutate(fertcost_stand= (FertCost_New/mean(tfertfarm_fil$FertCost_New))/sd(tfertfarm_fil$FertCost_New),TotalFert_stand= (TotalFert_new/mean(tfertfarm_fil$TotalFert_new))/sd(tfertfarm_fil$TotalFert_new) )
summary(tfertfarm_fil_stand)

### hierarchical clustering after stadardization

dist_farms_stan <- tfertfarm_fil_stand %>% select(TotalFert_stand, fertcost_stand) %>% dist(method = "euclidean") 
hc_farms <- hclust(dist_farms_stan, method="complete")

dend_farms <- as.dendrogram(hc_farms)
plot(dend_farms)

###Which variables are suited for data clustering into different farm types, for chemical fertilizer usage

#1 idea based on research conducted by Xiaomin
    # cluster the data based on farm size and fertilizer behaviour
    # does a same cluster appear as she described in her table?

glimpse()
glimpse(cgfd_xiao)

size_clases <- cgfd_xiao %>% select(HCODE, Farmsize,TotalFert_new)
ggplot(size_clases, aes(x=Farmsize, y=TotalFert_new))+geom_point()
size_clases %>% filter(Farmsize>100)

ggplot(size_clases, aes(x=log(Farmsize), y=log(TotalFert_new)))+geom_point()

head(size_clases, 10)

#standardize the data
size_clases <-   size_clases %>% rowwise() %>% mutate(Farmsize_stan= (Farmsize/mean(size_clases$Farmsize, na.rm=TRUE))/sd(size_clases$Farmsize, na.rm=TRUE),TotalFert_stand= (TotalFert_new/mean(size_clases$TotalFert_new, na.rm=TRUE))/sd(size_clases$TotalFert_new, na.rm=TRUE) )

mean(size_clases$Farmsize, na.rm=TRUE)
sd(size_clases$Farmsize, na.rm=TRUE)
summary(size_clases)

# hierarchical clustering







# Is there a relationship between fertilizer price and fertilizer application amount
test<-cgfd_xiao %>% filter(!is.na(TotalFert_new), !is.na(FertCost_New))
  
  
  cor(test$FertCost_New, test$TotalFert_new, method = "pearson")
  cor.test(test$FertCost_New, test$TotalFert_new)


# positive corrleation between price and application amount.... does not make sense if the price is higher the amount should reduce

  ggplot(test, aes(TotalFert_new, FertCost_New))+ geom_point()

# the higher the price, the higher is also the application amount... 

# How much overfertilization is happening?

  ## surpluses - yield und uptake minus die gabe, wenn dann surplus
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



