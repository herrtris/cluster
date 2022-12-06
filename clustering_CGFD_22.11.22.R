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
library(purrr)

getwd()
setwd("~/Landwirtschaftliche Betriebslehre/Projekt_china_data/Dataclustering")

cgfd_xiao <- read.table("Yumi_merged_final.csv", dec=".", sep=";", head=TRUE)
glimpse(cgfd_xiao)
summary(cgfd_xiao)
head(cgfd_xiao, 10)

cgfd_xiao %>% filter(!is.na(MaizeArea), !is.na(maizeyield3), !is.na(TotalFert_new)) %>% count() # including maizeyield into the clustering leads to loss of additional 31 farms

################### How to approach Cluster analysis ####################
# Def.: explanatory data analysis where obs are divided into meaningful groups that share common characteristics#

# Preconditions: 
                # 1. No missing data
                # 2. Select the features
                # 3. features need to be on a similiar scale, convert the varibles if they differ largely in the average or the expected variability
                #    convert and standardize if necessary e.g. by (obs-mean)/sd()
                #    or use the scale function for that - normalizes each feature to a mean of zero and a variance of 1

# 4. Calculate the distance using the dist function, choose method euclidean or binary depending on the data (dummification might be needed for latter)
#     dist_players<-dist(players, method="euclidean")
# 
# 5. Do the clustering using the hclust function; choose the linkage criteria: complete, single, average
#     hc_players <- hclust(dist_players, method="complete")
#     
# 6. Exctracting the clusters, but k must be known for that, after that move to original dataset
# cluster_assignments<- cutree(players, k=2)
# players cluster <- mutate(players, cluster=cluster_assignments)
# after that visualize
    
#Hierarchical clustering
# # 7. Create dendogram first to decide upon the number of clusters
# dend_players <- as.dendogram(hc_players)
# dend_colored <- color_branches(dend_players, h=15)
# plot(dend_colored)
# 
# # 8. Decide on the heigt and therefor on the amount of clusters to consider using cutree() with height
# cluster_assignments <- cutree(hc_players, h=15)
# players_clustered <- mutate(players, cluster=cluster_assignments)



# 1st. clustering approach: Based on Xiaomin chemical fertilizer amount is correlated with farm size. Clustering for these to variables

clus_sf <- cgfd_xiao %>% select(TotalFert_new, MaizeArea)
summary(clus_sf)

# eliminate NAs for both variables
clus_sf <- clus_sf %>% filter(!is.na(TotalFert_new),!is.na(MaizeArea))
summary(clus_sf)

# median, mean is way different use scale to standaradize
clus_sf_scaled <-scale(clus_sf)
summary(clus_sf_scaled)

# calculate the distance
dis_clus_sf_scaled <- dist(clus_sf_scaled, method="euclidean")

# perform clustering
hc_dis_clus_sf_scaled <- hclust(dis_clus_sf_scaled, method="complete")


# create dendogram
dend_clus_sf <- as.dendrogram(hc_dis_clus_sf_scaled)
plot(dend_clus_sf)

# deciding on the k, or the height so to say
dend_colored <- color_branches(dend_clus_sf, h=3)
plot(dend_colored)

cluster_assignments <- cutree(hc_dis_clus_sf_scaled, h=3)

#Final step, add these clusters to the dataset, cut at h=3 leads to 6 cluster
farms_clustered <- mutate(clus_sf, cluster=cluster_assignments)
summary(farms_clusterd)

# visualizing clusters
ggplot(farms_clustered, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point()

# calculating the mean by cluster
farms_clustered %>% group_by(cluster) %>% summarise_all(list(mean))



##################### clustering with kmeans method #####################

# finding the right amount of clusters k with elbow method
tot_wihinss <- map_dbl(1:10, function(k){
                model<- kmeans(x= clus_sf_scaled, centers = k)
                model$tot.withinss})

elbow_df <- data.frame(
            k=1:10,
            tot_withinss=tot_wihinss)

print(elbow_df)
ggplot(elbow_df, aes(x=k, y=tot_wihinss))+geom_line()+ scale_x_continuous(breaks=1:10)

# judging from the plot I would choose k=4 for clustering

model <- kmeans(clus_sf_scaled, centers=4)
farms_clustered_kmeans <- mutate(clus_sf, cluster=model$cluster)

farms_clustered_kmeans %>% group_by(cluster) %>% summarise_all(list(mean))
ggplot(farms_clustered_kmeans, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point()


#änderungen etc


#---------------------------------------- Notes and first try
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
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



