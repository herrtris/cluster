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


# Uploading the location file - to connect the location data

farm_codes <- read_xlsx("Part0_mitHCCode_31.08.2022.xlsx", sheet = 2)
summary(farm_codes)

farm_codes <- farm_codes %>% select('Farmers code', 'Provinz, translated','Stadt, translated', 'Landkreis translated', 'Village translated', 'NCP Region')
summary(farm_codes)

farm_codes <- farm_codes %>% mutate(HCODE=as.character(`Farmers code`) ) 
str(farm_codes)
farm_codes <- farm_codes %>% select(-c("Farmers code"))

cgfd_xiao <- cgfd_xiao %>% mutate(HCODE=as.character(HCODE))
str(cgfd_xiao)

# joining data
test <- left_join(cgfd_xiao, farm_codes)
str(cgfd_xiao)

summary(test)
summary(cgfd_xiao)
head(test)

# checking the duplicaes in the join
test %>% group_by(HCODE) %>% count(HCODE) %>% filter(n>1)

test%>% filter(HCODE=="1511101")     #will be removed in clustering             # 
test%>% filter(HCODE=="1513201")     #will be removed in clustering             # 
test%>% filter(HCODE=="1531201")     # has to be checked again                  #cleared
test%>% filter(HCODE=="1541102")     # remove one of them it is a duplicate
test%>% filter(HCODE=="1723105")     # remove one of them it is a duplicate
test%>% filter(HCODE=="1723106")     # will be removed in clustering
test%>% filter(HCODE=="1743105")     # needs to be checked


#treating duplicates

test2<-test%>% filter(!(`Village translated`=="Cow Camp" & HCODE=="1531201"))

test2 %>% filter(HCODE=="1511101" | HCODE=="1513201" | HCODE=="1541102") 
                   
test2<-test2%>% filter(!(`Stadt, translated`=="Luoyang" & HCODE=="1511101"))
test2<-test2%>% filter(!(`Village translated`=="Houyuchicun" & HCODE=="1743105"))
test2 %>% filter(HCODE=="1723105" | HCODE=="1723106" | HCODE=="1743105")

# The function distinct() [dplyr package] can be used to keep only unique/distinct rows from a data frame. 
# If there are duplicate rows, only the first row is preserved. It’s an efficient version of the R base function unique().
test2 <-distinct(test2)
                
test2%>% filter(HCODE=="1511101")     #will be removed in clustering              #  take second cleared
test2%>% filter(HCODE=="1513201")     #will be removed in clustering              #      
test2%>% filter(HCODE=="1531201")     # has to be checked again                   #     cleared
test2%>% filter(HCODE=="1541102")     # remove one of them it is a duplicate      # 
test2%>% filter(HCODE=="1723105")     # remove one of them it is a duplicate      # 
test2%>% filter(HCODE=="1723106")     # will be removed in clustering             # 
test2%>% filter(HCODE=="1743105")     # needs to be checked                       # here want first

# duplicates are treated now
cgfd_xiao<-test2         

rm(test)           
rm(test2)

summary(cgfd_xiao)

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
summary(farms_clustered)

# visualizing clusters
ggplot(farms_clustered, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point()
ggplot(farms_clustered, aes(x=log(MaizeArea), y=TotalFert_new, color=factor(cluster)))+ geom_point() #some farms seem to have the same value for fertilizer application

# adding regional component of provinces
head(farms_clustered)

test<-cgfd_xiao %>% filter(!is.na(TotalFert_new) , !is.na(MaizeArea))
test <- test %>% select(-c(MaizeArea, TotalFert_new))

plot_clustering_1 <-cbind(test,farms_clustered)
summary(plot_clustering_1)
rm(test)

# chhecking test and cgfd_xiao must contain same values for Totalfert and maizearea
cgfd_xiao %>% filter(HCODE=="1511103")
plot_clustering_1 %>% filter(HCODE=="1511103")

# facet_wrap by different geographical scales
ggplot(plot_clustering_1, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point() +facet_wrap(~`Provinz, translated`)

# check the provinces villages
ggplot(plot_clustering_1%>%filter(`Provinz, translated`=="Hebei"), aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point() +
                        facet_wrap(~`Landkreis translated`)

ggplot(plot_clustering_1%>%filter(`Provinz, translated`=="Heilongjiang"), aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point() +
  facet_wrap(~`Landkreis translated`)

ggplot(plot_clustering_1%>%filter(`Provinz, translated`=="Henan"), aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point() +
  facet_wrap(~`Landkreis translated`)

ggplot(plot_clustering_1%>%filter(`Provinz, translated`=="Jilin"), aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point() +
  facet_wrap(~`Landkreis translated`)

ggplot(plot_clustering_1%>%filter(`Provinz, translated`=="Shandong"), aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point() +
  facet_wrap(~`Landkreis translated`)


# Despite clustering by county there is still a big variance in the data, despite seeing some similiarities in counties

# calculating the mean by cluster
mean<-farms_clustered %>% group_by(cluster) %>% summarise_all(list(mean))
n<-farms_clustered %>% group_by(cluster) %>% summarise(n=n())

farms_clustered <- cbind(mean,n$n)
farms_clustered %>% filter(cluster<5)
farms_clustered


##################### clustering with kmeans method #####################

# finding the right amount of clusters k with elbow method
tot_wihinss <- map_dbl(1:20, function(k){
                model<- kmeans(x= clus_sf_scaled, centers = k)
                model$tot.withinss})

elbow_df <- data.frame(
            k=1:20,
            tot_withinss=tot_wihinss)

print(elbow_df)
ggplot(elbow_df, aes(x=k, y=tot_wihinss))+geom_line()+ scale_x_continuous(breaks=1:20)

# judging from the plot I would choose k=4 or k=3 for clustering

model <- kmeans(clus_sf_scaled, centers=4)
farms_clustered_kmeans <- mutate(clus_sf, cluster=model$cluster)

farms_clustered_kmeans %>% group_by(cluster) %>% summarise(mean(TotalFert_new), mean(MaizeArea), n=n())

ggplot(farms_clustered_kmeans, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point()



############################################################### silhouette analysis k means ######################################################
# How many ks are necessary
###################################################################################################################################################

# "right" amount of k using silhouette analysis
sil_width <- map_dbl(2:20, function(k) {
             model <- pam(x=clus_sf_scaled, k=k)
             model$silinfo$avg.width})

sil_df <- data.frame(k=2:20, sil_width=sil_width)

print(sil_df)
ggplot(sil_df, aes(x=k, y=sil_width))+geom_line()+ scale_x_continuous(breaks=1:20)

sil_df %>% arrange(desc(sil_width))

# The silhouette width is more difficult to interpret
        # highest for 18 - 20 - 17 - 19 - 16 15 - 14 - 2

        # Lets try k means with 18 clusters

model_18 <- kmeans(clus_sf_scaled, centers=18)
farms_clustered_kmeans_18 <- mutate(clus_sf, cluster=model_18$cluster)

#farms_clustered_kmeans_18 %>% group_by(cluster) %>% summarise_all(list(mean))
ggplot(farms_clustered_kmeans_18, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point()


### ######################################################################################################
#### Clustering while including maize yield ##########################################################
####################################################################################################

# 3 variables: totalfertnew, maizearea, maizeyield



clus_sfy <- cgfd_xiao %>% select(TotalFert_new, MaizeArea, maizeyield3)
summary(clus_sfy)

# eliminate NAs
clus_sfy <- clus_sfy %>% filter(!is.na(TotalFert_new), !is.na(MaizeArea), !is.na(maizeyield3))

# scaling
clus_sfy_scaled <-scale(clus_sfy)
summary(clus_sfy_scaled)

# calculate the distance
dis_clus_sfy_scaled <- dist(clus_sfy_scaled, method="euclidean")

# perform clustering
hc_dis_clus_sfy_scaled <- hclust(dis_clus_sfy_scaled, method="complete")


# create dendogram
dend_clus_sfy <- as.dendrogram(hc_dis_clus_sfy_scaled)
plot(dend_clus_sfy)

# deciding on the k, or the height so to say
dend_colored_sfy <- color_branches(dend_clus_sfy, h=5)
plot(dend_colored_sfy)

cluster_assignments_sfy <- cutree(hc_dis_clus_sfy_scaled, h=5)

#Final step, add these clusters to the dataset, cut at h=5 leads to 6 cluster
farms_clustered_sfy <- mutate(clus_sfy, cluster=cluster_assignments_sfy)
summary(farms_clustered_sfy)

# visualizing clusters pairwise
ggplot(farms_clustered_sfy, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point()
ggplot(farms_clustered_sfy, aes(x=MaizeArea, y=maizeyield3, color=factor(cluster))) + geom_point()

ggplot(farms_clustered_sfy, aes(x=log(MaizeArea), y=TotalFert_new, color=factor(cluster))) + geom_point()


# Can I get this into a 3D plot?
#install.packages("scatterplot3d")
library(scatterplot3d)

scatterplot3d(x=farms_clustered_sfy$MaizeArea, y=farms_clustered_sfy$TotalFert_new, z=farms_clustered_sfy$maizeyield3, color = factor(farms_clustered_sfy$cluster))

scatterplot3d(x=farms_clustered_sfy$MaizeArea, y=farms_clustered_sfy$maizeyield3, z=farms_clustered_sfy$TotalFert_new, color = factor(farms_clustered_sfy$cluster))


# calculating the mean by cluster
means <-farms_clustered_sfy %>% group_by(cluster) %>% summarise(mean(TotalFert_new), mean(MaizeArea), mean(maizeyield3), n=n())  

means %>% arrange(desc(`mean(TotalFert_new)`))
means[-c(6,7),] %>% arrange(desc(`mean(TotalFert_new)`))

##Excluding the ones with n<3, leaves 3 clusters using hierarchical clustering
#5 high input - high yield, 1 high input medium yield, 4 medium input high yield, 2 medium input low yield, 3 low input low yield

##################################### adding k means ############################################################################

# finding the right amount of clusters k with elbow method
tot_wihinss <- map_dbl(1:20, function(k){
  model<- kmeans(x= clus_sfy_scaled, centers = k)
  model$tot.withinss})

elbow_df <- data.frame(
  k=1:20,
  tot_withinss=tot_wihinss)

print(elbow_df)
ggplot(elbow_df, aes(x=k, y=tot_wihinss))+geom_line()+ scale_x_continuous(breaks=1:20)

# diffcult to judge about a k from this plot perhaps 5 or 6?? siluoette method??


# "right" amount of k using silhouette analysis
sil_width <- map_dbl(2:20, function(k) {
  model <- pam(x=clus_sfy_scaled, k=k)
  model$silinfo$avg.width})

sil_df <- data.frame(k=2:20, sil_width=sil_width)

print(sil_df)
ggplot(sil_df, aes(x=k, y=sil_width))+geom_line()+ scale_x_continuous(breaks=1:20)

sil_df %>% arrange(desc(sil_width))


# judging from the plots I would choose k=8 for clustering

model <- kmeans(clus_sfy_scaled, centers=8)
farms_clustered_kmeans <- mutate(clus_sfy, cluster=model$cluster)

means <-farms_clustered_kmeans %>% group_by(cluster) %>% summarise(mean(TotalFert_new), mean(MaizeArea),mean(maizeyield3), n=n())

ggplot(farms_clustered_kmeans, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point()

means %>% arrange(desc(`mean(TotalFert_new)`))

############################################################################################################
############################ Clusterin including 4 variables ###############################################
############################################################################################################

# TotalFert_New, MaizeArea, Grainincome, maizeyield

clus_sfyg <- cgfd_xiao %>% select(TotalFert_new, MaizeArea, maizeyield3, Grainincome)
summary(clus_sfyg)

# eliminate NAs
clus_sfyg <- clus_sfyg %>% filter(!is.na(TotalFert_new), !is.na(MaizeArea), !is.na(maizeyield3), !is.na(Grainincome))

# scaling
clus_sfyg_scaled <-scale(clus_sfyg)
summary(clus_sfyg_scaled)

# calculate the distance
dis_clus_sfyg_scaled <- dist(clus_sfyg_scaled, method="euclidean")

# perform clustering
hc_dis_clus_sfyg_scaled <- hclust(dis_clus_sfyg_scaled, method="complete")


# create dendogram
dend_clus_sfyg <- as.dendrogram(hc_dis_clus_sfyg_scaled)
plot(dend_clus_sfyg)

# deciding on the k, or the height so to say
dend_colored_sfyg <- color_branches(dend_clus_sfyg, h=5)
plot(dend_colored_sfyg)

cluster_assignments_sfyg <- cutree(hc_dis_clus_sfyg_scaled, h=5)

#Final step, add these clusters to the dataset, cut at h=5 leads to 7 cluster
farms_clustered_sfyg <- mutate(clus_sfyg, cluster=cluster_assignments_sfyg)
summary(farms_clustered_sfyg)


# visualizing clusters
ggplot(farms_clustered_sfyg, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point()
ggplot(farms_clustered_sfyg, aes(x=log(MaizeArea), y=TotalFert_new, color=factor(cluster)))+ geom_point() 



# calculating the mean by cluster
means_sfyg <-farms_clustered_sfyg %>% group_by(cluster) %>% summarise(mean(TotalFert_new), mean(MaizeArea), mean(maizeyield3), mean(Grainincome), n=n())  

means_sfyg %>% arrange(desc(`mean(TotalFert_new)`))


################################## Continued with kmeans #############################################################################################

# finding the right amount of clusters k with elbow method
tot_wihinss <- map_dbl(1:20, function(k){
  model<- kmeans(x= clus_sfyg_scaled, centers = k)
  model$tot.withinss})

elbow_df <- data.frame(
  k=1:20,
  tot_withinss=tot_wihinss)

print(elbow_df)
ggplot(elbow_df, aes(x=k, y=tot_wihinss))+geom_line()+ scale_x_continuous(breaks=1:20)

# Nice!! Elbow method also leads to clustering of K=7

model_sfyg <- kmeans(clus_sfyg_scaled, centers=7)


farms_clustered_kmeans_sfyg <- mutate(clus_sfyg, cluster=model_sfyg$cluster)
summary(farms_clustered_kmeans_sfyg)

means_sfyg <-farms_clustered_kmeans_sfyg %>% group_by(cluster) %>% summarise(mean(TotalFert_new), mean(MaizeArea), mean(maizeyield3), mean(Grainincome), n=n())  
means_sfyg

means_sfyg %>% arrange(desc(`mean(TotalFert_new)`))



ggplot(farms_clustered_kmeans_sfyg, aes(x=MaizeArea, y=TotalFert_new, color=factor(cluster)))+ geom_point()


################## checking silhouette width for completion #########################
# "right" amount of k using silhouette analysis
sil_width <- map_dbl(2:20, function(k) {
  model <- pam(x=clus_sfyg_scaled, k=k)
  model$silinfo$avg.width})

sil_df <- data.frame(k=2:20, sil_width=sil_width)

print(sil_df)
ggplot(sil_df, aes(x=k, y=sil_width))+geom_line()+ scale_x_continuous(breaks=1:20)

sil_df %>% arrange(desc(sil_width))

# Silhouette analysis indicates rather higher level for k, but is "okay" at k=7...


### adding a more "regional" perspective, find out a share - how many of which province belong to specifc cluster

########################################### Province Perspective #################################################


# adding regional component of provinces
head(farms_clustered_kmeans_sfyg)

test<-cgfd_xiao %>% filter(!is.na(TotalFert_new), !is.na(MaizeArea), !is.na(maizeyield3), !is.na(Grainincome))
test <- test %>% select(-c(MaizeArea, TotalFert_new, maizeyield3, Grainincome))

str(test)
str(farms_clustered_kmeans_sfyg)

# assumption Reihenfolge bleibt gleich in datasets, short check
cgfd_xiao %>% select(TotalFert_new, MaizeArea, maizeyield3, Grainincome) %>% 
              filter(!is.na(TotalFert_new), !is.na(MaizeArea), !is.na(maizeyield3), !is.na(Grainincome)) %>%
              head(n=5)

farms_clustered_kmeans_sfyg %>% head(n=5)

## the order of the farms stays the same I can combine them

# somth does not work out there should be the same means etc and ns as before

means_sfyg <-farms_clustered_kmeans_sfyg %>% group_by(cluster) %>% summarise(mean(TotalFert_new), mean(MaizeArea), mean(maizeyield3), mean(Grainincome), n=n())  
means_sfyg
means_sfyg %>% arrange(desc(`mean(TotalFert_new)`))


clustering_kmeans_sfyg <-cbind(test,farms_clustered_kmeans_sfyg)
summary(plot_clustering_1)


means_combined <-clustering_kmeans_sfyg %>% group_by(cluster) %>% summarise(mean(TotalFert_new), mean(MaizeArea), mean(maizeyield3), mean(Grainincome), n=n())  
means_combined
means_combined %>% arrange(desc(`mean(TotalFert_new)`))





pivot<-clustering_kmeans_sfyg %>% group_by(cluster, `Provinz, translated`) %>% count() 
pivot

# Stacked
sp <-ggplot(pivot, aes(fill=cluster, y=n, x=`Provinz, translated`) ) + 
  geom_bar(position="stack", stat="identity") 

sp

ggplot(pivot, aes(fill=`Provinz, translated`, y=n, x=cluster) ) + 
  geom_bar(position="stack", stat="identity") +scale_x_continuous(breaks=1:7)












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
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



