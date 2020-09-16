##Assignment 2 b110 - Market Segmentation
tivo <- read.csv("C:/Users/trang/OneDrive/Minerva/2nd year/B110/tivo cluster.csv", header = TRUE)
alltivo <- read.csv("C:/Users/trang/OneDrive/Minerva/2nd year/B110/alltivo.csv")
str(tivo)

#Convert factors columns into interger columns
factor.columns <- c(3,4,8)
for (i in factor.columns) {
  tivo[, i] <- as.integer(tivo[, i]) #change all dates into interger class
  
}
str(tivo)



#Normalization
z<- tivo[,-c(1,1)]
z
m <- apply(z, 2, mean)
s <- apply(z,2,sd)
z <- scale(z, m, s)
z


#Cacluating Euclidean distance
distance <- dist(z)

hc.c <- hclust(distance) #the cluster is packed because of many IDs and variables
plot(hc.c, hang=-1)

#cluster membership
member.c <- cutree(hc.c,5)
table(member.c)


#Conduct the correlation matrix to select variables

str(tivo)

tivo = tivo[,-8]
str(tivo)
tivo.cor = cor(tivo,  use = "complete.obs") #use to not count the NAs
df <- data.frame(round(tivo.cor**2,2))

write.csv(df,"cor max")

tivo_cor <- read.csv("C:/Users/trang/OneDrive/Minerva/2nd year/B110/tivo no outlier cor max.csv")
tivo_cor = tivo_cor[,-8]
tivo.cor = cor(tivo_cor,  use = "complete.obs") #use to not count the NAs
df <- data.frame(round(tivo.cor**2,2))
df



#Cluster based on TV time
set.seed(123)
first_cluster <- kmeans(na.omit(tivo$TV.Viewing..hours.day.),centers = 7) #eliminate NAs
first_cluster


## list of cluster assignments
o = order(first_cluster$cluster)
df_firstcluster <- data.frame(tivo$ID[o], first_cluster$cluster[o])


#Visualize

plot(tivo$TV.Viewing..hours.day., type="n", xlim=c(3,1000))
text(x=tivo$TV.Viewing..hours.day., labels=tivo$ID, col=first_cluster$cluster+1)

#k=7 gives better clusters




list_id1 <- with(df_firstcluster,df_firstcluster$tivo.ID.o.[df_firstcluster$first_cluster.cluster.o.==1])
length(list_id1)
list_id2 <- (with(df_firstcluster,df_firstcluster$tivo.ID.o.[df_firstcluster$first_cluster.cluster.o.==2]))
list_id3 <- (with(df_firstcluster,df_firstcluster$tivo.ID.o.[df_firstcluster$first_cluster.cluster.o.==3]))
list_id4 <- (with(df_firstcluster,df_firstcluster$tivo.ID.o.[df_firstcluster$first_cluster.cluster.o.==4]))
list_id5 <- (with(df_firstcluster,df_firstcluster$tivo.ID.o.[df_firstcluster$first_cluster.cluster.o.==5]))
list_id6 <- (with(df_firstcluster,df_firstcluster$tivo.ID.o.[df_firstcluster$first_cluster.cluster.o.==6]))
list_id7 <- (with(df_firstcluster,df_firstcluster$tivo.ID.o.[df_firstcluster$first_cluster.cluster.o.==7]))
###Why when I put (with) into c(with...), the length of list_id changes (smaller)????


library(dplyr)

#Segment 1
tivo_clus1 <- tivo %>% filter(tivo$ID %in% list_id1) #filter the ids with the Ids in list1


tivo_clus1$ID
mean(tivo1$Annual.Income..x1000...)
mean(tivo1$Monthly.Electronics.Spend)

#location
table(tivo1$Purchasing.Location)

#appealing feature
alltivo1 <- alltivo %>% filter(alltivo$ID %in% list_id1)
table(alltivo1$Purchasing.Location)
table(alltivo1$Favorite.feature)


#Segment 2

tivo_clus2 <- tivo %>% filter(tivo$ID %in% list_id2) #filter the ids with the Ids in list1
mean(tivo_clus2$TV.Viewing..hours.day.)
mean(tivo_clus2$Technology.Adoption)


tivo_clus2$ID
mean(tivo_clus2$Annual.Income..x1000...)
mean(tivo_clus2$Monthly.Electronics.Spend)
#location
table(tivo_clus2$Purchasing.Location)

#appealing feature
alltivo2 <- alltivo %>% filter(alltivo$ID %in% list_id2)
table(alltivo2$Purchasing.Location)
table(alltivo2$Favorite.feature)

#Segment 3

tivo_clus3 <- tivo %>% filter(tivo$ID %in% list_id3) #filter the ids with the Ids in list1
mean(tivo_clus3$TV.Viewing..hours.day.)
mean(tivo_clus3$Technology.Adoption)


length(tivo_clus3$ID)
mean(tivo_clus3$Annual.Income..x1000...)
mean(tivo_clus3$Monthly.Electronics.Spend)
#location
table(tivo_clus3$Purchasing.Location)
#appealing feature
alltivo3 <- alltivo %>% filter(alltivo$ID %in% list_id3)
table(alltivo3$Purchasing.Location)
table(alltivo3$Favorite.feature)


#Segment 4

tivo_clus4 <- tivo %>% filter(tivo$ID %in% list_id4) #filter the ids with the Ids in list1
tivo_clus4$ID
mean(tivo_clus4$TV.Viewing..hours.day.)
mean(tivo_clus4$Technology.Adoption)


mean(tivo_clus4$Annual.Income..x1000...)
mean(tivo_clus4$Monthly.Electronics.Spend)
#location
table(tivo_clus4$Purchasing.Location)
#appealing feature
alltivo4 <- alltivo %>% filter(alltivo$ID %in% list_id4)
table(alltivo4$Purchasing.Location)
table(alltivo4$Favorite.feature)

#Segment 5
tivo_clus5 <- tivo %>% filter(tivo$ID %in% list_id5) #filter the ids with the Ids in list1
mean(tivo_clus5$TV.Viewing..hours.day.)

tivo_clus5$ID
mean(tivo_clus5$Annual.Income..x1000...)
mean(tivo_clus5$Monthly.Electronics.Spend)
#location
table(tivo_clus5$Purchasing.Location)
#appealing feature
alltivo5 <- alltivo %>% filter(alltivo$ID %in% list_id5)
table(alltivo5$Purchasing.Location)
table(alltivo5$Favorite.feature)

#Segment 6
tivo_clus6 <- tivo %>% filter(tivo$ID %in% list_id6) #filter the ids with the Ids in list1
mean(tivo_clus6$TV.Viewing..hours.day.)
tivo_clus6$ID
tivo_clus6$ID
mean(tivo_clus6$Annual.Income..x1000...)
mean(tivo_clus6$Monthly.Electronics.Spend)
#location
table(tivo_clus6$Purchasing.Location)
#appealing feature
alltivo6 <- alltivo %>% filter(alltivo$ID %in% list_id6)
table(alltivo6$Purchasing.Location)
table(alltivo6$Favorite.feature)

#Segment 7
tivo_clus7 <- tivo %>% filter(tivo$ID %in% list_id7) #filter the ids with the Ids in list1
mean(tivo_clus7$TV.Viewing..hours.day.)

mean(tivo_clus7$Annual.Income..x1000...)
mean(tivo_clus7$Monthly.Electronics.Spend)
#location
table(tivo_clus7$Purchasing.Location)
#appealing feature
alltivo7 <- alltivo %>% filter(alltivo$ID %in% list_id7)
table(alltivo7$Purchasing.Location)
table(alltivo7$Favorite.feature)








##Come back to Kmeans method
set.seed(12)
second_cluster <- kmeans(na.omit(tivo$Monthly.Electronics.Spend),centers = 4) #eliminate NAs
second_cluster
second_cluster$centers
second_cluster$size


## list of cluster assignments
o = order(second_cluster$cluster)
df_secondcluster <- data.frame(tivo$ID[o], second_cluster$cluster[o])
df_secondcluster

#Visualize

plot(tivo$Monthly.Electronics.Spend, type="n", xlim=c(0,100))
text(x=tivo$Monthly.Electronics.Spend, y=tivo$ID, labels=tivo$ID, col=first_cluster$cluster+1)



list_id1 <- (with(df_secondcluster,df_secondcluster$tivo.ID.o.[df_secondcluster$second_cluster.cluster.o.==1]))
length(list_id1)
list_id2 <- (with(df_secondcluster,df_secondcluster$tivo.ID.o.[df_secondcluster$second_cluster.cluster.o.==2]))
list_id3 <- (with(df_secondcluster,df_secondcluster$tivo.ID.o.[df_secondcluster$second_cluster.cluster.o.==3]))
list_id4 <- (with(df_secondcluster,df_secondcluster$tivo.ID.o.[df_secondcluster$second_cluster.cluster.o.==4]))

##Find characteristics of each cluster
tivo_clus1 <- tivo %>% filter(tivo$ID %in% list_id1) #filter the ids with the Ids in list1
tivo_clus1$ID
mean(tivo_clus1$Annual.Income..x1000...)
mean(tivo_clus1$Monthly.Electronics.Spend)
#location
table(tivo_clus1$Purchasing.Location)
#appealing feature
alltivo7 <- alltivo %>% filter(alltivo$ID %in% list_id1)
table(alltivo1$Purchasing.Location)
table(alltivo1$Favorite.feature)


##Find characteristics of each cluster
tivo_clus2 <- tivo %>% filter(tivo$ID %in% list_id2) #filter the ids with the Ids in list1
mean(tivo_clus2$Annual.Income..x1000...)
mean(tivo_clus2$Monthly.Electronics.Spend)
#location
table(tivo_clus2$Purchasing.Location)
#appealing feature
alltivo7 <- alltivo %>% filter(alltivo$ID %in% list_id2)
table(alltivo2$Purchasing.Location)
table(alltivo2$Favorite.feature)


#Segment 3
tivo_clus3 <- tivo %>% filter(tivo$ID %in% list_id3) #filter the ids with the Ids in list1
tivo_clus3$ID
mean(tivo_clus3$Annual.Income..x1000...)
mean(tivo_clus3$Monthly.Electronics.Spend)
#location
table(tivo_clus3$Purchasing.Location)
#appealing feature
alltivo3 <- alltivo %>% filter(alltivo$ID %in% list_id3)
table(alltivo3$Purchasing.Location)
table(alltivo3$Favorite.feature)


#Segment 4
tivo_clus4 <- tivo %>% filter(tivo$ID %in% list_id4) #filter the ids with the Ids in list1
tivo[tivo$ID == "923",]

mean(tivo_clus4$Annual.Income..x1000...)
mean(tivo_clus4$Monthly.Electronics.Spend)
#location
table(tivo_clus4$Purchasing.Location)
#appealing feature
alltivo4 <- alltivo %>% filter(alltivo$ID %in% list_id4)
table(alltivo4$Purchasing.Location)
table(alltivo4$Favorite.feature)
  
