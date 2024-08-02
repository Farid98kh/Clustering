library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(caret)
library(psych)
library(pastecs)
library(mice)
library("corrplot")
library(factoextra)
library(ggrepel)
library(clustertend)
library(fpc)
library(tidyverse)
library(cluster)
library(readr)
library(lattice)

## **DENETİMSİZ İSTATİSTİKSEL ÖĞRENME 1.ÖDEV**

### **1.Veri Seti Bilgisi**

df <- read.csv("G:/Yüksek Lisans/Denetimsiz İstatistiksel Öğrenme/1.odev/travel/tripadvisor_review.csv", header = TRUE, sep=",")
head(df)
View(df)
colnames(df)=c("User ID","art_galleries","dance_clubs","juice_bars","restaurants","museums","resorts","parks_picnic_spots","beaches","theaters","religious_institutions")
#1.sütun çıkarıldı
df <- df[,-1]

dim(df$beaches)
range(df$religious_institutions)
head(df)



## Tanımlayıcı İstatistikler


summary(df)

apply(df,2,sd)

### **Kutu Grafikleri**

boxplot(df$art_galleries , horizantal = T, xlab= "art_galleries")
boxplot(df$dance_clubs, horizantal = T,xlab ="dance_clubs")
boxplot(df$juice_bars, horizantal = T,xlab= "juice_bars")
boxplot(df$restaurants, horizantal = T,xlab= "restaurants")
boxplot(df$museums, horizantal = T, xlab="museums")
boxplot(df$resorts, horizantal = T,xlab = "resorts")
boxplot(df$parks_picnic_spots, horizantal = T, xlab="parks_picnic_spots")
boxplot(df$beaches, horizantal = T,xlab="beaches")
boxplot(df$theaters, horizantal = T,xlab="theaters")
boxplot(df$religious_institutions, horizantal = T, xlab="religious_institutions")


attach(df)
# Aykırı değerleri içermeyen bir veri seti oluşturma

for (x in c('art_galleries','dance_clubs','restaurants','resorts','theaters')) {
  value = df[,x][df[,x] %in% boxplot.stats(df[,x])$out]
  df[,x][df[,x] %in% value] = NA
} 
md.pattern(df)
dim(df)
df <- na.omit(df)

#Aykırı değer kontolü sağlanarak verilerde düzenleme yapılmıştır.***
  
boxplot(df, main= "Degiskenlikler",
        ylab="Frekans",  
        col = c("orchid2","orangered", 
                         "red","blue","lightgray","lightskyblue","lightpink",
                         "lightsalmon","lightgoldenrod","gold"))
                         


### **Değişkenler Arasındaki Korelasyon**

corr <- cor((df), method = "pearson")
corr

corrplot.mixed(corr, lower="pie",upper="number")  




### ***Hopkins Testi***
set.seed(123)
hopkins.data <- hopkins(df,n=nrow(df)-1)
hopkins.data


dfScaled <- preProcess(df, method=c("center", "scale"))
dfScaled <- predict(dfScaled, newdata = df)


boxplot(dfScaled)


## Temel Bileşenler Analizi


dfScaled.pca <- prcomp(dfScaled, center = TRUE, scale. = TRUE)  
summary(dfScaled.pca)


(dfScaled.pca$sdev)^2

#Çıktı incelendiğinde, 4 temel bileşenin özvektörler değerlerinin 1'in üzerinde olduğu görülmektedir. 

fviz_eig(dfScaled.pca)

x <- fa.parallel(dfScaled, fm="pa", fa="both", n.iter=1)


dfScaled.pca$rotation[,1:4] 


df_new <- dfScaled.pca$x[,1:4] #dört bileşene ait yeni veri seti 
df_new
df_new2 <- dfScaled.pca$x[,1:2] #dört bileşene ait yeni veri seti 


fviz_contrib(dfScaled.pca, choice = "var", axes = 1,top = 10)
fviz_contrib(dfScaled.pca, choice = "var", axes = 2,top = 10)
fviz_contrib(dfScaled.pca, choice = "var", axes = 3,top = 10)
fviz_contrib(dfScaled.pca, choice = "var", axes = 4,top = 10)


fviz_pca_ind(dfScaled.pca, axes = c(1,2),
             col.ind = "cos2",gradient.cols = c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE )
 
#Gözlemlerin PC1 ve PC2 grafiğindeki katkıları incelendiğinde çok yoğun olmasa da sol ve sağ olarak bir kümelenme görülmekte. 

aggregate(df, by=list(cluster=km_res$cluster), mean)

df[c(303,2,351,181),]
summary(df)


PC1-PC2

fviz_pca_var(dfScaled.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
cor(dfScaled.pca$x[,1],dfScaled.pca$x[,2])


PC1-PC3

fviz_pca_var(dfScaled.pca, axes = c(1, 3), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


PC1-PC4

fviz_pca_var(dfScaled.pca, axes = c(1, 4), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)



PC2-PC3
fviz_pca_var(dfScaled.pca, axes = c(2, 3), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


PC2-PC4
fviz_pca_var(dfScaled.pca, axes = c(2, 4), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


PC3-PC4
fviz_pca_var(dfScaled.pca, axes = c(3, 4), col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


## **Uzaklık/Benzerlik Ölçüleri**

dist.euc=get_dist(df_new, method="euclidean")
fviz_dist(dist.euc)



dist.cor=get_dist(df, method="pearson")
fviz_dist(dist.cor)


## **Kümeleme Analizi**

### **K-Means**

#  ***Dirsek Yöntemi(Elbow)***
  
set.seed(123)
fviz_nbclust(df_new,kmeans,method = "wss",nstart = 25)


#***Silhouette Yöntemi***
  
 
set.seed(123)
fviz_nbclust(df_new,kmeans,method = "silhouette") #for average silhouette width



#***Gap İstatistiği Yöntemi***
  
  
set.seed(123)
fviz_nbclust(df_new,kmeans,method = "gap_stat", nboot=50)


set.seed(123)
km_res <- kmeans(df_new, 2, nstart=25) 
#print(km_res)


fviz_cluster(km_res, data = df_new,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


set.seed(123)
km_res <- kmeans(df_new, 2, nstart=25) 
#print(km_res)


fviz_cluster(km_res, data = df_new, axes = c(1,4),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)







set.seed(123)
km_res <- kmeans(df_new, 2, nstart=25) 
#print(km_res)


fviz_cluster(km_res, data = df_new, axes = c(2,3),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)






set.seed(123)
km_res <- kmeans(df_new, 3, nstart=25) 
#print(km_res)


fviz_cluster(km_res, data = df_new,axes = c(1,4),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



set.seed(123)
km_res <- kmeans(df_new, 3, nstart=25) 
#print(km_res)


fviz_cluster(km_res, data = df_new,axes = c(3,4),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)




set.seed(123)
km_res <- kmeans(df_new, 5, nstart=25) 
#print(km_res)


fviz_cluster(km_res, data = df_new,axes = c(1,2),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


#**K-medoids**
  
#  **Küme Sayısının Belirlenmesi**
  
set.seed(123)
fviz_nbclust(df_new,pam,method = "wss")
fviz_nbclust(df_new,pam, method = "silhouette")



set.seed(123)
pam_df_2 <- pam(df_new,2)
print(pam_df_2)
fviz_cluster(pam_df_2,
             ellipse.type = "convex"
             , repel = TRUE
             , axes = c(1,2)
             
)


#k-medoids pca
set.seed(123)
pam_df_2 <- pam(df_new,2)
fviz_cluster(pam_df_2,
             ellipse.type = "convex"
             , repel = TRUE
             , axes = c(2,3)
             
)





set.seed(123)
pam_df_2 <- pam(df_new,3)
#print(pam_df_2)
fviz_cluster(pam_df_2,
             ellipse.type = "convex"
             , repel = TRUE
             , axes = c(1,2)
             
)



set.seed(123)
pam_df_2 <- pam(df_new,6)
fviz_cluster(pam_df_2,
             ellipse.type = "t"
             , repel = TRUE
             , axes = c(1,2)
            
)


## **Clara**

#**Küme Sayısının Belirlenmesi**

set.seed(123)
fviz_nbclust(df_new,clara,method = "wss")
fviz_nbclust(df_new,clara, method = "silhouette")




set.seed(123)
df_clara <- clara(df_new,2, samples = 50, pamLike = TRUE)
print(df_clara)

fviz_cluster(df_clara,
             ellipse.type = "convex",geom = "point"
             , pointsize = 1,
             ggtheme = theme_classic()
             
        ,axes = c(1,2)     
)





set.seed(123)
df_clara <- clara(df_new,5, samples = 50, pamLike = TRUE)
#print(df_clara)

fviz_cluster(df_clara,
             ellipse.type = "t",geom = "point"
             , pointsize = 1,
             ggtheme = theme_classic()
             
        ,axes = c(1,2)     
)




set.seed(123)
df_clara <- clara(df_new,3, samples = 50, pamLike = TRUE)
#print(df_clara)

fviz_cluster(df_clara,
             ellipse.type = "t",geom = "point"
             , pointsize = 1,
             ggtheme = theme_classic()
             
        ,axes = c(2,3)     
)


## **SONUÇ**

#Veri seti için en uygun kümeleme yönteminin k-means ve küme sayısının 2 olduğuna karar verilmiştir.

set.seed(123)
km_res <- kmeans(df_new, 2, nstart=25) 
print(km_res)





fviz_cluster(km_res, data = df_new,
             ellipse.type = "convex", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)



## **Kümeleme Sonuçları**
```{r}
aggregate(df, by=list(cluster=km_res$cluster), mean)


summary(df)


#Her değişkenin ortalamaları küme ortalamaları ile karşılaştırılmıştır.



# ##################################
# ### Density-Based Clustering
# ##################################
# Yoğunluk bazlı kümelemede küme sayısının önceden belirlenmesi gerekmemektedir;
# ancak MinPts ve eps değerlerinin önceden belirlenmesi gerekmektedir. eps parametresi,
# x noktasının çevresindeki komşuların yarıçapını tanımlar. Buna x'in epsilon komşuluğu denir.
# MinPts parametresi, "eps" yarıçapı içindeki minimum komşu sayısıdır. Bu değerlere karar
# vermek için KNN distplot kullanılabilir.
# 

library(factoextra)
library(fpc)
library(dbscan)

view(df_new)
summary(df_new)
head(df_new)

dim(df_new)
par(mfrow=c(1,1))
plot(df_new, col=c("red","blue","green","black","purple","pink","red4","green4","blue4","gray"))
set.seed(123)

# DBSCAN hesaplama fpc paketiyle
# dbscan() için uygun bir eps değeri belirlemek üzere bir k-NN grafiği kullanmak için minPts, dbscan'de kullanılan minPts belirtilebilir ve k = minPts - 1 olarak ayarlanır.
# k, MinPts'i ifade etmektedir. k=minPts - 1
# Yapılan çeşitli denemeler sonrasında 10'a karar
# verilmiştir. kNNdisplot incelenirken, tıpkı Dirsek Yöntemi gibi,
# çizginin "dirsek" yaptığı nokta saptanılmalıdır.
# Bu nokta eps değeri olarak seçilmelidir.
set.seed(123)
kNNdistplot(df_new, k = 10)
abline(h = 1.5, lty = 2)

db <- fpc::dbscan(df_new, eps = 1.5, MinPts = 10)
print(db)
# Plot DBSCAN sonucu
fviz_cluster(db, data = df_new, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

# Çeşitli denemeler sonucunda en uygun değerin 1.5 
# olduğuna karar verilmiştir. Fakat uygulanan kümeleme sonuçları 
# beğenilmediği ve küme sayısı 1 olduğu için en iyi sonuç veren 
# eps değeri olarak kabul edilmemiştir.



db <- fpc::dbscan(df_new, eps = 1, MinPts = 10)
print(db)
# Plot DBSCAN sonucu
fviz_cluster(db, data = df_new, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

en uygun k değeri 10 olmakla birlikte bütün eps değerlerini aldıktan sonra
analizlerde küme sayısı bir olarak veya sadece gürültü çıkmıştır. Bir küme, hiç küme anlamına geldiği için kabul edilmemektedir.





db <- fpc::dbscan(df_new, eps = 0.6, MinPts = 8)
print(db)
# Plot DBSCAN sonucu
fviz_cluster(db, data = df_new, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
# çok karmaşık bir sonuç vermesi nedeniyle kabul edilmemektedir
# k=8 için eps değeri 1.2 olması gerektiğine karşı 0.6 ve herhangi kombinasyon denenmiştir
# k=8 e eps=1.2 da da aynı bir önceki gibi tek kümeden oluşmakta.
db <- fpc::dbscan(df_new, eps = 0.6, MinPts = 5)
print(db)
# Plot DBSCAN sonucu
fviz_cluster(db, data = df_new, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
# çok karmaşık bir görsel verdiği ve 18 kümeden oluşması nedeniyle ve aşırı derecede
# gürültü barındırdığı için kabul edilmeyen bir çok denemelere benzer burada da eps değeri 0.6 ve MinPts 5 olarak
# kabul edilememektedir

db <- fpc::dbscan(df_new, eps = 0.5, MinPts = 9)
print(db)
# Plot DBSCAN sonucu
fviz_cluster(db, data = df_new, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

# 0.5 eps değeriyle bütün deneyler sonucunda ya bütün veri gürültü olarak saptanmıştır ve ya çok fazla gürültüyle arada büyük bir varyasyon ile küçük kümeler oluşmuştur ve kabul edilmemektedir

# bütün analizler ve deneyler sonucunda yoğunluk bazlı kümelemeye hiç uygun bir veri olmadığına karar verilmiştir
# buna rağmen önemli ölçüde bilgi kaybı yaşayarak 4 dim den oluşan verinin boyutunu
# 2 ye indirgeyerek incelemek istenilmiştir

set.seed(123)
kNNdistplot(df_new2, k = 10)
abline(h = 0.2, lty = 2)

db <- fpc::dbscan(df_new2, eps = 1, MinPts = 10)
print(db)
# Plot DBSCAN sonucu
fviz_cluster(db, data = df_new2, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())


db <- fpc::dbscan(df_new2, eps = 0.3, MinPts = 10)
print(db)
# Plot DBSCAN sonucu
fviz_cluster(db, data = df_new2, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

view(df_new2)

# MinPts 10 alarak irsek noktasını yani eps değerini çok az belirleyerek eps=0.2 ve 0.3 daha iyi sonuçlar elde ediliyor
# bu eps lerin dışında k 10 lu kümeleme analizlerinde tek küme veya hiç küme sonucunu aldık

eps=0.2

eps=0.3