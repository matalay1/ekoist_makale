# ÇOK BOYUTLU ÖLÇEKLEME / MULTIDIMENSIONAL SCALLING

library(readxl)
isyerisayisidegisim <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx", sheet = "Sayfa1")
isyerisayisidegisim <- edit(isyerisayisidegisim)
isyerisayisidegisim <-data.frame(isyerisayisidegisim[,-1], row.names = isyerisayisidegisim[,1])

# Classical MDS

# Load required packages
library(magrittr)
library(dplyr)
library(ggpubr)

# 1-Compute MDS

mds1 <- isyerisayisidegisim %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble() 
colnames(mds1) <- c("Boyut 1", "Boyut 2")


# Plot MDS
ggscatter(mds1, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(isyerisayisidegisim),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust1 <- kmeans(mds1, 3)$cluster %>%
  as.factor()
mds1 <- mds1 %>%
  mutate(groups = clust1)
# Plot and color by groups
ggscatter(mds1, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(isyerisayisidegisim),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

library(MASS)

mds1.scal <- cmdscale(dist(isyerisayisidegisim),k=2,eig=T)
eqscplot(mds1.scal$points)
mds1.scal$GOF
summary(mds1.scal)
NewCoords1 = mds1.scal$points
NewDists1 <- dist(NewCoords1, diag=TRUE, upper=TRUE)
r1 <- cor(c(dist(isyerisayisidegisim)), c(NewDists1))
r1

rsquared1=r1^2
rsquared1

freedom1 = NROW(c(NewDists1)) - 2
Fvalue1 <- rsquared1 /((1 - rsquared1)/freedom1)
pf(Fvalue1, 1, freedom1, lower.tail=FALSE)
freedom1
Fvalue1

########
# A3-stress deðeri

isyerisayisidegisim_dist = dist(x = isyerisayisidegisim) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
isyerisayisidegisim_mds=smacofSym(isyerisayisidegisim_dist,ndim = 2)
s1 <- isyerisayisidegisim_mds$stress
s1

###############################################
sigortalisayisidegisim <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa2")
sigortalisayisidegisim <- edit(sigortalisayisidegisim)
sigortalisayisidegisim <-data.frame(sigortalisayisidegisim[,-1], row.names = sigortalisayisidegisim[,1])

# 2-Compute MDS
mds2 <- sigortalisayisidegisim %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds2) <- c("Boyut 1", "Boyut 2")

mds2.scal <- cmdscale(dist(sigortalisayisidegisim),k=2,eig=T)
mds2.scal$GOF

NewCoords2 = mds2.scal$points
NewDists2 <- dist(NewCoords2, diag=TRUE, upper=TRUE)
r2 <- cor(c(dist(sigortalisayisidegisim)), c(NewDists2))
r2
rsquared2=r2^2

freedom2 = NROW(c(NewDists2)) - 2
Fvalue2 <- rsquared2 /((1 - rsquared2)/freedom2)
pf(Fvalue2, 1, freedom2, lower.tail=FALSE)

# Plot MDS
ggscatter(mds2, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(sigortalisayisidegisim),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust2 <- kmeans(mds2, 3)$cluster %>%
  as.factor()
mds2 <- mds2 %>%
  mutate(groups = clust2)
# Plot and color by groups
ggscatter(mds2, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(sigortalisayisidegisim),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)



########
# A3-stress deðeri

sigortalisayisidegisim_dist = dist(x = sigortalisayisidegisim) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
sigortalisayisidegisim_mds=smacofSym(sigortalisayisidegisim_dist,ndim = 2)
s2 <- sigortalisayisidegisim_mds$stress

#############################################################
isyerisayisidegisim2 <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx", sheet = "Sayfa3")
isyerisayisidegisim2 <- edit(isyerisayisidegisim2)
isyerisayisidegisim2 <-data.frame(isyerisayisidegisim2[,-1], row.names = isyerisayisidegisim2[,1])

# 3-Compute MDS
mds3 <- isyerisayisidegisim2 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds3) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds3, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(isyerisayisidegisim2),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust3 <- kmeans(mds3, 3)$cluster %>%
  as.factor()
mds3 <- mds3 %>%
  mutate(groups = clust3)
# Plot and color by groups
ggscatter(mds3, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(isyerisayisidegisim2),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

library(MASS)

mds3.scal <- cmdscale(dist(isyerisayisidegisim2),k=2,eig=T)
mds3.scal$GOF

NewCoords3 = mds3.scal$points
NewDists3 <- dist(NewCoords3, diag=TRUE, upper=TRUE)
r3 <- cor(c(dist(isyerisayisidegisim2)), c(NewDists3))
r3

rsquared3=r3^2
rsquared3

freedom3 = NROW(c(NewDists3)) - 2
Fvalue3 <- rsquared3 /((1 - rsquared3)/freedom3)
pf(Fvalue3, 1, freedom3, lower.tail=FALSE)

########
# A3-stress deðeri

isyerisayisidegisim2_dist = dist(x = isyerisayisidegisim2) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
isyerisayisidegisim2_mds=smacofSym(isyerisayisidegisim2_dist,ndim = 2)
s3 <- isyerisayisidegisim2_mds$stress

#######################################################
sigortalisayisidegisim2 <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa4")
sigortalisayisidegisim2 <- edit(sigortalisayisidegisim2)
sigortalisayisidegisim2 <-data.frame(sigortalisayisidegisim2[,-1], row.names = sigortalisayisidegisim2[,1])

# 4-Compute MDS
mds4 <- sigortalisayisidegisim2 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds4) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds4, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(sigortalisayisidegisim2),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust4 <- kmeans(mds4, 3)$cluster %>%
  as.factor()
mds4 <- mds4 %>%
  mutate(groups = clust4)
# Plot and color by groups
ggscatter(mds4, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(sigortalisayisidegisim2),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

mds4.scal <- cmdscale(dist(sigortalisayisidegisim2),k=2,eig=T)
mds4.scal$GOF

NewCoords4 = mds4.scal$points
NewDists4 <- dist(NewCoords4, diag=TRUE, upper=TRUE)
r4 <- cor(c(dist(sigortalisayisidegisim2)), c(NewDists4))

rsquared4=r4^2
rsquared4

freedom4 = NROW(c(NewDists4)) - 2
Fvalue4 <- rsquared4 /((1 - rsquared4)/freedom4)
pf(Fvalue4, 1, freedom4, lower.tail=FALSE)

########
# A3-stress deðeri

sigortalisayisidegisim2_dist = dist(x = sigortalisayisidegisim2) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
sigortalisayisidegisim2_mds=smacofSym(sigortalisayisidegisim2_dist,ndim = 2)
s4 <- sigortalisayisidegisim2_mds$stress

#########################################################
oran1 <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa5")
oran1 <- edit(oran1)
oran1 <-data.frame(oran1[,-1], row.names = oran1[,1])

# 5-Compute MDS
mds5 <- oran1 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds5) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds5, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(oran1),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust5 <- kmeans(mds5, 3)$cluster %>%
  as.factor()
mds5 <- mds5 %>%
  mutate(groups = clust5)
# Plot and color by groups
ggscatter(mds5, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(oran1),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

################
mds5.scal <- cmdscale(dist(oran1),k=2,eig=T)
mds5.scal$GOF

NewCoords5 = mds5.scal$points
NewDists5 <- dist(NewCoords5, diag=TRUE, upper=TRUE)
r5 <- cor(c(dist(oran1)), c(NewDists5))
r5

rsquared5=r5^2
rsquared5

freedom5 = NROW(c(NewDists5)) - 2
Fvalue5 <- rsquared5 /((1 - rsquared5)/freedom5)
pf(Fvalue5, 1, freedom5, lower.tail=FALSE)

########
# A3-stress deðeri

oran1_dist = dist(x = oran1) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
oran1_mds=smacofSym(oran1_dist,ndim = 2)
s5 <- oran1_mds$stress

#########################################################
oran2 <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa6")
oran2 <- edit(oran2)
oran2 <-data.frame(oran2[,-1], row.names = oran2[,1])

# Compute MDS
mds6 <- oran2 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds6) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds6, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(oran2),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust6 <- kmeans(mds6, 3)$cluster %>%
  as.factor()
mds6 <- mds6 %>%
  mutate(groups = clust6)
# Plot and color by groups
ggscatter(mds6, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(oran2),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

################
mds6.scal <- cmdscale(dist(oran2),k=2,eig=T)
mds6.scal$GOF

NewCoords6 = mds6.scal$points
NewDists6 <- dist(NewCoords6, diag=TRUE, upper=TRUE)
r6 <- cor(c(dist(oran2)), c(NewDists6))
r6

rsquared6=r6^2
rsquared6

freedom6 = NROW(c(NewDists6)) - 2
Fvalue6 <- rsquared6 /((1 - rsquared6)/freedom6)
pf(Fvalue6, 1, freedom6, lower.tail=FALSE)

########
# A3-stress deðeri

oran2_dist = dist(x = oran2) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
oran2_mds=smacofSym(oran2_dist,ndim = 2)
s6 <- oran2_mds$stress

#########################################################
isyerioran <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa7")
isyerioran <- edit(isyerioran)
isyerioran <-data.frame(isyerioran[,-1], row.names = isyerioran[,1])

# Compute MDS
mds7 <- isyerioran %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds7) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds7, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(isyerioran),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust7 <- kmeans(mds7, 3)$cluster %>%
  as.factor()
mds7 <- mds7 %>%
  mutate(groups = clust7)
# Plot and color by groups
ggscatter(mds7, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(isyerioran),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

################
mds7.scal <- cmdscale(dist(isyerioran),k=2,eig=T)
mds7.scal$GOF

NewCoords7 = mds7.scal$points
NewDists7 <- dist(NewCoords7, diag=TRUE, upper=TRUE)
r7 <- cor(c(dist(isyerioran)), c(NewDists7))
r7

rsquared7=r7^2
rsquared7

freedom7 = NROW(c(NewDists7)) - 2
Fvalue7 <- rsquared7 /((1 - rsquared7)/freedom7)
pf(Fvalue7, 1, freedom7, lower.tail=FALSE)

########
# A3-stress deðeri

isyerioran_dist = dist(x = isyerioran) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
isyerioran_mds=smacofSym(isyerioran_dist,ndim = 2)
s7 <- isyerioran_mds$stress

#########################################################
sigortalioran <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa8")
sigortalioran <- edit(sigortalioran)
sigortalioran <-data.frame(sigortalioran[,-1], row.names = sigortalioran[,1])

# Compute MDS
mds8 <- sigortalioran %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds8) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds8, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(sigortalioran),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust8 <- kmeans(mds8, 3)$cluster %>%
  as.factor()
mds8 <- mds8 %>%
  mutate(groups = clust8)
# Plot and color by groups
ggscatter(mds8, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(sigortalioran),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

################
mds8.scal <- cmdscale(dist(sigortalioran),k=2,eig=T)
mds8.scal$GOF

NewCoords8 = mds8.scal$points
NewDists8 <- dist(NewCoords8, diag=TRUE, upper=TRUE)
r8 <- cor(c(dist(sigortalioran)), c(NewDists8))
r8

rsquared8=r8^2
rsquared8

freedom8 = NROW(c(NewDists8)) - 2
Fvalue8 <- rsquared8 /((1 - rsquared8)/freedom8)
pf(Fvalue8, 1, freedom8, lower.tail=FALSE)

########
# A3-stress deðeri

sigortalioran_dist = dist(x = sigortalioran) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
sigortalioran_mds=smacofSym(sigortalioran_dist,ndim = 2)
s8 <- sigortalioran_mds$stress

#########################################################
oran3 <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa9")
oran3 <- edit(oran3)
oran3 <-data.frame(oran3[,-1], row.names = oran3[,1])

# Compute MDS
mds9 <- oran3 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds9) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds9, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(oran3),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust9 <- kmeans(mds9, 3)$cluster %>%
  as.factor()
mds9 <- mds9 %>%
  mutate(groups = clust9)
# Plot and color by groups
ggscatter(mds9, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(oran3),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

################
mds9.scal <- cmdscale(dist(oran3),k=2,eig=T)
mds9.scal$GOF

NewCoords9 = mds9.scal$points
NewDists9 <- dist(NewCoords9, diag=TRUE, upper=TRUE)
r9 <- cor(c(dist(oran3)), c(NewDists9))
r9

rsquared9=r9^2
rsquared9

freedom9 = NROW(c(NewDists9)) - 2
Fvalue9 <- rsquared9 /((1 - rsquared9)/freedom9)
pf(Fvalue9, 1, freedom9, lower.tail=FALSE)

########
# A3-stress deðeri

oran3_dist = dist(x = oran3) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
oran3_mds=smacofSym(oran3_dist,ndim = 2)
s9 <- oran3_mds$stress

#########################################################
oran4 <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa10")
oran4 <- edit(oran4)
oran4 <-data.frame(oran4[,-1], row.names = oran4[,1])

# Compute MDS
mds10 <- oran4 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds10) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds10, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(oran4),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust10 <- kmeans(mds10, 3)$cluster %>%
  as.factor()
mds10 <- mds10 %>%
  mutate(groups = clust10)
# Plot and color by groups
ggscatter(mds10, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(oran4),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

################
mds10.scal <- cmdscale(dist(oran4),k=2,eig=T)
mds10.scal$GOF

NewCoords10 = mds10.scal$points
NewDists10 <- dist(NewCoords10, diag=TRUE, upper=TRUE)
r10 <- cor(c(dist(oran4)), c(NewDists10))
r10

rsquared10=r10^2
rsquared10

freedom10 = NROW(c(NewDists10)) - 2
Fvalue10 <- rsquared10 /((1 - rsquared10)/freedom10)
pf(Fvalue10, 1, freedom10, lower.tail=FALSE)

########
# A3-stress deðeri

oran4_dist = dist(x = oran4) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
oran4_mds=smacofSym(oran4_dist,ndim = 2)
s10 <- oran4_mds$stress

#########################################################
isyerioran2 <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa11")
isyerioran2 <- edit(isyerioran2)
isyerioran2 <-data.frame(isyerioran2[,-1], row.names = isyerioran2[,1])

# Compute MDS
mds11 <- isyerioran2 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds11) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds11, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(isyerioran2),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust11 <- kmeans(mds11, 3)$cluster %>%
  as.factor()
mds11 <- mds11 %>%
  mutate(groups = clust11)
# Plot and color by groups
ggscatter(mds11, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(isyerioran2),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

################
mds11.scal <- cmdscale(dist(isyerioran2),k=2,eig=T)
mds11.scal$GOF

NewCoords11 = mds11.scal$points
NewDists11 <- dist(NewCoords11, diag=TRUE, upper=TRUE)
r11 <- cor(c(dist(isyerioran2)), c(NewDists11))
r11

rsquared11=r11^2
rsquared11

freedom11 = NROW(c(NewDists11)) - 2
Fvalue11 <- rsquared11 /((1 - rsquared11)/freedom11)
pf(Fvalue11, 1, freedom11, lower.tail=FALSE)

########
# A3-stress deðeri

isyerioran2_dist = dist(x = isyerioran2) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
isyerioran2_mds=smacofSym(isyerioran2_dist,ndim = 2)
s11 <- isyerioran2_mds$stress

#########################################################
sigortalioran2 <- read_excel("M:/2-çalýþmalar/Kitap2.xlsx",sheet = "Sayfa12")
sigortalioran2 <- edit(sigortalioran2)
sigortalioran2 <-data.frame(sigortalioran2[,-1], row.names = sigortalioran2[,1])

# Compute MDS
mds12 <- sigortalioran2 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds12) <- c("Boyut 1", "Boyut 2")
# Plot MDS
ggscatter(mds12, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(sigortalioran2),
          size = 1,
          repel = TRUE)

# Create 3 groups using k-means clustering. Color points by groups
# K-means clustering
clust12 <- kmeans(mds12, 3)$cluster %>%
  as.factor()
mds12 <- mds12 %>%
  mutate(groups = clust12)
# Plot and color by groups
ggscatter(mds12, x = "Boyut 1", y = "Boyut 2", 
          label = rownames(sigortalioran2),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)
################
mds12.scal <- cmdscale(dist(sigortalioran2),k=2,eig=T)
mds12.scal$GOF

NewCoords12 = mds12.scal$points
NewDists12 <- dist(NewCoords12, diag=TRUE, upper=TRUE)
r12 <- cor(c(dist(sigortalioran2)), c(NewDists12))
r12

rsquared12=r12^2
rsquared12

freedom12 = NROW(c(NewDists12)) - 2
Fvalue12 <- rsquared12 /((1 - rsquared12)/freedom12)
pf(Fvalue12, 1, freedom12, lower.tail=FALSE)

########
# A3-stress deðeri

sigortalioran2_dist = dist(x = sigortalioran2) #veriler ayný ölçekteyse uzaklýklar doðrudan hesaplanýr

library(smacof)
sigortalioran2_mds=smacofSym(sigortalioran2_dist,ndim = 2)
s12 <- sigortalioran2_mds$stress

#########################################################

mds1$groups
mds2$groups
mds3$groups
mds4$groups
mds5$groups
mds6$groups
mds7$groups
mds8$groups
mds9$groups
mds10$groups
mds11$groups
mds12$groups

#############
mds1.scal$GOF
mds2.scal$GOF
mds3.scal$GOF
mds4.scal$GOF
mds5.scal$GOF
mds6.scal$GOF
mds7.scal$GOF
mds8.scal$GOF
mds9.scal$GOF
mds10.scal$GOF
mds11.scal$GOF
mds12.scal$GOF

##############

rsquared1
rsquared2
rsquared3
rsquared4
rsquared5
rsquared6
rsquared7
rsquared8
rsquared9
rsquared10
rsquared11
rsquared12

################

s1
s2
s3
s4
s5
s6
s7
s8
s9
s10
s11
s12

##############
summary(oran1)
prod(oran1$S)^(1/81)

summary(oran2)
prod(oran2$S)^(1/81)

summary(isyerioran)
prod(isyerioran$Adana)^(1/18)

#############  F I N I S H  #########################################

citation("dplyr")
citation("readxl")
citation("magrittr")
citation("ggpubr")
citation("stats")
citation("smacof")
