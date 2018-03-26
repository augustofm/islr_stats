### LAB 1 ----

library(data.table)

## QUESTION 04 ----
set.seed(1)

dt <- data.table(USArrests)
states <- row.names(USArrests)
states
names(dt)
str(dt)
apply(dt, 2, mean)
apply(dt, 2, var)

# Since means and variances are very high 
# and different among the attributes, scaling
# has to be used

pr.out <- prcomp(USArrests, scale = TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)

# we can plot the first two principal components 
# as follows
biplot(pr.out, scale = 0)
dt[abs(x1) < 2 & abs(x2) <10 , y := 2]
dt[is.na(y), y := 1]

# since pc are only unique up to a sign change
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale = 0)

pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var

# percentage of variance explained
pve <- pr.var/sum(pr.var)
pve

# ploting that percentage and the cummulative
# percentage

par(mfrow = c(1,2))
plot(pve,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
plot(cumsum(pve),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')


### LAB 2 ----

require(data.table)
par(mfrow = c(1,1))

# 10.5.1 K-Means Clustering ----

set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1]+3
x[1:25, 2] <- x[1:25, 2]-4
plot(x)

km.out <- kmeans(x, 2, nstart = 20)
km.out$cluster
plot(x, 
     col = (km.out$cluster+1), 
     main = "K-means Clustering Results with K = 2",
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 2)
points(km.out$centers, col = "black")

km.out <- kmeans(x, 3, nstart = 20)
km.out$cluster
plot(x, 
     col = (km.out$cluster+1), 
     main = "K-means Clustering Results with K = 2",
     xlab = "",
     ylab = "",
     pch = 20,
     cex = 2)

set.seed(3)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss
# higher nstart avoids local minimun 

# 10.5.2 Hierarchical Clustering ----

hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

par(mfrow = c(1,3))
plot(hc.complete, main = "Complete Linkage",
xlab = "", sub = "", cex = 0.9)
plot(hc.average, main = "Average Linkage",
xlab = "", sub = "", cex = 0.9)
plot(hc.single, main = "Single Linkage",
xlab = "", sub = "", cex = 0.9)
