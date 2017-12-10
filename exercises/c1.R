## Exercise 8

require(data.table)

college <- data.table(fread("College.csv"))
fix(college)

rownames(college)=college[,1]
fix(college)
summary(college)
pairs(college[,3:12])
plot(as.factor(college$Private), college$Outstate, col="red")
plot(as.factor(college$Private), college$Elite, col="red")

college[, Elite:=as.factor(ifelse(Top10perc>50, "Yes", "No"))]
summary(college[,Elite])

hist(college$Outstate, breaks=5)
hist(college$Outstate, col=2, breaks=10)
par(mfrow=c(2,2))

require(ggplot2)
ggplot(college)+
  geom_line(aes(x=perc.alumni, y=Expend))+
  facet_wrap(~Elite)
ggplot(college)+
  geom_line(aes(x=Grad.Rate, y=perc.alumni))+
  facet_wrap(~Private)
