setwd("~/Desktop/2/BD/P2")

obesity<-read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
obesity[1:10,1:10]

str(obesity)

heights<-obesity[,3]
heights

obesity2<-obesity

obesity2$Gender<-as.numeric(factor(obesity2$Gender))
obesity2$Gender
obesity2$family_history_with_overweight<-as.numeric(factor(obesity2$family_history_with_overweight))
obesity2$family_history_with_overweight
obesity2$FAVC<-as.numeric(factor(obesity2$FAVC))
obesity2$FAVC
obesity2$CAEC<-as.numeric(factor(obesity2$CAEC))
obesity2$CAEC
obesity2$SMOKE<-as.numeric(factor(obesity2$SMOKE))
obesity2$SMOKE
obesity2$SCC<-as.numeric(factor(obesity2$SCC))
obesity2$SCC
obesity2$CALC<-as.numeric(factor(obesity2$CALC))
obesity2$CALC
obesity2$MTRANS<-as.numeric(factor(obesity2$MTRANS))
obesity2$MTRANS
obesity2$NObeyesdad<-as.numeric(factor(obesity2$NObeyesdad))
obesity2$NObeyesdad

summary(obesity2)
str(obesity2)

install.packages("psych")
install.packages("sna")
library(sna)
library(psych)

describe(obesity2)

plot(obesity2)

normalize<-function(x){((x-min(x))/(max(x)-min(x)))}
normalize

obesity2.norm<-as.data.frame(lapply(obesity2,normalize))
obesity2.norm[1:10,]

obesity2.min.height<-min(obesity2$Height)
obesity2.min.height
obesity2.min.age<-min(obesity2$Age)
obesity2.min.age

obesity2.max.weight<-max(obesity2$Weight)
obesity2.max.weight
obesity2.max.height<-max(obesity2$Height)
obesity2.max.height

zscore<-function(x){(x-mean(x))/sd(x)}
zscore(c(110,120,130,140,150))

obesity2.znorm<-as.data.frame(lapply(obesity2,scale))
obesity2.znorm[1:10,]

install.packages("corrplot")
library(corrplot)

obesity2cor<-cor(obesity2)
obesity2cor
corrplot(obesity2cor)

install.packages("plotly")
library(plotly)

plot_ly(obesity2,x = ~Height, y = ~Weight, z = ~Age, type = "scatter3d")
plot_ly(obesity2,x = ~Height, y = ~Weight, z = ~Age, type = "scatter3d",color = ~NObeyesdad)

cor_melt <- as.data.frame(as.table(obesity2cor))
cor_melt <- cor_melt[cor_melt$Var1 != cor_melt$Var2, ]
cor_melt <- cor_melt[order(abs(cor_melt$Freq), decreasing = TRUE), ]
top_correlations <- head(unique(cor_melt), 30)
print(top_correlations)

install.packages("factoextra")
library(factoextra)

obesity2.k5<-kmeans(obesity2.norm,centers = 5)
str(obesity2.k5)
obesity2.k5
factoextra::fviz_cluster(obesity2.k5,obesity2.norm)

obesity2.k5$centers

optk<-function(data,nc=15,seed=1234)
{
    opt<-(nrow(data)-1)*sum(apply(data,2,var))
    for(i in 2:nc)
    {
        set.seed(seed)
        opt[i]<-sum(kmeans(data,centers=i)$withinss)
    }
    plot(1:nc,opt,type="b",xlab="Number of clusters",ylab="Within groups sum of squares")
}

optk(obesity2,nc=7,seed=12324)

factoextra::fviz_nbclust(obesity2,FUNcluster = kmeans,method="wss",k.max = 12,verbose = TRUE)

obesity_subset <- obesity2.norm[, c("Height", "Weight", "CAEC", "family_history_with_overweight", "FAF", "FAVC", "Gender", "NObeyesdad")]

obesity_subset.norm.rows=nrow(obesity_subset)
obesity_subset.sample=0.7
obesity_subset.rows=obesity_subset.sample*obesity_subset.norm.rows
obesity_subset.rows

obesity_subset.train.index=sample(obesity_subset.norm.rows,obesity_subset.rows)
length(obesity_subset.train.index)

obesity_subset.train=obesity_subset[obesity_subset.train.index,]
obesity_subset.train[1:10,]

obesity_subset.test=obesity_subset[-obesity_subset.train.index,]
obesity_subset.test[1:10,]

obesity_subset.train.k7=kmeans(obesity_subset.train,centers = 7)
obesity_subset.train.k7

install.packages("class")
library(class)

obesity_subset.test.k7=knn(obesity_subset.train,obesity_subset.test,obesity_subset.train.k7$cluster,k=7)
obesity_subset.test.k7

obesity_subset.test.kmeans.k7=kmeans(obesity_subset.test,centers = 7)
obesity_subset.test.kmeans.k7

obesity_subset.test.k7.labels=obesity_subset.test.kmeans.k7$cluster
length(obesity_subset.test.k7.labels)
obesity_subset.test.k7.labels

obesity_subset.train.glm=glm(formula = obesity_subset.train$NObeyesdad ~ .,family = gaussian, data=obesity_subset.train)
summary(obesity_subset.train.glm)

obesity_subset.train.glm.anova=anova(obesity_subset.train.glm,test="Chisq")
obesity_subset.train.glm.anova

plot(obesity_subset.train.glm)

obesity_subset.test.pred <- predict(obesity_subset.train.glm,newdata = obesity_subset.test)
obesity_subset.test.pred
summary(obesity_subset.test.pred)

confint(obesity_subset.train.glm)

obesity_subset.test.pred.k7=kmeans(obesity_subset.test.pred,centers = 7)
obesity_subset.test.pred.k7

install.packages("gmodels")
library(gmodels)

obesity_subset.test.ct.k7 = CrossTable(obesity_subset.test.pred.k7$cluster,obesity_subset.test.kmeans.k7$cluster,prop.chisq = TRUE)

ct <- CrossTable(obesity_subset.test.pred.k7$cluster, 
                 obesity_subset.test.kmeans.k7$cluster, 
                 prop.chisq = FALSE)

crosstable_matrix <- ct$t

calculate_metrics <- function(crosstable) {

  row_totals <- rowSums(crosstable)
  col_totals <- colSums(crosstable)
  total <- sum(crosstable)

  precision <- diag(crosstable) / row_totals
  recall <- diag(crosstable) / col_totals
  f1_score <- 2 * (precision * recall) / (precision + recall)

  accuracy <- sum(diag(crosstable)) / total

  specificity <- sapply(1:nrow(crosstable), function(i) {
    TN <- sum(crosstable) - (row_totals[i] + col_totals[i] - crosstable[i, i])
    TN / (TN + sum(crosstable[, i]) - crosstable[i, i])
  })

  return(data.frame(Cluster = 1:nrow(crosstable), Precision = precision, Recall = recall, 
                    F1_Score = f1_score, Specificity = specificity, Accuracy = accuracy))
}

metrics <- calculate_metrics(your_crosstable_matrix)
print(metrics)

average_metrics <- colMeans(metrics[, -1], na.rm = TRUE)
average_metrics