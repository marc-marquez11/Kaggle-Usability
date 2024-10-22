###https://www.kaggle.com/datasets/rajugc/kaggle-dataset
###As of 4/20/2024, there are 317,983 datasets in kaggle


library(MASS)
library(car)
library(ggplot2)
library(tree)
library(randomForest)
library(gbm)
library(mvtnorm)
library(BART)

detach(kaggle)
kaggle = read.csv(
  'C:/Users/MMM/Desktop/colleg stuff/Spring 2024/Experimental Design/ed hw/project/kaggle-preprocessed-fixed.csv',
  header = T)
str(kaggle)
attach(kaggle)
kaggle = kaggle[,-(2:3)]
kaggle$CSV = factor(kaggle$CSV)
# kaggle$JSON = factor(kaggle$JSON)
# kaggle$SQLITE = factor(kaggle$SQLITE)
kaggle$Other = factor(kaggle$Other)
kaggle$Medals = factor(kaggle$Medals, levels = 
                         c('No Medal', 'Bronze', 'Silver', 'Gold'))
kaggle$Day = factor(kaggle$Day, levels = 
                      c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
str(kaggle)
kaggle = kaggle[,!names(kaggle) %in% c('id', 'Month', 'Year', 'Date', 'Time')]
detach(kaggle); attach(kaggle)
str(kaggle)

##Summary statistics
par(mfrow=c(2,3))
summary(No_of_files)
boxplot(No_of_files, horizontal = TRUE, xlab = 'Number of files', cex.lab = 2)
hist(No_of_files, xlab = 'Number of files', main = '') #crazy outliers

summary(log(No_of_files)) #unusable
boxplot(log(No_of_files), horizontal = TRUE, xlab = 'log(Number of files)', cex.lab = 2)
hist(log(No_of_files), xlab = 'log(Number of files)', main = '', cex.lab = 2, ylab = "")

length(boxplot.stats(No_of_files)$out)
length(boxplot.stats(log(No_of_files))$out)

# summary(size_conv)
boxplot(size_conv, horizontal = TRUE, xlab = 'File size', 
        main = "Cont. Predictor Boxplots", cex.lab = 2, cex.main = 1.5)
hist(size_conv, xlab) #same problem

# summary(log(size_conv))
boxplot(log(size_conv), horizontal = TRUE, xlab = 'log(File size)', cex.lab = 2)
hist(log(size_conv), xlab = 'log(File size)', cex.lab = 2, main = 'Histograms', 
     cex.main = 1.5, ylab = "") #better
length(boxplot.stats(size_conv)$out)

table(CSV); barplot(table(CSV), xlab = 'Contains CSV', cex.lab = 2)
# table(JSON); barplot(table(JSON))
# table(SQLITE); barplot(table(SQLITE))
table(Other); barplot(table(Other), xlab = 'Contains Other', cex.lab = 2)

boxplot(Upvotes, horizontal = TRUE, xlab = 'Upvotes', cex.lab = 2)
hist(Upvotes) #mfw outliers again
summary(Upvotes)

boxplot(log(Upvotes+1), horizontal = TRUE, xlab = 'log(Upvotes + 1)', cex.lab = 2)
hist(log(Upvotes+1), xlab = 'log(Upvotes + 1)', cex.lab = 2, main = "", ylab = "")
summary(log(Upvotes+1)) #yikes
length(boxplot.stats(log(Upvotes+1))$out)
length(boxplot.stats(Upvotes)$out)
log(Upvotes+1)[summary(log(Upvotes+1))[5]+3*IQR(log(Upvotes+1))]

logu = log(Upvotes+1)
u.iqr = IQR(Upvotes)
logu.iqr = IQR(logu)
length(Upvotes[Upvotes > summary(Upvotes)[5] + 1.5 * u.iqr]) #more than 10% yikes
length(logu[logu > summary(logu)[5] + 3 * logu.iqr]) #44 is better

table(Medals); barplot(table(Medals), xlab = 'Medal', cex.lab = 2)

par(mfrow = c(1,3))
boxplot(Month_since, horizontal = TRUE, xlab = 'Months since 2015', cex.lab = 2)
hist(Month_since, xlab = 'Months since 2015', main = "", cex.lab = 2, ylab = "")
summary(Month_since)
length(boxplot.stats(Month_since)$out)

table(Day); barplot(table(Day), xlab = 'Day of Upload', cex.lab = 2)


boxplot(Usability, horizontal = TRUE, xlab = 'Usability')
hist(Usability, main = "") #pretty skewed, duh
summary(Usability)
length(boxplot.stats(Usability)$out)

tapply(Usability, CSV, mean); tapply(Usability, CSV, sd)
tapply(Usability, Other, mean); tapply(Usability, Other, sd)
tapply(Usability, Medals, mean); tapply(Usability, Medals, sd)
tapply(Usability, Day, mean); tapply(Usability, Day, sd) #holy varied data

kaggle$log.size_conv = log(kaggle$size_conv)

#quick multico. test
cor(kaggle[,c('size_conv', 'Month_since')], 
    method = c("pearson"))


# plot(Usability~log(No_of_files))
par(mfrow = c(2,3))

plot(Usability~log(size_conv), xlab = "Log(File size)", cex.lab = 1.5)

plot(Usability~Month_since, xlab = 'Months since 2015', cex.lab = 1.5,
     main = '1 Factor Models', cex.main = 2)

plot(Usability~CSV, xlab = 'Has CSV', cex.lab = 1.5)
leveneTest(lm(Usability~CSV))[1,3]

plot(Usability~Other, xlab = 'Has Other', cex.lab = 1.5)
leveneTest(lm(Usability~Other))[1,3]

# plot(Usability~log(Upvotes+1))
plot(Usability~Medals, xlab = 'Medal', cex.lab = 1.5)
leveneTest(lm(Usability~Medals))[1,3]

plot(Usability~Day, xlab = 'Day of Upload', cex.lab = 1.5)
leveneTest(lm(Usability~Day))[1,3]


plot(rstandard(lm(Usability~log(size_conv)))~lm(Usability~log(size_conv))$fitted.values, 
     xlab = 'Predicted Usability', ylab = 'Residuals', 
     main = 'Residual Plot (log(File size))', cex.lab = 1.5)
ncvTest(lm(Usability~log(size_conv))) #not sig


plot(rstandard(lm(Usability~Month_since))~lm(Usability~Month_since)$fitted.values, 
     xlab = 'Predicted Usability', ylab = 'Residuals',
     main = 'Residual Plot (Months since 2015)', cex.lab = 1.5)
ncvTest(lm(Usability~Month_since))$p

plot(rstandard(lm(Usability~CSV))~lm(Usability~CSV)$fitted.values, 
     xlab = 'Predicted Usability', ylab = 'Residuals', 
     main = 'Residual Plot (Has CSV)', cex.lab = 1.5)
leveneTest(lm(Usability~CSV))[1,3]

plot(rstandard(lm(Usability~Other))~lm(Usability~Other)$fitted.values, 
     xlab = 'Predicted Usability', ylab = 'Residuals', 
     main = 'Residual Plot (Has Other)', cex.lab = 1.5)
leveneTest(lm(Usability~Other))[1,3]

plot(rstandard(lm(Usability~Medals))~lm(Usability~Medals)$fitted.values, 
     xlab = 'Predicted Usability', ylab = 'Residuals', 
     main = 'Residual Plot (Medal)', cex.lab = 1.5)
leveneTest(lm(Usability~Medals))[1,3]

plot(rstandard(lm(Usability~Day))~lm(Usability~Day)$fitted.values, 
     xlab = 'Predicted Usability', ylab = 'Residuals', 
     main = 'Residual Plot (Day)', cex.lab = 1.5)
leveneTest(lm(Usability~Day))[1,3]


# #ok, gonna pick:
# log(size_conv) - quant
# CSV - cat (2)
# Other - cat (2) (usability only)
# Medals - cat (4)
# Months Since 2015 - quant
# Usability - quant - response

set.seed(1)
# index = sample(nrow(kaggle), size = 3000, replace = FALSE)
# kaggle = kaggle[index,]
index.train = sample(nrow(kaggle), size = 3000, replace = FALSE)
kaggle.test = kaggle[-index.train, c('log.size_conv','CSV','Other',
                                     'Medals','Month_since', 'Day')]
kaggle.test$Usability = kaggle[-index.train, 'Usability']
kaggle.train = kaggle[index.train, c('log.size_conv','CSV','Other',
                                     'Medals','Month_since', 'Day', 'Usability')]
detach(kaggle)

## assign each observation to one of the 10 groups
groups <- cut(1:nrow(kaggle), breaks = 10, labels = F)

err.test = matrix(NA, 10, 8)
prune.kaggle = c(rep(NA, 10))
for (i in 1:10) {
  ## define training and testing data for this split of cross validation
  print(paste0('GROUP ', i))
  testIndex <- which(groups == i)
  kaggle.test <- kaggle[testIndex,c('log.size_conv','CSV','Other',
                                 'Medals','Month_since', 'Day')]
  test.y <- kaggle[testIndex, 'Usability']
  kaggle.train <- kaggle[-testIndex, c('log.size_conv','CSV','Other',
                                    'Medals','Month_since', 'Day', 'Usability')]
  train.y <- kaggle[testIndex, 'Usability']
  
  ####The models
  ##Regular decision tree
  kaggle.tree = tree(Usability~log.size_conv+CSV+Other
                     +Medals+Month_since, data = kaggle.train)
  # summary(kaggle.tree)
  # plot(kaggle.tree)
  # text(kaggle.tree, digits = 2)
  
  cv.kaggle = cv.tree(kaggle.tree)
  plot(cv.kaggle$size, cv.kaggle$dev, type='b')
  best.cv = cv.kaggle$size[which.min(cv.kaggle$dev)]
  prune.kaggle = prune.tree(kaggle.tree, best = best.cv)
  plot(prune.kaggle)
  text(prune.kaggle, digits=2)
  print(summary(prune.kaggle))
  
  # mean((kaggle$Usability - predict(prune.kaggle))^2) #train
  predict.tree = predict(prune.kaggle, newdata=kaggle.test)
  err.test[i,1] = mean((test.y - predict.tree)^2) #yikes

}

## Look at average error rate and choose the one that is smallest
which.min(err.test[,1])

##Bagging
kaggle.bag = randomForest(Usability~log.size_conv+CSV+Other
                          +Medals+Month_since+Day, data = kaggle.train, mtry = 5,
                          ntree =  200, importance = TRUE)
importance(kaggle.bag)
mean((kaggle.train$Usability - predict(kaggle.bag))^2) #train
predict.bag = predict(kaggle.bag, newdata = kaggle.test)
mean((kaggle.test$Usability - predict.bag)^2)


##Random Forest
kaggle.rf = randomForest(Usability~log.size_conv+CSV+Other
                         +Medals+Month_since+Day, data = kaggle.train,
                         ntree = 200, importance = TRUE)
kaggle.rf
importance(kaggle.rf)
mean((kaggle.train$Usability - predict(kaggle.rf))^2) #train
predict.rf = predict(kaggle.rf, newdata = kaggle.test)
mean((kaggle.test$Usability - predict.rf)^2)


##Boosting with no interaction
kaggle.boost = gbm(Usability~log.size_conv+CSV+Other
                  +Medals+Month_since+Day, data = kaggle.train,
                  distribution="gaussian", n.trees=2000, 
                  interaction.depth=1, shrinkage = 0.01)
kaggle.boost
summary(kaggle.boost)
mean((kaggle.train$Usability - predict(kaggle.boost))^2) #train
predict.boost = predict(kaggle.boost, newdata = kaggle.test, n.trees=2000)
mean((kaggle.test$Usability - predict.boost)^2)

##Boosting with 2 factor interaction
kaggle.boost2 = gbm(Usability~log.size_conv+CSV+Other
                   +Medals+Month_since+Day, data = kaggle.train,
                   distribution="gaussian", n.trees=2000, 
                   interaction.depth=2, shrinkage = 0.01)
kaggle.boost2
summary(kaggle.boost2)
mean((kaggle.train$Usability - predict(kaggle.boost2, n.trees = 2000))^2) #train
predict.boost2 = predict(kaggle.boost2, newdata = kaggle.test, n.trees=2000)
mean((kaggle.test$Usability - predict.boost2)^2)

#3 factor
kaggle.boost3 = gbm(Usability~log.size_conv+CSV+Other
                    +Medals+Month_since+Day, data = kaggle.train,
                    distribution="gaussian", n.trees=2000, 
                    interaction.depth=3, shrinkage = 0.01)
kaggle.boost3
summary(kaggle.boost3)
mean((kaggle.train$Usability - predict(kaggle.boost3))^2) #train
predict.boost3 = predict(kaggle.boost3, newdata = kaggle.test, n.trees=2000)
mean((kaggle.test$Usability - predict.boost3)^2)

#4 factor
kaggle.boost4 = gbm(Usability~log.size_conv+CSV+Other
                    +Medals+Month_since+Day, data = kaggle.train,
                    distribution="gaussian", n.trees=2000, 
                    interaction.depth=4, shrinkage = 0.01)
kaggle.boost4
summary(kaggle.boost4)
mean((kaggle.train$Usability - predict(kaggle.boost4))^2) #train
predict.boost4 = predict(kaggle.boost4, newdata = kaggle.test, n.trees=2000)
mean((kaggle.test$Usability - predict.boost4)^2)

par(mfrow=c(2,3))
plot(kaggle.boost4 , i = "Medals", ylim = c(5,10))
plot(kaggle.boost4 , i = "Month_since", ylim = c(5,10))
plot(kaggle.boost4, i = 'log.size_conv', ylim = c(5,10))
plot(kaggle.boost4, i = 'CSV', ylim = c(5,10))
plot(kaggle.boost4, i = 'Day', ylim = c(5,10))
plot(kaggle.boost4, i = 'Other', ylim = c(5,10))

#BART
kaggle.x = kaggle[,c('log.size_conv','CSV','Other',
                  'Medals','Month_since')]
kaggle.y = kaggle[,'Usability']
kaggle.test.x = kaggle.test[,c('log.size_conv','CSV','Other',
                    'Medals','Month_since')]

kaggle.bart = gbart(kaggle.x, kaggle.y, x.test = kaggle.test.x)

yhat.bart <- kaggle.bart$yhat.test.mean
mean((kaggle.test$Usability - yhat.bart)^2)

ord <- order(kaggle.bart$varcount.mean, decreasing = T)
kaggle.bart$varcount.mean[ord]







