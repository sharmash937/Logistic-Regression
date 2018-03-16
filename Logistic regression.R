data <- read.csv(file.choose(), header = T)
str(data)
data$admit <- as.factor(data$admit)
data$rank <- as.factor(data$rank)
str(data)

#Two way table of factor variables
#cross tabulation for admit and rank
xtabs(~admit+rank, data = data)

#sample the train and test
ind <- sample(2, nrow(data), replace = T, prob=c(0.8,0.2))

train <- data[ind==1,]
test <- data[ind==2,]

#making a model for glm
model <- glm(admit~.-gpa, data = train, family = "binomial")
summary(model)


#predition
p1 <- predict(model, train, type = 'response')
head(p1)
head(train)



#misclassification error on train
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted=pred1, Actual=train$admit)

1-sum(diag(tab1))/sum(tab1)


#misclassification error on test  
p2 <- predict(model, test, type = 'response')
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(Predicted=pred2, Actual=test$admit)

1-sum(diag(tab2))/sum(tab2)






