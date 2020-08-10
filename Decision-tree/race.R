read.csv("data.txt", stringsAsFactors = FALSE)

df <- read.table("data.txt", sep = "," , header = FALSE)

colnames(df) <- c("age", "workclass", "fnlwgt", "education", "education_num", 
                  "marital_status", "occupation","relationship", "race", "sex", 
                  "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

#head(df,2)

df<- read.table("data.txt",sep = ",", header = FALSE, na.strings = " ?")

df <- na.omit(df)                #omit missing values


d1 <- df




data <- df


set.seed(35)
pd3 <- data[sample(nrow(data), 4000), ]

smp_siz = floor(0.6*nrow(pd3))
set.seed(35)
train_ind = sample(nrow(pd3),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =pd3[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=pd3[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind





mytree <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  minsplit = 2, 
  minbucket = 1)


train2 <- train

train2$race <- NULL

mytree <- rpart(
  income ~ ., 
  data = train2, 
  method = "class",
  
  minsplit = 2, 
  minbucket = 1)

#confusion matrix

predict_unseen <-predict(mytree, test2, type = 'class')

table_mat <-  table(test$income, predict_unseen)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)









attr <- information.gain(income ~ ., data=train, unit="log2")
print(attr)

subset <- cutoff.k(attr,10)

f <- as.simple.formula(subset, "income")
print(f)


confusionMatrix(predict_unseen,test2$income)

test2 <- test

test2$race <- NULL

install.packages("caret", dependencies = TRUE)
install.packages("dplyr")
install.packages("xlsx")
install.packages("FSelector")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("data.tree")
install.packages("caTools")

library("caret")
library("dplyr")
library("xlsx")
library("FSelector")
library("rpart")
library("rpart.plot")
library("data.tree")
library("caTools")


rplot.plot(mytree)










