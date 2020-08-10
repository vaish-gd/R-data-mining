# install.packages("caret", dependencies = TRUE)
# install.packages("dplyr")
# install.packages("xlsx")
# install.packages("FSelector")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("data.tree")
# install.packages("caTools")
# library("caret")
# library("dplyr")
# library("xlsx")
# library("FSelector")
# library("rpart")
# library("rpart.plot")
# library("data.tree")
# library("caTools")

#all the dependencies and libraries are importted and installed.

df <- read.csv("data.txt", stringsAsFactors = FALSE)

df <- read.table("data.txt", sep = "," , header = FALSE)


#head(df,2)

df<- read.table("data.txt",sep = ",", header = FALSE, na.strings = " ?")

df <- na.omit(df)                #omit missing values

colnames(df) <- c("age", "workclass", "fnlwgt", "education", "education_num", 
                  "marital_status", "occupation","relationship", "race", "sex", 
                  "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")
data <- df


set.seed(35)
sample_data <- data[sample(nrow(data), 4000), ]

smp_siz = floor(0.6*nrow(sample_data))
set.seed(35)
train_ind = sample(nrow(sample_data),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =sample_data[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=sample_data[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind


mytree <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'information'), 
  minsplit = 2, 
  minbucket = 1)

# here let's first include all the attributes and then obtain a decision tree

print(mytree)

# 1) root 2400 587  <=50K (0.75541667 0.24458333)  
# 2) relationship= Not-in-family, Other-relative, Own-child, Unmarried 1271  79  <=50K (0.93784422 0.06215578)  
# 4) capital_gain< 4718.5 1247  57  <=50K (0.95429030 0.04570970) *
#   5) capital_gain>=4718.5 24   2  >50K (0.08333333 0.91666667) *
#   3) relationship= Husband, Wife 1129 508  <=50K (0.55004429 0.44995571)  
#   6) education= 10th, 11th, 12th, 1st-4th, 5th-6th, 7th-8th, 9th, Assoc-acdm, Assoc-voc, HS-grad, Preschool, Some-college 770 238  <=50K (0.69090909 0.30909091)  
#   12) capital_gain< 5095.5 725 194  <=50K (0.73241379 0.26758621) *
#   13) capital_gain>=5095.5 45   1  >50K (0.02222222 0.97777778) *
#   7) education= Bachelors, Doctorate, Masters, Prof-school 359  89  >50K (0.24791086 0.75208914) *


#Now lets predict the test set and obtain a confusion matrix



predict_unseen <-predict(mytree, test, type = 'class')
confusion_matrix <-  table(predict_unseen,test$income)
#here predict will be xaxis actual wil be y axis
print(confusion_matrix)


# predict_unseen  <=50K  >50K
# <=50K   1163   171
# >50K      61   205


accuracy_precision_recall_fscore <- function(matrix1) {
  tn <- matrix1[2,2]
  tp <- matrix1[1,1]
  fn <- matrix1[2,1]
  fp <- matrix1[1,2]
  accuracy <- (tp+tn)/(tp+tn+fp+fn)
  precision <- tp/(tp+fp)
  recall  <- tp/(tp+fn)
  f_score <- 2*(precision*recall)/(precision+recall)
  vector <- c(accuracy,precision,recall,f_score)
  return(vector)
}
print(accuracy_precision_recall_fscore(confusion_matrix))
#prints values for acc,precis,recall,fscore is 0.8550000 0.8718141 0.9501634 0.9093041

confusionMatrix(predict_unseen,test$income)
#This prints a lot of parameters that are derived from confusionMatrix for determining the
#efficiency of the model.
#like kappa cvalue,sensitivity,specifity,detection prevalance, etc.
print(confusionMatrix)





attr <- information.gain(income ~ ., data=train, unit="log2")
print(attr)

# here it uses information gain - by using the log2 you ensure, it's computed like that

# attr_importance
# age                0.086344158
# workclass          0.021142475
# fnlwgt             0.000000000
# education          0.117544148
# education_num      0.111162798
# marital_status     0.157030142
# occupation         0.107065152
# relationship       0.172465800
# race               0.007671101
# sex                0.037873947
# capital_gain       0.097742991
# capital_loss       0.040897648
# hours_per_week     0.059180305
# native_country     0.026166586


#the given below attributes are the used attributes.
# capital_gain       0.097742991
# relationship       0.172465800
# education          0.117544148

# Some attributes that have higher information_gain than the used attributes is
# marital_status     0.157030142
# occupation         0.107065152
# education_num      0.111162798

#So why are they not used in building the tree.
#This is because these values only talks of information-gain initially,
#not after the 1st split by taking relationship(highest-gain) into account.

#Now after knowing that these attributes are used in building the
# decision tree, we know that by removing all or some of any of the other
# attributes that arent involved in decision tree building, will yield the same result.

# And among the attributes that arent involved in building the decision tree,

#So let us remove many of the attributes not involved in the tree

d1 <- df

d1$workclass <- NULL

d1$race <- NULL

d1$sex <- NULL

d1$education_num <-NULL

d1$native_country <- NULL

d1$hours_per_week <- NULL

d1$age <- NULL

data <- d1



set.seed(35)
sample_data <- data[sample(nrow(data), 4000), ]

smp_siz = floor(0.6*nrow(sample_data))
set.seed(35)
train_ind = sample(nrow(sample_data),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =sample_data[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=sample_data[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind





mytree_with_removed_attrs <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'information'), 
  minsplit = 2, 
  minbucket = 1)

predict_unseen <-predict(mytree_with_removed_attrs, test, type = 'class')

table_mat <-  table(predict_unseen,test$income)

mytree_complex <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'information'), 
  minsplit = 2, 
  minbucket = 1,cp=-1)

predict_unseen <-predict(mytree_complex, test, type = 'class')

table_mat <-  table(predict_unseen,test$income)


# predict_unseen  <=50K  >50K
# <=50K   1163   171
# >50K      61   205

#Here we can see that when we print the confusion matrix of table_mat 
#after removing the following attributes we get the same confusion matrix.
#So even upon removing those attributes it makes no difference to the confusion matrix

rpart.plot(mytree)






