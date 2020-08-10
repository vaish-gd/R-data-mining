

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




mytree_gini <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1)

# 1) root 2400 587  <=50K (0.75541667 0.24458333)  
# 2) relationship= Not-in-family, Other-relative, Own-child, Unmarried 1271  79  <=50K (0.93784422 0.06215578)  
# 4) capital_gain< 4718.5 1247  57  <=50K (0.95429030 0.04570970) *
#   5) capital_gain>=4718.5 24   2  >50K (0.08333333 0.91666667) *
#   3) relationship= Husband, Wife 1129 508  <=50K (0.55004429 0.44995571)  
#   6) education= 10th, 11th, 12th, 1st-4th, 5th-6th, 7th-8th, 9th, Assoc-acdm, Assoc-voc, HS-grad, Preschool, Some-college 770 238  <=50K (0.69090909 0.30909091)  
#   12) capital_gain< 5095.5 725 194  <=50K (0.73241379 0.26758621) *
#   13) capital_gain>=5095.5 45   1  >50K (0.02222222 0.97777778) *
#   7) education= Bachelors, Doctorate, Masters, Prof-school 359  89  >50K (0.24791086 0.75208914) *

mytree_gini_complex <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp =-1)

mytree_gini_test <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp =0.1)

predict_unseen <-predict(mytree_gini, test, type = 'class')
confusion_matrix_gini <-  table(predict_unseen,test$income)


# confusion_matrix_gini
# 
# predict_unseen  <=50K  >50K
# <=50K   1163   171
# >50K      61   205


#accuracy_precision_recall_fscore 0.8550000 0.8718141 0.9501634 0.9093041

predict_unseen_gini_complex <-predict(mytree_gini_complex, test, type = 'class')
confusion_matrix_gini_complex <-  table(predict_unseen_gini_complex,test$income)


# confusion_matrix_gini_complex
# predict_unseen_gini_complex  <=50K  >50K
# <=50K   1061   161
# >50K     163   215


#now withholding one attribute
library(caret)
gini_importance <- varImp(mytree_gini,scale = FALSE)
print(gini_importance)

# Overall
# age             37.02275
# capital_gain   278.50322
# education      244.86466
# education_num  244.86466
# hours_per_week   8.13391
# marital_status 175.87444
# occupation      98.69002
# relationship   179.83478
# workclass        0.00000
# fnlwgt           0.00000
# race             0.00000
# sex              0.00000
# capital_loss     0.00000
# native_country   0.00000

#here we withhold the attribute which has 

gini_importance_complex <- varImp(mytree_gini_complex,scale = FALSE)
print(gini_importance_complex)

# Overall
# age            292.77339
# capital_gain   329.02114
# capital_loss    77.19730
# education      409.20591
# education_num  364.09808
# fnlwgt         293.01525
# hours_per_week 173.94678
# marital_status 209.50978
# native_country  65.04569
# occupation     315.05533
# race            49.33732
# relationship   222.61795
# sex             26.60023
# workclass      115.64204

#here we withhold the attribute that has lower index in the 1st one.

printcp(mytree_gini)

# CP nsplit rel error  xerror     xstd
# 1 0.154174      0   1.00000 1.00000 0.035874
# 2 0.073254      2   0.69165 0.71039 0.031622
# 3 0.034072      3   0.61840 0.63884 0.030303
# 4 0.010000      4   0.58433 0.60818 0.029698


#here for different values of cp, we get different xerrors the idea
#is to choose the one with the least amount of xerrors. cp also
#determines overfitting ensuring. 

#So ideally we can choose the cp value 0.5 and compare for 3 values
#cp=-1, cp= 0.1 and most optimal when cp is not included.

mytree_gini_cp_is_half <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp =0.1)
predict_unseen_gini_cp_is_half <-predict(mytree_gini_cp_is_half, test, type = 'class')
confusion_matrix_gini_cp_is_half <-  table(predict_unseen_gini_cp_is_half,test$income)


gini_importance_cp_is_half <- varImp(mytree_gini_cp_is_half,scale = FALSE)
print(gini_importance_cp_is_half)

# predict_unseen_gini_cp_is_half  <=50K  >50K
# <=50K   1166   214
# >50K      58   162

#here in all the three comparisons we get sex is the least used. So we remove sex 
#and obtain the tree.


d1 <- df

d1$sex <- NULL

data <- d1

set.seed(35)
sample_data <- data[sample(nrow(data), 4000), ]

smp_siz = floor(0.6*nrow(sample_data))
set.seed(35)
train_ind = sample(nrow(sample_data),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =sample_data[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=sample_data[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind



mytree_gini_upon_removing <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1)


predict_unseen_upon_removing <-predict(mytree_gini_upon_removing, test, type = 'class')
confusion_matrix_gini_upon_removing <-  table(predict_unseen_upon_removing,test$income)

mytree_gini_complex_removed <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp =-1)

predict_unseen_complex_removed <-predict(mytree_gini_complex_removed, test, type = 'class')
confusion_matrix_gini_complex_removed <-  table(predict_unseen_complex_removed,test$income)

mytree_gini_test_removed <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'gini'), 
  minsplit = 2, 
  minbucket = 1, cp =0.1)

predict_unseen_test_removed <-predict(mytree_gini_test_removed, test, type = 'class')
confusion_matrix_gini_test_removed <-  table(predict_unseen_test_removed,test$income)







# predict_unseen_upon_removing  <=50K  >50K
# <=50K   1163   171
# >50K      61   205

#accuracy_precision_recall_fscore[1] 0.8550000 0.8718141 0.9501634 0.9093041

# import_in_info_gain<- varImp(mytree,scale = FALSE)
# > import_in_info_gain
# Overall
# age             40.39883
# capital_gain   350.20901
# education      310.86546
# education_num  310.86546
# hours_per_week  28.24130
# marital_status 257.36456
# occupation     129.19185
# relationship   262.27187
# workclass        0.00000
# fnlwgt           0.00000
# race             0.00000
# sex              0.00000
# capital_loss     0.00000
# native_country   0.00000

#We can see subtle difference between info_gains attribute importance and
#and gini index. This is because cp is very less and most optimal.
#if we choose cp=-1 there will be significant difference between the variable importance.


#Naive_bayes.
Naive_Bayes_Model=naiveBayes(income ~., data=train)
NB_Predictions=predict(Naive_Bayes_Model,test)
confusion_matrix_naive<-table(NB_Predictions,test$income)

# NB_Predictions  <=50K  >50K
# <=50K   1140   198
# >50K      84   178

#accuracy_precision_recall_fscore 0.8237500 0.8520179 0.9313725 0.8899297


