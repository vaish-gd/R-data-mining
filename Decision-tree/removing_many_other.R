
colnames(df) <- c("age", "workclass", "fnlwgt", "education", "education_num", 
                  "marital_status", "occupation","relationship", "race", "sex", 
                  "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")


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





mytree <- rpart(
  income ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'information'), 
  minsplit = 2, 
  minbucket = 1)

predict_unseen <-predict(mytree, test, type = 'class')

table_mat <-  table(test$income, predict_unseen)

attr <- information.gain(income ~ ., data=train, unit="log2")
print(attr)




