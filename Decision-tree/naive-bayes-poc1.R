#install.packages("e1071")
#library(e1071)

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


Naive_Bayes_Model=naiveBayes(income ~., data=train)
NB_Predictions=predict(Naive_Bayes_Model,test)
mat<-table(NB_Predictions,test$income)

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

