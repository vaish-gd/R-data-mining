
path <- 'http://itlab.uta.edu/courses/CSE5334-data-mining/current-offering/project-1-classification/census-adult.txt'
data <- read.csv(path,header=FALSE)
colnames(data)<-c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","salary")
#nrow(data)
install.packages("rpart.plot")
library("rpart.plot")
set.seed(35)
# Set Seed so that same sample can be reproduced in future also
#pd <- data[sample(n=3,size=4000,replace=FALSE, prob=c(0.4,0.4,0.2)),]
pd2 <- sample(data,size=4000,prob=c(0.6,0.4),replace=F)
train <- data[pd==1,]
test <-  data[pd==2,]


#install.packages("rpart.plot", dependencies=TRUE, repos="http://cran.rstudio.com/")

#use 'library("rpart.plot")' latlontree = rpart(MEDV ~ LAT + LON, data=boston)

pd3 <- data[sample(nrow(data), 4000), ]

smp_siz = floor(0.6*nrow(pd3))
set.seed(35)
train_ind = sample(nrow(pd3),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =pd3[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=pd3[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind



mytree <- rpart(
  salary ~ ., 
  data = train, 
  method = "class",
  parms = list(split = 'information'), 
  minsplit = 2, 
  minbucket = 1)

d1 <- data

attr <- information.gain(salary ~ ., data=d1, unit="log2")



