library(ROCR)
d12 <-data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf)


Naive_Bayes_Model=naiveBayes(income ~., data=train)
NB_Predictions=predict(Naive_Bayes_Model,test)
mat<-table(NB_Predictions,test$income)


pred1 <- prediction(as.numeric(predict_unseen),test$income)
infogain_perf <- performance(pred1,"tpr","fpr")

pred2 <- prediction( as.numeric(NB_Predictions),test$income )
nb_perf <- performance(pred2,"tpr","fpr")

pred3 <- prediction(as.numeric(predict_unseen_gini_complex),test$income)
gini_complex_perf <- performance(pred3,"tpr","fpr")


plot( gini_complex_perf, col="green")
plot(infogain_perf,col="red")
plot(nb_perf,add=T,col='blue')
legend("topleft",
       c("gini complex","info gain","naive bayes"),
       fill=c("green","red","blue"))



plot(infogain_perf, colorize = TRUE, color ="blue")
plot(nb_perf, colorize = TRUE)


