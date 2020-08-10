accuracy_precision_recall_fscore <- function(matrix1) {
  tp <- matrix1[2,2]
  tn <- matrix1[1,1]
  fp <- matrix1[1,2]
  fn <- matrix1[2,1]
  accuracy <- (tp+tn)/(tp+tn+fp+fn)
  precision <- tp/(tp+fp)
  recall  <- tp/(tp+fn)
  f_score <- 2*(precision*recall)/(precision+recall)
  
  vector <- c(accuracy,precision,recall,f_score)
  
  return(vector)
   }