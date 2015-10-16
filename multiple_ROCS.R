#' This function will take a list of prediction vectors
#' and a target variable vector and produce ROC curves 
#' for each prediction vector given
#' Vectors must be given in a list, with the name of each model
#' as the name of the prediction vector
#' 

library('data.table')
library('ROCR')
library('ggplot2')

ROC.plot <- function(pred.vectors,target){
  pred.obj <- prediction(pred.vectors[[1]],target)
  perf.obj <- performance(pred.obj,'tpr','fpr')
  roc.data <- data.table(fpr=perf.obj@x.values[[1]],
                         tpr=perf.obj@y.values[[1]],
                         model=names(pred.vectors)[[1]])
  if(length(pred.vectors)>1){
    for(k in 2:length(pred.vectors)){
      pred.obj2 <- prediction(pred.vectors[[k]],target)
      perf.obj2 <- performance(pred.obj2,'tpr','fpr')
      roc.data <- rbindlist(list(roc.data, data.table(fpr=perf.obj2@x.values[[1]],
                                                 tpr=perf.obj2@y.values[[1]],
                                                 model=names(pred.vectors)[[k]])))
    } #end for loop
  } #end if statement
  
  ggplot(roc.data)+geom_line(aes(x=fpr,y=tpr,color=model))
}


if(F){
  pred.vectors <- list(glm=uw_glm$Prediction,noprev=uw_noprev$Prediction)
  target <- uw_glm$Home.Win
  ROC.plot(pred.vectors,target)
}