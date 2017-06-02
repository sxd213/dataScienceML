getROC_AUC = function(probs, true_Y){
  probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
  val = unlist(probsSort$x)
  idx = unlist(probsSort$ix)  
  
  roc_y = true_Y[idx];
  stack_x = cumsum(roc_y == 2)/sum(roc_y == 2)
  stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)    
  
  auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  #return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
  return(auc)
}