# 
# # ex1
# # Deciding on an algorithm.
# Asking the right question.
# 
# ex2
# Our algorithm may be overfitting the training data, predicting both the signal and the noise.
# 
# ex3
# NOT! 10% test set, 90% training set
# # shoudl be ~60/40  
# ex4
# Predictive value of a positive
# sensitiviy
# 
# ex5
# NOT 99% duh - click mistake?
# 0,9%

do_ex5 <- function (Sensitivity, Specificity, OccurenceRate){
  # Sensitivity = TP / (TP+FN)
  # Specificity = TN / (TN+FP)
  # Positive Predictive Value = TP / (TP + FP)
  # Negative Predictive Value = TN / (TN + FN)
  # Accuracy = (TP + TN)/ (TP+FP+TN+FN)
  
  # OccurenceRate = (TP+FN)/(TN+FP)
  
  N = 1e9
  
  TP <- Sensitivity * OccurenceRate * N
  FN <- (1-Sensitivity)* OccurenceRate * N
  TN <- Specificity * (1-OccurenceRate) * N
  FP <- (1-Sensitivity) * (1-OccurenceRate) * N
  PPV = TP / (TP + FP)
  NPV = TN / (TN + FN)
  Accuracy = (TP + TN)/ (TP+FP+TN+FN)
  
  print (paste("Positive Predictive Value=", PPV))
  c(TP=TP,FN=FN, TN=TN, FP=FP, PPV=PPV, NPV=NPV, ACC=Accuracy)
}

ex5<-function(){
  do_ex5(99/100, 99/100, 1/1000)[5]*100
  
  #do_ex5(80/100, 70/100, 1/10)
  
}

