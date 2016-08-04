source("kfold_greedy_algorithm.R")

#running greedy decision tree on diferent dataset of UCI machine learning
accuracy1 <- kfold_greedy_algorithm(filename="input data/Bank note/data_banknote_authentication.txt",separator=",",head=F,classColumNum = 5,method="gini")
accuracy2 <- kfold_greedy_algorithm(filename="input data/Blood transfusion/transfusion.data",separator=",",head=T,classColumNum = 5,method="gini")
accuracy3 <- kfold_greedy_algorithm(filename="input data/Haberman/haberman.data",separator=",",head=F,classColumNum = 4,method="gini")
accuracy4 <- kfold_greedy_algorithm(filename="input data/Pima indian diabetes/pima-indians-diabetes.data",separator=",",head=F,classColumNum = 9,method="gini")
accuracy5 <- kfold_greedy_algorithm(filename="input data/Memographic/mammographic_masses.data",separator=",",head=F,classColumNum = 6,method="gini")
accuracy6 <- kfold_greedy_algorithm(filename="input data/Fertility/fertility_Diagnosis.txt",separator=",",head=F,classColumNum = 10,method="gini")
accuracy7 <- kfold_greedy_algorithm(filename="input data/User Knowledge/data_knowledge.csv",separator=",",head=T,classColumNum = 6,method="gini")
accuracy8 <- kfold_greedy_algorithm(filename="input data/Wisconsin Breast Cancer/breast-cancer-wisconsin.txt",separator=",",head=F,classColumNum = 10,method="gini")
accuracy9 <- kfold_greedy_algorithm(filename="input data/Wine/wine.data",separator=",",head=F,classColumNum = 1,method="gini")
accuracy10 <- kfold_greedy_algorithm(filename="input data/Iris/iris_modified.csv",separator=",",head=F,classColumNum = 5,method="gini")

summary_table <- data.frame(Accuracy1= accuracy1,Accuracy2=accuracy2,Accuracy3=accuracy3,Accuracy4=accuracy4,Accuracy5=accuracy5,Accuracy6=accuracy6,Accuracy7=accuracy7,Accuracy8=accuracy8,Accuracy9=accuracy9,Accuracy10=accuracy10)

#write the output to csv file
write.table(summary_table,file= "output/output_result.csv",row.names = F,sep = ",")

print("Output is stored inside 'output/output_result.csv'")
