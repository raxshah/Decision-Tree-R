source("kfold_greedy_algorithm_pessimistic.R")

#run each line by line and output will be generated inside '/output/output_pessimistic_summary.csv', '/output/output_pessimistic_rocCurve.png', 'output_pessimistic_prCurve.png'
#previous output would be overrided

accuracy1 <- kfold_greedy_algorithm_pessimistic(filename="input data/Bank note/data_banknote_authentication.txt",separator=",",head=F,classColumNum = 5,method="gini")

accuracy2 <- kfold_greedy_algorithm_pessimistic(filename="input data/Blood transfusion/transfusion.data",separator=",",head=T,classColumNum = 5,method="gini")

accuracy3 <- kfold_greedy_algorithm_pessimistic(filename="input data/Haberman/haberman.data",separator=",",head=F,classColumNum = 4,method="gini")

accuracy4 <- kfold_greedy_algorithm_pessimistic(filename="input data/Pima indian diabetes/pima-indians-diabetes.data",separator=",",head=F,classColumNum = 9,method="gini")

accuracy5 <- kfold_greedy_algorithm_pessimistic(filename="input data/Memographic/mammographic_masses.data",separator=",",head=F,classColumNum = 6,method="gini")

accuracy6 <- kfold_greedy_algorithm_pessimistic(filename="input data/Wine/wine.data",separator=",",head=F,classColumNum = 1,method="gini")

accuracy7 <- kfold_greedy_algorithm_pessimistic(filename="input data/Iris/iris_modified.csv",separator=",",head=F,classColumNum = 5,method="gini")

accuracy8 <- kfold_greedy_algorithm_pessimistic(filename="input data/Fertility/fertility_Diagnosis.txt",separator=",",head=F,classColumNum = 10,method="gini")

accuracy9 <- kfold_greedy_algorithm_pessimistic(filename="input data/User Knowledge/data_knowledge.csv",separator=",",head=T,classColumNum = 6,method="gini")

accuracy10 <- kfold_greedy_algorithm_pessimistic(filename="input data/Wisconsin Breast Cancer/breast-cancer-wisconsin.txt",separator=",",head=F,classColumNum = 10,method="gini")
