source("greedy_algorithm.R")

list.of.packages <- c("dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#running greedy decision tree on 'blood transfusion' data of UCI machine learning
accuracy <- greedy_algorithm(filename="input data/Blood transfusion/transfusion.data",separator=",",head=T,classColumNum = 5,method="information")

summary_table <- data.frame(Accuracy= accuracy)

#write the output to csv file
write.table(summary_table,file= "output/output_result.csv",row.names = F,sep = ",")
print("Output is stored inside 'output/output_result.csv'")