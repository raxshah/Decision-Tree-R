require(dplyr)
require("PRROC")

kfold_greedy_algorithm_validation <- function(filename,separator,head,classColumNum,method){
   
             
        set.seed(1)
        
        # filename <- "input data/data_banknote_authentication.txt"
        # separator <- ","
        # classColumNum <- 5
        # head <- F
        # method <- "gini"
        
        uci.data <- read.table(file=filename,sep = separator,header = head,stringsAsFactors = F,na.strings = c("?"))
        names(uci.data)[classColumNum] <- c(paste("V",classColumNum,sep=""))
        col_withoutClass <- which( colnames(uci.data) != paste("V",classColumNum,sep="") )
        
        print("File has been read successfully")
        
        n <- nrow(uci.data)
        shuffled <- uci.data[sample(n),]
        
        # Initialize the accs vector
        accs <- rep(0,10)
        b_accs <- rep(0,10)
        f_measure <- rep(0,10)
        roc_measure <<- data.frame(Response= numeric(),Predictor= numeric())
        
        #fold <- 1
        
        # 10  fold times
        for (fold in 1:10) {
                
                print(paste("-----------------------------Fold ",fold,"----------------------------"))
                
                # These indices indicate the interval of the test set
                indices <- (((fold-1) * round((1/10)*nrow(shuffled))) + 1):((fold*round((1/10) * nrow(shuffled))))
                
                # Exclude them from the train set
                train <- shuffled[-indices,]
                
                #Now prepare validation set of 25% to find the accuracy 
                n1 <- nrow(train)
                
                shuffled_v <- train[sample(n1),]
                train <- shuffled_v[1:round(0.75 * n1),]
                test <- shuffled_v[(round(0.75 * n1) + 1):n1,]
                
                test <- test[complete.cases(test),]
                train <- train[complete.cases(train),]
                

                #remove prviously saved dataframes 
                if(exists("df.predictedValue") == T)
                        rm(df.predictedValue)
                
                if(exists("summary_train_model") == T)
                        rm(summary_train_model)
                
                if(exists("summary_train_model_b") == T)
                        rm(summary_train_model_b)
                
                #generate unique sequnce in R to give unique id to each node of decision tree
                idFactory <- function(buf_n=1000000) {
                        curr <- 0L
                        last <- -1L
                        val <- NULL
                        function() {
                                if ((curr %% buf_n) == 0L) {
                                        val <<- formatC(last + seq_len(buf_n), width=9, flag=0, format="d")
                                        last <<- last + buf_n
                                        curr <<- 0L
                                }
                                val[curr <<- curr + 4L]
                        }
                }
                emitID <- idFactory()
                
                #find best split
                best_split <- function(train){
                        
                        summary_Column_Gini <- matrix(data= NA,ncol = 3,byrow = T)
                        for(col in col_withoutClass){
                                
                                #find mid-point vector
                                col.value <- train[,col]
                                col.value <- sort(col.value)
                                col.value <- unique(col.value)
                                mean.point.vector <- rep(NA,length(col.value))
                                
                                for(i in 1:length(col.value)){
                                        
                                        
                                        if(is.na(col.value[i+1]) == T){
                                                
                                                col.value[i+1] <- col.value[i]
                                        }
                                        
                                        mean.point <- (col.value[i]+ col.value[i+1])/2
                                        mean.point.vector[i] <- mean.point
                                }
                                
                                mean.point.vector <- mean.point.vector[!is.na(mean.point.vector)]
                                
                                summaryGini <- matrix(data= NA,ncol = 2,byrow = T)
                                
                                for(i in 1:length(mean.point.vector)){
                                        
                                        totalRecords <- nrow(train)
                                        
                                        split0 <- train[train[,col] < mean.point.vector[i],]
                                        split1 <- train[train[,col] >= mean.point.vector[i],]
                                        
                                        split0.totalRecords <- nrow(split0)
                                        split1.totalRecords <- nrow(split1)
                                        
                                        if(split0.totalRecords >0){
                                                
                                                #count Gini information
                                                if(method == "gini"){
                                                        
                                                        split0.gini <- 1 - (nrow(split0[split0[,classColumNum] == 0,])/split0.totalRecords) ^2 - 
                                                                (nrow(split0[split0[,classColumNum] == 1,])/split0.totalRecords)^2
                                                }
                                                #Otherwise count Infromation Gain
                                                else{
                                                        n0 <- nrow(split0[split0[,classColumNum] == 0,])
                                                        n1 <- nrow(split0[split0[,classColumNum] == 1,])
                                                        n0.log.value <- 0
                                                        n1.log.value <- 0
                                                        
                                                        if(n0 > 0)
                                                                n0.log.value <- log(n0/split0.totalRecords)
                                                        
                                                        if(n1 > 0)
                                                                n1.log.value <- log(n1/split0.totalRecords)
                                                        
                                                        split0.gini <-  - ((n0/split0.totalRecords) * n0.log.value) - ((n1/split0.totalRecords) * n1.log.value)
                                                        
                                                }
                                                
                                                
                                                
                                        }else{
                                                split0.gini <- 0
                                                
                                        }
                                        
                                        if(split1.totalRecords >0){
                                                
                                                if(method == "gini"){
                                                        split1.gini <- 1 - (nrow(split1[split1[,classColumNum] == 0,])/split1.totalRecords) ^2 - 
                                                                (nrow(split1[split1[,classColumNum] == 1,])/split1.totalRecords)^2
                                                        
                                                }
                                                else{
                                                        
                                                        n0 <- nrow(split1[split1[,classColumNum] == 0,])
                                                        n1 <- nrow(split1[split1[,classColumNum] == 1,])
                                                        n0.log.value <- 0
                                                        n1.log.value <- 0
                                                        
                                                        if(n0 > 0)
                                                                n0.log.value <- log(n0/split1.totalRecords)
                                                        
                                                        if(n1 > 0)
                                                                n1.log.value <- log(n1/split1.totalRecords)
                                                        
                                                        split1.gini <-  - ((n0/split1.totalRecords) * n0.log.value) - ((n1/split1.totalRecords) * n1.log.value)
                                                        
                                                }
                                                
                                        }else{
                                                split1.gini <- 0 
                                                
                                        }
                                        
                                        gini_split <- as.double(split0.gini * (split0.totalRecords/totalRecords)) + as.double(split1.gini * (split1.totalRecords/totalRecords))
                                        
                                        if(split0.gini == 0 | split1.gini ==0){
                                                
                                                gini_split <- gini_split + 0.01
                                        }
                                        
                                        summaryGini <- rbind(summaryGini,c(mean.point.vector[i],gini_split))
                                }
                                
                                summaryGini <- summaryGini[which.min(summaryGini[,2]),]
                                
                                splitValue <- summaryGini[1]
                                giniValue <- summaryGini[2]
                                
                                summary_Column_Gini <- rbind(summary_Column_Gini,c(col,splitValue,giniValue))
                        }
                        
                        summary_Column_Gini <- summary_Column_Gini[which.min(summary_Column_Gini[,3]),]
                        colNum <- summary_Column_Gini[1]
                        splitValue <- summary_Column_Gini[2]
                        giniValue <- summary_Column_Gini[3]
                        
                        return(c(colNum,splitValue,giniValue))
                        
                }
                print("Growing Tree....")
                
                sequ <<- 0
                nodeOwnNum <<- 0
                child0 <<- 1
                child1 <<- 2
                parent <<- 0
                summary_train_model <<- NULL
                summary_train_model <<- data.frame(NodeNum = NA, SplitColumn= NA,SplitValue=NA,ClassLable= NA,NodeType = NA,Parent=NA,Child0= NA,Child1= NA,stringsAsFactors = F)
                
                
                growtree <- function(dataset,nodeOwnNum,sequ,parent,child0,child1){
                        sequ <- sequ+2
                        
                        identical_feature <- nrow(unique(dataset[,-classColumNum]))
                        dataset.clases <- nlevels(factor(dataset[,classColumNum]))
                        
                        if(dataset.clases == 1 | identical_feature == 1){
                                summary_train_model <<- rbind(summary_train_model,list(nodeOwnNum,NA,NA,dataset[1,classColumNum],"LN",parent,NA,NA))
                        }
                        else{
                                dataset.splitSummary <- best_split(dataset)
                                dataset.colNum <- dataset.splitSummary[1]
                                dataset.splitValue <- dataset.splitSummary[2]
                                summary_train_model <<- rbind(summary_train_model,list(nodeOwnNum,dataset.colNum,dataset.splitValue,NA,"IN",parent,child0,child1))
                                
                                split0 <- dataset[dataset[,dataset.colNum] >= dataset.splitValue,]
                                split1 <- dataset[dataset[,dataset.colNum] < dataset.splitValue,]
                                
                                
                                if(!(nrow(split0) == 0)){
                                        growtree(split0,child0,sequ,nodeOwnNum,as.numeric(emitID()),as.numeric(emitID()))
                                        
                                }   
                                else{
                                        
                                }
                                
                                if(!(nrow(split1) == 0)){
                                        
                                        growtree(split1,child1,sequ,nodeOwnNum,as.numeric(emitID()),as.numeric(emitID()))
                                }   
                                else{
                                        
                                }
                                
                        }
                }
                
                growtree(train,nodeOwnNum,sequ,parent,child0,child1)
                summary_train_model <- summary_train_model[-1,]
                isFirstPruning <<- T
                accuracy <<- 0
                summary_train_model_b <<- summary_train_model

                
                print("Tree has been grown successfully. Now Predicting....")
                
                pred <<- NULL
                conf <<- NULL
                
                while(T){
                        
                        print(paste("Inside While...",accuracy))
                        
                        if(!isFirstPruning){
                                
                                print("Inside Pruning...")
                                
                                
                                if(exists("df.predictedValue") == T){
                                        rm(df.predictedValue)}
                                
                                if(exists("pred") == T){
                                        rm(pred)}
                                

                                
                                if(nrow(summary_train_model_b) <= 3)
                                        break;
                                
                                print("Pruning the tree....")
                                

                                remove_child_leaf <- function(x){
                                        
                            
                                        
                                        parent.node <- summary_train_model_b[summary_train_model_b$NodeNum == x,]
                                        
                                        firstChild <- summary_train_model_b[summary_train_model_b$NodeNum == parent.node$Child0,]
                                        secondChild <- summary_train_model_b[summary_train_model_b$NodeNum == parent.node$Child1,]
                                        
                                        summary_train_model_b <<- summary_train_model_b[!summary_train_model_b$NodeNum == secondChild$NodeNum,]
                                        summary_train_model_b <<- summary_train_model_b[!summary_train_model_b$NodeNum == firstChild$NodeNum,]
                                        
                                        

                                        
                                        summary_train_model_b[summary_train_model_b$NodeNum == x,]$NodeType <- "PN"
                                        summary_train_model_b[summary_train_model_b$NodeNum == x,]$SplitColumn <- NA
                                        summary_train_model_b[summary_train_model_b$NodeNum == x,]$SplitValue <- NA
                                        summary_train_model_b[summary_train_model_b$NodeNum == x,]$Child0 <- NA
                                        summary_train_model_b[summary_train_model_b$NodeNum == x,]$Child1 <- NA
                                        
                                 
                                        return(summary_train_model_b)
                                        
                                }
                                

                                parent.df <- summary_train_model_b %>% filter(NodeType == "LN" | NodeType == "PN") %>% select(Parent) %>% group_by(Parent) %>% summarise(parent_count = n()) %>% filter(parent_count == 2)
                                
                                if(nrow(parent.df) == 0) break;
                                
                                parent.df <- as.vector(parent.df$Parent)
                                
                                
                                
                                for(remove in parent.df){
                                        dfk <- remove_child_leaf(remove)
                                        summary_train_model_b <<- dfk
                                        
                                }
                                

                                
                        }
                        
                        summary_train_model <<- summary_train_model_b
                        
                        
 
                        df.predictedValue <<- NULL
                        df.predictedValue <<- data.frame(predictedValue = NA)
       
                        predict <- function(record,parent,nodenum){
                                
                                
                                dict <- summary_train_model %>% filter(NodeNum == nodenum) %>%  select(NodeNum,SplitColumn,SplitValue,NodeType,ClassLable,Parent,Child0,Child1) 

                                if(dict$NodeType == "LN" | dict$NodeType == "PN"){
                                        
                                        if(dict$NodeType == "LN")
                                                return(dict$ClassLable)
                                        
                                        else
                                                return(record[,classColumNum])
                                        
                                }
                                else{
                                        split.column <- dict$SplitColumn
                                        split.value <- dict$SplitValue
                                        
                                        childNode <- NA
                                        if(record[,split.column] >= split.value){
                                                
                                                childNode <- dict$Child0
                                                
                                        }else{
                                                childNode <- dict$Child1
                                        }
                                        
                                        parent <- dict$Parent
                                        
                                        predict(record,parent,childNode)
                                }
                                
                        }
                        
                        df.predictedValue <<- data.frame(predictedValue = NA)
                        pred <<- NULL
                        pred.classLable <- NULL
                        
                        for(n_1 in 1:nrow(test)){
                                
                         
                                pred.classLable <-  predict(test[n_1,],0,0)
                                df.predictedValue <- rbind(df.predictedValue,pred.classLable)
                                
                        }
                        pred <<- df.predictedValue[-1,]
                        
                        print("Prediction completed. Now counting accuracy......")
                        
                        # Calculate the confusion matrix: conf
                        conf <- table(test[,classColumNum], pred)
                        
                        #count accuracy
                        accuracy_n <- sum(diag(conf))/sum(conf)
                        
                        
                        if(isFirstPruning == T)
                        {
                                print(paste("Accuracy of tree is: ",accuracy_n,sep=" "))
                        }else
                        {
                                print(paste("Accuracy of Pruned tree is: ",accuracy_n,sep=" "))
                        }
                        
                        if(accuracy_n > accuracy){
                                
                                if(!isFirstPruning)
                                        print(paste("Accuracy improved with this prune. Now Pruning again.. ",sep=" "))
                                
                                accuracy <<- accuracy_n
                                
                        }else{
                                print(paste("Accuracy is not improved.So Keeping previously pruned tree for prediction.",sep=" "))
                                accuracy <<- accuracy_n
                                break;        
                                
                        }
                        
                        isFirstPruning <<- F
                        
                }
                print(paste("Best Accuracy for fold",fold,"is:",accuracy,sep=" "))
                
                accs[fold]<- sum(diag(conf)) / sum(conf)

                
                 for (i3 in 1:length(pred)) {
                         roc_measure <<- rbind(roc_measure, data.frame(Response =test[,classColumNum][i3], Predictor = pred[i3]))
                 }
                print(str(roc_measure))

                sensitivity <- conf[1,1]/(conf[1,1] + conf[2,1])
                specificity <- conf[2,2]/(conf[1,2] + conf[2,2])
                precision <-  conf[1,1]/(conf[1,1] + conf[1,2])
                        
                b_accs[fold]<- (sensitivity + specificity )* 0.5
                f_measure[fold] <- 2 * (precision*sensitivity)/(precision+sensitivity)
                
        }
        
        accuracy <- mean(accs)
        b_accuracy <- mean(b_accs)
        f_measure <- mean(f_measure)
        print(str(roc_measure))
        print(paste("Overall Accuracy for validation set is:",accuracy,sep=" "))
        print(paste("Overall Balanced Accuracy for validation set is:",b_accuracy,sep=" "))
        print(paste("Overall F measure for validation set is:",f_measure,sep=" "))
        
        roc_curve <- roc.curve(roc_measure$Response,roc_measure$Predictor, curve = TRUE)
        pr_curve <- pr.curve(roc_measure$Response,roc_measure$Predictor, curve = TRUE)
       
        png("output/output_validation_rocCurve.png", width=4, height=4, units="in", res=300)
        plot(roc_curve)
        dev.off()
        
        png("output/output_validation_prCurve.png", width=4, height=4, units="in", res=300)
        plot(pr_curve)
        dev.off()
        
        
        out_df <- data.frame(Accuracy=accuracy,Balanced_Accuracy = b_accuracy,F1measure= f_measure)
        
        write.table(out_df,file= "output/output_validation_summary.csv",row.names = F,sep = ",")
        
        print("Output file is exported...")
        
        return_values <- data.frame(Accuracy=accuracy,Balanced_Accuracy = b_accuracy,F1measure= f_measure)
        
        return(return_values)
        
}