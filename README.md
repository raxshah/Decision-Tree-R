# Decision-Tree-R

In this repo, I have developed binary decision tree from scratch using R. Following different approaches are used in order to make decision tree


1. Implemented the **greedy algorithm** that learns a classification tree given a data set assuming
all features are numerical. For this, Gini and information gain can be specified by user to decide on the best attribute to split in every step. Since it is greedy decision tree, algorithm will stop growing the tree when all examples in a node belong to the same class or the remaining examples
contain identical features.

2. Implemented 10-fold cross-validation to evaluate the accuracy of algorithm on 10 different
data sets from the UCI Machine Learning Repository. Selected only those data sets where all features are
numerical. In certain cases, algorithm will convert categorical features into numerical by encoding them using
sparse binary representation. That is, if feature values belong to a set {blue, yellow,red, green}, it will encode
this feature using 4-dimensional binary vectors such that if the feature value is blue, the encoding
is (1, 0, 0, 0), if the feature value is yellow, the encoding is (0, 1, 0, 0), etc.
