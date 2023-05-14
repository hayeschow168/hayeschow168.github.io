library(caret)
set.seed(32343)




# read data
setwd("C:/Users/hchow/Downloads/Practical Machine Learning/course project")
data_test <- read.csv("pml-testing.csv", na.strings = c("", "NA"))
data_train <- read.csv("pml-training.csv", na.strings = c("", "NA"))

# explore data
# data_test[, grepl("_belt", names(data_test))]
# data_test[, grepl("_arm", names(data_test))]
# data_test[, grepl("_forearm", names(data_test))]
# data_test[, grepl("_dumbbell", names(data_test))]

# data_test[, sapply(data_test, is.numeric)]
# remove columns with mostly NAs
# remove columns with mostly blanks
x <- (colMeans(is.na(data_train))) * 100
blanks <- names(which(x > 90))

#belt <- data_test[, grepl("_belt", names(data_test)) & sapply(data_test, is.numeric)]
#arm <- data_test[, grepl("_arm", names(data_test)) & sapply(data_test, is.numeric)]
#forearm <- data_test[, grepl("_forearm", names(data_test)) & sapply(data_test, is.numeric)]
#dumbbell <- data_test[, grepl("_dumbbell", names(data_test)) & sapply(data_test, is.numeric)]

# find correlated predictors

M <- abs(cor(df[,8:59])) # ignore first 7 and last column
diag(M) <- 0
which(M > 0.8, arr.ind = T)


# pca analysis
pca_df <- prcomp(df[,8:59], scale = TRUE)
var_explained <- pca_df$sdev^2/sum(pca_df$sdev^2)
# scree plot
https://www.statology.org/scree-plot-r/
barplot(var_explained[1:25], xlab="PCA", ylab = "variance explained")
var_explained[1:25] # 25 Variables to get over 95% variance explained
pca_df_cols <- rownames(pca_df$rotation)[1:25] #list of columns

df[1, names(df) %in% pca_df_cols]


010caretpackage.pdf
fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE)

df <- data_train[ , !(names(data_train) %in% blanks)]

# data doesn't work with glm
# try parallel?
# enable parallel for rf? <- 
# try pre-process with pca to remove extra columns
# or try stepwise variable selection?
# which one removes more variables while maintain accuracy.
df2 <- cbind(df[, names(df) %in% pca_df_cols], factor(df[,60]))

colnames(df2) <- c(pca_df_cols, "classe")
rf <- randomForest(classe ~ ., data = df2)

p1 <- predict(rf, data_test)
https://www.guru99.com/r-random-forest-tutorial.html
https://www.listendata.com/2014/11/random-forest-with-r.html
https://www.projectpro.io/recipes/perform-random-forest-r#mcetoc_1g5vh1b90e

# why used random forest
# glm can only use 2-class outcomes
# didn't use caret package because of performance.
https://medium.com/@aravanshad/gradient-boosting-versus-random-forest-cfa3fa8f0d80


<- 
# find any outliers

# assume normal distributions

# preprocess if not normal

# pca analysis, assume most variables are quantitative (week2)
  https://cmdlinetips.com/2019/04/introduction-to-pca-with-r-using-prcomp/
  http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
# predict with regression multiple covariates (week2)

# cross-validate, with validation set




