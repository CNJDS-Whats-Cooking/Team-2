###################################################################################
# Create Machine Learning Models
# 1. Find best features
# 2. Create Training and Test data sets
# 3. Build ML Models 
# 4. Validate accuracy
# 5. Create Submission file
##################################################################################

# Load the required packages (if packages are not available, install them first)
for (package in c('tm', 'caret', 'Boruta', 'e1071', 'class',
                  'party', 'randomForest', 'xgbmodel')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Clean the environment
rm(list=ls())
dev.off()

# Declare all variables
file.path <- "C://Users//Linesh//Documents//Linesh//General//DS Meetup//Kaggle//Cooking//Input Files//"
saved.obj <- "C://Users//Linesh//Documents//Linesh//General//DS Meetup//Kaggle//Cooking//R-Objects//"

load(file=paste0(saved.obj, "dtm-ingredients-spr-R-Object"))
load(file=paste0(saved.obj, "train-R-Object"))
load(file=paste0(saved.obj, "test-R-Object"))

#library(tm)
df.cuisines <- as.data.frame(as.matrix(dtm.ingredients.spr))  
#save(dtm.ingredients.spr, file=paste0(saved.obj, "dtm-ingredients-spr-R-Object"))
#save(tdm, file=paste0(saved.obj, "tdm-R-Object"))
rm(dtm.ingredients.spr)
#rm(tdm)

#df.cuisines <- sign(df.cuisines) 
traincleaned <- df.cuisines[1:39774,]    
testcleaned <- df.cuisines[39775:49718,]  
save(df.cuisines, file=paste0(saved.obj, "df-cuisines-R-Object"))
rm(df.cuisines)

traincleaned$cuisine <- train.dat$cuisine    
traincleaned$cuisine <- as.factor(traincleaned$cuisine) 
rm(train.dat)

save(traincleaned, file=paste0(saved.obj, "traincleaned-R-Object"))
save(testcleaned, file=paste0(saved.obj, "testcleaned-R-Object"))

# Find best feature set using Boruta package (internally uses randomForest technique)
#library(Boruta)
set.seed(123)
boruta.train <- Boruta(cuisine ~., data=traincleaned, doTrace=2, maxRuns=11)
print(boruta.train)
# > print(boruta.train)
# Boruta performed 10 iterations in 5.386475 days.
# No attributes deemed important.
# No attributes deemed unimportant.
# 1739 tentative attributes left: `(10`, `(14.5`, `(powder)`, `33%`, `40%` and 1734
# more.

dev.off()
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

################# DATA PARTITION - begin #########################################

#library(caret)

# considering response variable as strata
data_part <- createDataPartition(y = traincleaned$cuisine, 
                                 p = 0.7, list = F)
train.samp.1 <- traincleaned[data_part,] # 70% here
train.samp.2 <- traincleaned[-data_part,] # 30% data goes here

train.data <- train.samp.1[,1:1739]
train.label <- train.samp.1[,1740]
test.data <- train.samp.2[,1:1739]
test.label <- train.samp.2[,1740]

table(train.samp.1$cuisine) # both training samples have all 20 cuisines
# > table(train.samp.1$cuisine) # both training samples have all 20 cuisines
# 
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 327          563         1083         1872          529         1853          823 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 2103          467         5487          369          997          581         4507 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 575          343         3024          693         1078          578 

table(train.samp.2$cuisine) # both training samples have all 20 cuisines
# > table(train.samp.2$cuisine) # both training samples have all 20 cuisines
# 
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 140          241          463          801          226          793          352 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 900          200         2351          157          426          249         1931 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 246          146         1296          296          461          247 
# > 

# number of rows of data that have missing values
sum(!complete.cases(train.samp.1))
#   > sum(!complete.cases(train.samp.1))
# [1] 0

sum(!complete.cases(train.samp.2))
# > sum(!complete.cases(train.samp.2))
# [1] 0
################# DATA PARTITION - end ###########################################


################# MEMORY SETTING - begin #########################################
memory.size() # shows the current size of memory
memory.limit() # shows the current limit of memory
memory.limit(16384)  # set the current limit to 16 GB if Windows 64-bit
################# MEMORY SETTING - end ###########################################


################# LINEAR MODEL - BEGIN ###########################################

# lm is used to fit linear models. It can be used to carry out regression, 
# single stratum analysis of variance and analysis of covariance (although aov may 
# provide a more convenient interface for these).
train <- train.samp.1
train$cuisine <- as.numeric(train.samp.1$cuisine)-1
#train$cuisine <- as.factor(train$cuisine)  # lm fails with levels

linear.pred <- lm(cuisine ~ ., data = train)

summary(linear.pred)
# ...
# wrapper       -1.621e+00  4.393e-01  -3.690 0.000224 ***
# xanthan               NA         NA      NA       NA    
# yakisoba      -4.239e+00  1.436e+00  -2.952 0.003159 ** 
# yam            5.889e-01  6.063e-01   0.971 0.331423    
# yardlong      -2.982e+00  9.486e-01  -3.144 0.001669 ** 
# yeast         -1.572e+00  3.100e-01  -5.071 3.98e-07 ***
# yellow         1.416e-01  1.134e-01   1.249 0.211691    
# yellowfin      2.970e+00  2.229e+00   1.333 0.182620    
# yoghurt       -1.176e+00  2.927e-01  -4.019 5.85e-05 ***
# yogurt        -1.073e+00  2.563e-01  -4.186 2.85e-05 ***
# yolk          -4.710e-01  1.626e-01  -2.896 0.003781 ** 
# york           3.193e-01  1.648e+00   0.194 0.846354    
# young         -1.941e+00  2.471e+00  -0.786 0.432104    
# yucca         -5.742e+00  2.156e+00  -2.663 0.007751 ** 
# ...
# Residual standard error: 3.719 on 26161 degrees of freedom
# Multiple R-squared:  0.4552,  Adjusted R-squared:   0.42 
# F-statistic: 12.94 on 1690 and 26161 DF,  p-value: < 2.2e-16

# Predict Output
predict.lm = predict(linear.pred, test.data) 
# Warning message:
#   In predict.lm(linear.pred, test.data) :
#   prediction from a rank-deficient fit may be misleading

summary(predict.lm)
# > summary(predict.lm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -7.129   8.059   9.705   9.914  11.860  31.120 

save(linear.pred, file=paste0(saved.obj, "linear-pred-R-Object"))
rm(linear.pred)
save(predict.lm, file=paste0(saved.obj, "predict-lm-R-Object"))
rm(predict.lm)

################# LINEAR MODEL - BEGIN ###########################################


################# SVM MODEL - BEGIN ##############################################

# svm is used to train a support vector machine. It can be used to carry out general
# regression and classification (of nu and epsilon-type), as well as 
# density-estimation. A formula interface is provided.

#library(e1071)

train <- train.samp.1
#train$cuisine <- as.numeric(train.samp.1$cuisine)-1
train$cuisine <- as.factor(train$cuisine)

svm.pred <-svm(cuisine ~ ., data = train)
# Warning message:
#   In svm.default(x, y, scale = scale, ..., na.action = na.action) :
#   Variable(s) 'artisan' and 'kim' and 'sunda' constant. Cannot scale data.

summary(svm.pred)
# > summary(svm.pred)
# 
# Call:
#   svm(formula = cuisine ~ ., data = train)
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.0005750431 
# 
# Number of Support Vectors:  23246
# 
# ( 327 563 1028 1459 529 1853 823 1640 467 3694 369 996 581 3033 573 343 2747 693 
#   950 578 )
# 
# 
# Number of Classes:  20 
# 
# Levels: 
#   brazilian british cajun_creole chinese filipino french greek indian irish 
#   italian jamaican japanese korean mexican moroccan russian southern_us spanish 
#   thai vietnamese


# #Predict Output 
predict.svm <- predict(svm.pred,test.data)

table(test.label)
# test.label
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 140          241          463          801          226          793          352 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 900          200         2351          157          426          249         1931 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 246          146         1296          296          461          247 

table(predict.svm)
# predict.svm
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 0            0          261         1194            0           61           14 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 838            0         4163            0          156            5         2082 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 59            0         2659            0          430            0 

confusionMatrix(train.samp.2$cuisine, predict.svm)
# Overall Statistics
# 
# Accuracy : 0.5779         
# 95% CI : (0.569, 0.5868)
# No Information Rate : 0.3492         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.5111         
# Mcnemar's Test P-Value : NA             

# #Predict Output for the given test data file
predict.svm <- predict(svm.pred,testcleaned)

submission_svm <- data.frame(id=test.dat$id, cuisine=predict.svm)    
write.csv(submission_svm,file=paste0(file.path, "submission_svm.csv"),row.names=F)

# Kaggle Score/Ranking
# Score: 0.56698
# Rank: 1,254 / 1,388 (Bottom 10%)

save(svm.pred, file=paste0(saved.obj, "svm-pred-R-Object"))
rm(svm.pred)
save(predict.svm, file=paste0(saved.obj, "predict-svm-R-Object"))
rm(predict.svm)
save(submission_svm, file=paste0(saved.obj, "submission-svm-R-Object"))
rm(submission_svm)

################# SVM MODEL - END ################################################


################# NAIVE BAYES - BEGIN ############################################

# Computes the conditional a-posterior probabilities of a categorical class 
# variable given independent predictor variables using the Bayes rule.

train <- train.samp.1
#train$cuisine <- as.numeric(train.samp.1$cuisine)-1
train$cuisine <- as.factor(train$cuisine)

nb.pred <-naiveBayes(cuisine ~ ., data = train)
summary(nb.pred)
# > summary(nb.pred)
#          Length Class  Mode     
# apriori   20   table  numeric  
# tables  1739   -none- list     
# levels    20   -none- character
# call       4   -none- call     

#Predict Output 
predict.nb= predict(nb.pred, test.data)

table(test.label)
# test.label
# brazilian      british cajun_creole      chinese     filipino       french        greek       indian 
# 140          241          463          801          226          793          352          900 
# irish      italian     jamaican     japanese       korean      mexican     moroccan      russian 
# 200         2351          157          426          249         1931          246          146 
# southern_us      spanish         thai   vietnamese 
# 1296          296          461          247 

table(predict.nb)
# brazilian      british cajun_creole      chinese     filipino       french        greek       indian 
# 4296            7            0            0          170            0            4            1 
# irish      italian     jamaican     japanese       korean      mexican     moroccan      russian 
# 59            0         3445            0         2080            0          706          961 
# southern_us      spanish         thai   vietnamese 
# 0            0            2          191 

confusionMatrix(train.samp.2$cuisine, predict.nb)
# 5% accurary
# Overall Statistics
# 
# Accuracy : 0.0508         
# 95% CI : (0.047, 0.0549)
# No Information Rate : 0.3603         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.0368         
# Mcnemar's Test P-Value : NA             

# Build submission file for train.samp.2 - not worth trying for the given test data
submission_nb <- data.frame(id=test.data$id,pred.cuisine=predict.nb)    
write.csv(submission_nb,file=paste0(file.path, "submission_nb.csv"),row.names=F)

save(nb.pred, file=paste0(saved.obj, "nb-pred-R-Object"))
rm(nb.pred)
save(predict.nb, file=paste0(saved.obj, "predict-nb-R-Object"))
rm(predict.nb)
save(submission_nb, file=paste0(saved.obj, "submission-nb-R-Object"))
rm(submission_nb)

# with laplace smoothing
nb.pred <-naiveBayes(cuisine ~ ., data = train, laplace = 5)
summary(nb.pred)
# > summary(nb.pred)
# Length Class  Mode     
# apriori   20   table  numeric  
# tables  1739   -none- list     
# levels    20   -none- character
# call       4   -none- call     

#Predict Output 
predict.nb= predict(nb.pred, test.data)

table(test.label)
# test.label
# brazilian      british cajun_creole      chinese     filipino       french        greek       indian 
# 140          241          463          801          226          793          352          900 
# irish      italian     jamaican     japanese       korean      mexican     moroccan      russian 
# 200         2351          157          426          249         1931          246          146 
# southern_us      spanish         thai   vietnamese 
# 1296          296          461          247 

table(predict.nb)
# brazilian      british cajun_creole      chinese     filipino       french        greek       indian 
# 4296            7            0            0          170            0            4            1 
# irish      italian     jamaican     japanese       korean      mexican     moroccan      russian 
# 59            0         3445            0         2080            0          706          961 
# southern_us      spanish         thai   vietnamese 
# 0            0            2          191 

confusionMatrix(train.samp.2$cuisine, predict.nb)
# 5% accurary
# Overall Statistics
# 
# Accuracy : 0.0508         
# 95% CI : (0.047, 0.0549)
# No Information Rate : 0.3603         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.0368         
# Mcnemar's Test P-Value : NA             

# Build submission file for train.samp.2 - not worth trying for the given test data
submission_nb <- data.frame(id=test.dat$id, cuisine=predict.nb)    
write.csv(submission_nb,file=paste0(file.path, "submission_nb.csv"),row.names=F)

save(nb.pred, file=paste0(saved.obj, "nb-pred-R-Object"))
rm(nb.pred)
save(predict.nb, file=paste0(saved.obj, "predict-nb-R-Object"))
rm(predict.nb)
save(submission_nb, file=paste0(saved.obj, "submission-nb-R-Object"))
rm(submission_nb)

################# NAIVE BAYES - END ##############################################


################# KNN - begin #####################################################

#library(class)
knn_pred <- knn(train = train.data, test = test.data,cl = train.label, k=175)

table(test.label)
# > table(test.label)
# test.label
# brazilian      british cajun_creole      chinese     filipino       french        greek       indian 
# 140          241          463          801          226          793          352          900 
# irish      italian     jamaican     japanese       korean      mexican     moroccan      russian 
# 200         2351          157          426          249         1931          246          146 
# southern_us      spanish         thai   vietnamese 
# 1296          296          461          247 

summary(knn_pred)
# > summary(knn_pred)
# brazilian      british cajun_creole      chinese     filipino       french        greek       indian 
# 70           23          172          734          124          544           25          712 
# irish      italian     jamaican     japanese       korean      mexican     moroccan      russian 
# 15         4088           14          374          159         2509           48            1 
# southern_us      spanish         thai   vietnamese 
# 2045            5          210           50 

#library(caret)
knn_pred_conf_mat <- confusionMatrix(test.label, knn_pred)
# 57% accurary
# Overall Statistics
# 
# Accuracy : 0.5741         
# 95% CI : (0.5651, 0.583)
# No Information Rate : 0.3429         
# P-Value [Acc > NIR] : < 2.2e-16      
# 
# Kappa : 0.5074         
# Mcnemar's Test P-Value : NA             

# Build submission file for train.samp.2 - not worth trying for the given test data
submission_knn <- data.frame(id=train.samp.2$id, cuisine=knn_pred)    
write.csv(submission_knn,file=paste0(file.path, "submission_knn.csv"),row.names=F)

save(knn_pred, file=paste0(saved.obj, "knn-pred-R-Object"))
rm(knn_pred)
save(knn_pred_conf_mat, file=paste0(saved.obj, "knn_pred_conf_mat-R-Object"))
rm(knn_pred_conf_mat)
save(submission_knn, file=paste0(saved.obj, "submission_knn-R-Object"))
rm(submission_knn)

################# KNN - end #####################################################


################# CTREE - BEGIN ##################################################

#library(party)

ctreeingredient <- ctree(train.samp.1$cuisine ~ ., data = train.samp.1) 

ctreeingredient
# > ctreeingredient
# 
# Conditional inference tree with 19 terminal nodes
# 
# Response:  train.samp.1$cuisine 
# Inputs:  (10, (14.5, (powder), 33%, 40%, 95%, açai, acai, accent, achiot, acid, 
#                acke, acorn, act, activ, adobo, adzuki, agar, agav, age, ahi, aioli, 
#                ajwain, aka, albacor, ale, aleppo, alfalfa, alfredo, all, allspic, 
#                almond, amaretti, amaretto, amchur, american, amino, anaheim, ancho, 
#                anchovi, andouill, anejo, angel, anglais, angostura, anis, anjou, 
#                annatto, appl, appl           

# This plot may be too huge and time consuming; it may hang the system
# plot(ctreeingredient)
# dev.off()

#predictedctree<-predict( ctreeingredient , newdata = train.samp.2,list(level=.99)) 
predictedctree<-predict( ctreeingredient , newdata = train.samp.2) 

table(predictedctree)
# predictedctree
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 0            0           60         1548            0           50          231 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 1447            0         2609           15            0            0         1261 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 0            0         4358            0          343            0 

confusionMatrix(train.samp.2$cuisine, predictedctree)
# 46% accuracy
# Overall Statistics
# 
# Accuracy : 0.4598          
# 95% CI : (0.4508, 0.4688)
# No Information Rate : 0.3655          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.3861          
# Mcnemar's Test P-Value : NA              

# Build submission file for train.samp.2 - not worth trying for the given test data
submmsion_ctree <- data.frame(id=train.samp.2$id,pred.cuisine=predictedctree)    
nrow(submmsion_ctree)
# [1] 11922

write.csv(submmsion_ctree,file=paste0(file.path, "submmsion_ctree.csv"),row.names=F)

save(ctreeingredient, file=paste0(saved.obj, "ctreeingredient-R-Object"))
rm(ctreeingredient)
save(predictedctree, file=paste0(saved.obj, "predictedctree-R-Object"))
rm(predictedctree)
save(submmsion_ctree, file=paste0(saved.obj, "submmsion_ctree-R-Object"))
rm(submmsion_ctree)

#ctreeingredient <- ctree(train.samp.1$cuisine ~ ., 
#                         data = train.samp.1, 
#                         control = 
#                           ctree_control(maxsurrogate =8,mtry = 100,maxdepth=8000))    
#plot(ctreeingredient )   
#predictedctree<-predict( ctreeingredient , newdata = train.samp.2,list(level=.99)) 
# 41% accuracy

################# CTREE - end #####################################################


################# RANDOM FOREST - begin ###########################################

#library(randomForest)

rf.fit <- randomForest(cuisine ~ ., 
                       data = train[ , !(names(train) %in% 
                                           c("(10", "(14.5", "(powder)", "33%",
                                             "40%", "95%", "bacon,", "beans,",
                                             "bell®", "bertolli®", "boneless,",
                                             "bread,", "butter!®", "can't",
                                             "capers,", "cheese,", "chestnuts,",
                                             "chilies,", "clams,"))], 
                       ntree=2) 

summary(rf.fit)
predict.rf <- predict(rf.fit , newdata = test.data) 

table(test.label)

table(predict.rf)

confusionMatrix(train.samp.2$cuisine, predict.rf)

submission_rf <- data.frame(id=train.samp.2$id,pred.cuisine=predict.rf)    
nrow(submission_rf)
# [1] 11922

write.csv(submission_rf,file=paste0(file.path, "submission_rf.csv"),row.names=F)

save(rf.fit, file=paste0(saved.obj, "rf-fit-R-Object"))
rm(rf.fit)
save(pred.rf, file=paste0(saved.obj, "pred-rf-R-Object"))
rm(pred.rf)
save(submission_rf, file=paste0(saved.obj, "submission-rf-R-Object"))
rm(submission_rf)

################# RANDOM FOREST - end ###########################################


################# CONDITIONAL INFERENCE RANDOM FOREST - begin ###################

# An implementation of the random forest and bagging ensemble algorithms 
# utilizing conditional inference trees as base learners.

train <- train.samp.1
#train$cuisine <- as.numeric(train.samp.1$cuisine)-1
train$cuisine <- as.factor(train$cuisine)  # lm fails with levels

#library(party)
control <- cforest_control(teststat = "max",
                           testtype = "Teststatistic",
                           mincriterion = qnorm(0.9),
                           savesplitstats = FALSE,
                           ntree = 100, mtry = NULL, replace = TRUE,
                           fraction = 0.632, trace = TRUE)

# Took 48 hours to complete
fit.cf <- cforest(cuisine ~ ., data=train, controls=control)

summary(fit.cf)
# > summary(fit.cf)
# Length        Class         Mode 
# 1 RandomForest           S4

predict.cf <- predict(fit.cf , newdata = test.data) 

table(test.label)
# test.label
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 140          241          463          801          226          793          352 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 900          200         2351          157          426          249         1931 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 246          146         1296          296          461          247 

table(predict.cf)
# predict.cf
# brazilian      british cajun_creole      chinese     filipino       french        greek 
# 0            0            0         1380            0            0          166 
# indian        irish      italian     jamaican     japanese       korean      mexican 
# 585            0         8595            0            0            0          807 
# moroccan      russian  southern_us      spanish         thai   vietnamese 
# 0            0            0            0          389            0 

confusionMatrix(train.samp.2$cuisine, predict.cf)
# 37% accurary
# Overall Statistics
# 
# Accuracy : 0.372           
# 95% CI : (0.3633, 0.3807)
# No Information Rate : 0.7209          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.2467          
# Mcnemar's Test P-Value : NA              

# Build submission file for train.samp.2 - not worth trying for the given test data
submission_cf <- data.frame(id=train.samp.2$id,pred.cuisine=predict.cf)    
nrow(submission_cf)
# [1] 11922

write.csv(submission_cf,file=paste0(file.path, "submission_cf.csv"),row.names=F)

save(fit.cf, file=paste0(saved.obj, "cf-fit-R-Object"))
rm(fit.cf)
save(predict.cf, file=paste0(saved.obj, "pred-cf-R-Object"))
rm(predict.cf)
save(submission_cf, file=paste0(saved.obj, "submission-cf-R-Object"))
rm(submission_cf)

################# CONDITIONAL INFERENCE RANDOM  FOREST - end ####################


################ XGBOOST - begin ###############################################

#library(xgboost)

dtrain <- xgb.DMatrix(Matrix(
  data.matrix(train.samp.1[,!colnames(train.samp.1) %in% c('cuisine')])), 
  label = as.numeric(train.samp.1$cuisine)-1)
save(dtrain, file=paste0(saved.obj, "dtrain-R-Object"))

#advanced data set preparation
dtest <- xgb.DMatrix(Matrix(
  data.matrix(train.samp.2[,!colnames(train.samp.2) %in% c('cuisine')]))) 
watchlist <- list(train = dtrain, test = dtest)
save(dtest, file=paste0(saved.obj, "dtest-R-Object"))

#train multiclass model using softmax
train.samp.2$cuisine_num <- NULL
train.samp.2$cuisine_pred_num <- NULL

# Try different parameter for building xgbmodel
# Best combination will be used to build the final xgbmodel
conf.mat.overall <- 0.0
for (eta in c(0.2, 0.1, 0.05, 0.01) )
{
  for (colsample_bytree in c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
  {
    for(subsample in c(0.5, 0.6, 0.7, 0.8, 0.9, 1))
    {
      set.seed(1429)
      xgbmodel <- xgboost(data = dtrain, 
                          max.depth = 20, 
                          eta = eta, 
                          subsample = subsample, 
                          colsample_bytree = colsample_bytree,
                          nround = 1, 
                          objective = "multi:softmax", 
                          num_class = 20, 
                          verbose = 1, 
                          #early.stop.round = 5,
                          maximize = TRUE,
                          watchlist = watchlist)
      
      train.samp.2$cuisine_num <- NULL
      train.samp.2$cuisine_pred_num <- NULL
      
      xgbmodel.predict <- predict(xgbmodel, 
                                  newdata = data.matrix(train.samp.2[, !colnames(train.samp.2) 
                                                                     %in% c('cuisine')]))
      xgbmodel.predict.text <- levels(train.samp.1$cuisine)[xgbmodel.predict + 1]
      
      train.samp.2$cuisine_num <- as.numeric(train.samp.2$cuisine)-1
      train.samp.2$cuisine_pred_num <- xgbmodel.predict
      conf.mat <- confusionMatrix(train.samp.2$cuisine_num, train.samp.2$cuisine_pred_num)
      
      if (conf.mat$overall[[1]] > conf.mat.overall) {
        conf.mat.overall <- conf.mat$overall[[1]]
        final.eta <- eta
        final.colsample.bytree <- colsample_bytree
        final.subsample <- subsample
        final <- data.frame(cuisine_num=as.numeric(train.samp.2$cuisine)-1,
                            cuisine_pred_num=xgbmodel.predict)
        final.conf.mat <- conf.mat
      }
    }
  }
}
conf.mat.overall
final.eta
final.colsample.bytree
final.subsample

# final.eta = 0.1, final.subsample = 1, final.colsample.bytree = 1
train.samp.2$cuisine_num <- NULL
train.samp.2$cuisine_pred_num <- NULL
set.seed(1500)
xgbmodel <- xgboost(data = dtrain, 
                    max.depth = 20, 
                    eta = 0.1, 
                    subsample = 1, 
                    colsample_bytree = 1,
                    nround = 650, 
                    objective = "multi:softmax", 
                    num_class = 20, 
                    verbose = 1, 
                    watchlist = watchlist)

xgbmodel.predict <- predict(xgbmodel, 
                            newdata = data.matrix(train.samp.2[, !colnames(train.samp.2) 
                                                               %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train.samp.1$cuisine)[xgbmodel.predict + 1]

train.samp.2$cuisine_num <- as.numeric(train.samp.2$cuisine)-1
train.samp.2$cuisine_pred_num <- xgbmodel.predict
conf.mat <- confusionMatrix(train.samp.2$cuisine_num, train.samp.2$cuisine_pred_num)
# 0.795 accuracy
conf.mat$overall

# test.dat prediction (for train.samp.2 ONLY - not for Kaggle submission)
#predict 1
xgbmodel.predict.tst <- predict(xgbmodel, 
                                newdata = dtest)
xgbmodel.predict.text.tst <- levels(train.samp.1$cuisine)[xgbmodel.predict.tst + 1]
table(xgbmodel.predict.text.tst)
data.test <- data.frame(ID=test.dat$id, CUISINE=xgbmodel.predict.text.tst)


# Build Model and File for Kaggle submission test data
dtrain <- xgb.DMatrix(Matrix(
  data.matrix(traincleaned[,!colnames(train.samp.1) %in% c('cuisine')])), 
  label = as.numeric(traincleaned$cuisine)-1)

set.seed(1500)
xgbmodel <- xgboost(data = dtrain, 
                    max.depth = 20, 
                    eta = 0.1, 
                    subsample = 1, 
                    colsample_bytree = 1,
                    nround = 650, 
                    objective = "multi:softmax", 
                    num_class = 20, 
                    verbose = 1)

xgbmodel.predict <- predict(xgbmodel, 
                            newdata = data.matrix(testcleaned[, !colnames(testcleaned) 
                                                              %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train.samp.1$cuisine)[xgbmodel.predict + 1]

submission_xgbmodel <- data.frame(ID=test.dat$id, CUISINE=xgbmodel.predict.text)
write.csv(submission_xgbmodel, file=paste0(file.path, "submission_xgbmodel.csv"),
          row.names=F)

# Kaggle Score
# Score: 0.79938
# Rank: 223 / 1388 (Top 16%)

################### XGBOOST - end #################################################