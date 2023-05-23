# Name: SIAH YAO LIANG , NEAW AIK KA , TAI RUI XIAN
# JOB: DATA SCIENTIST 
# NAME OF PROGRAM: Capstone_STROKE.R
# DESCRIPTION :PATIENT STORKE PREDICTION 
# DATE FIRST WRITTEN WRITTEN WED ,4 APRIL 2023
# Date last updated:
# project folder : D:/Crystal/Capstone_project

#----------------------------------------- DATA IMPORT -------------------------------------------

library(readr)
library(dplyr)
library(psych)
library(lattice)
library(pastecs)
library(DataExplorer)
library(ggplot2)
library(ggridges)                      # Install & load scales package
library(scales)
setwd("/Users/byron/Library/CloudStorage/OneDrive-AsiaPacificUniversity/Y2S1/Crystal System/Capstone Project/")
stroke_ds<- read.csv("healthcare-dataset-stroke-data.csv",header=TRUE)

#andrew
# setwd('D:/Crystal/Capstone_project')
# stroke_ds<- read.csv("healthcare-dataset-stroke-data.csv",header=TRUE)

#ruis
#setwd("C:/Users/user/Downloads")
#stroke_ds<- read.csv("healthcare-dataset-stroke-data.csv",header=TRUE)


#----------------------------------------------------------------------------------------------
#-------------------------------------- DATA EXPLORATION -------------------------------------------

#View(stroke_ds)
str(stroke_ds) #view dataset structure 
dim(stroke_ds)

stroke_ds$bmi<-as.numeric(stroke_ds$bmi) #convert bmi from chr to num 
stroke_ds$age<-as.integer(stroke_ds$age) #convert age from num to integer 

#CHECK IF HAVE MISSING DATA
stroke_ds %>% 
  filter(!complete.cases(.)) #%>%   View()#BMI has missing 

unique(stroke_ds$gender) #no missing
mean(stroke_ds$age) #no missing
unique(stroke_ds$hypertension) #no missing
unique(stroke_ds$heart_disease) #no missing
unique(stroke_ds$ever_married) #no missing
unique(stroke_ds$work_type) #no missing 
unique(stroke_ds$Residence_type) #no missing
mean(stroke_ds$avg_glucose_level) #no missing
unique(stroke_ds$smoking_status) #no missing but have unknown
unique(stroke_ds$stroke) #no missing 
mean(stroke_ds$bmi) #have missing 

#HANDLE BMI MISSING VALUE BY IMPUTING MEAN OF IT 
stroke_ds$bmi[is.na(stroke_ds$bmi)]<- mean(stroke_ds$bmi,na.rm = TRUE)

# since there is only one "Other" in gender column so we will remove it 
stroke_ds<- stroke_ds %>% filter(!gender=="Other") 

plot_missing(stroke_ds)# check missing data in all columns

#----------------------------------------------------------------------------------------------
#-------------------------------- GENERAL OVERVIEW ------------------------------------
#install.packages("ggridges")

describe(stroke_ds)#check status of data
stat.desc(stroke_ds)
summary(stroke_ds)
plot_str(stroke_ds)#visualize for data attributes

#histogram of ID
ggplot(stroke_ds, aes(x = id)) + geom_histogram(fill = "cadetblue") + 
  theme_minimal() + labs(title = "Histogram of ID") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))
#Gender distribution bar chart
ggplot(stroke_ds, aes(x = gender, fill = gender)) + geom_bar() + 
  theme_minimal() + labs(title = "Gender bar chart") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#stroke ,gender
ggplot(stroke_ds, aes(x = stroke, fill = gender)) + geom_bar(position = "dodge") + 
  theme_minimal() + labs(title = "Stroke Status and gender Bar chart") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#Age distribution histogram
ggplot(stroke_ds, aes(x = age)) + geom_histogram(fill="cadetblue") + 
  theme_minimal() + labs(title = "Histogram of Age") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))
#Age outlier detection
ggplot(stroke_ds, aes(y = age)) + geom_boxplot(fill = "cadetblue") + theme_minimal() + 
  labs(title = "Boxplot of Age") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))
#Stroke vs Age
ggplot(stroke_ds, aes(x=age)) + geom_density() + facet_wrap(~stroke)+
  theme_light() + labs(title = "Density of Age and stroke status") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#Hypertension bar chart
stroke_ds$hypertension<-factor(stroke_ds$hypertension,levels = c(0,1),labels=c("No","Yes"))
ggplot(stroke_ds, aes(x = hypertension,fill=hypertension)) + geom_bar() + theme_minimal() + 
  labs(title = "Hypertension Bar chart") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))
       
ggplot(stroke_ds,aes(hypertension,age)) + 
  geom_boxplot()+theme_minimal() + 
  labs(title = "Hypertension Boxplot for age") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold")) #boxplot

ggplot(stroke_ds,aes(age,fill=hypertension)) + geom_density(alpha=0.5) + 
  facet_wrap(~stroke) + theme_light() + 
  labs(title = "Hypertension Density for age") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))#density

stroke_ds$hypertension = factor(stroke_ds$hypertension,
                                levels = c('No', 'Yes'),
                                labels = c('0', '1'))
#Heart disease bar chart
stroke_ds$heart_disease<-factor(stroke_ds$heart_disease,levels = c(0,1),labels=c("No","Yes"))
ggplot(stroke_ds, aes(x = heart_disease,fill = heart_disease)) + geom_bar() + theme_minimal() + 
  labs(title = "Heart disease Bar chart") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

ggplot(stroke_ds, aes(x = heart_disease, fill = heart_disease)) + geom_bar(position = 'dodge') + theme_minimal() +  #heart disease vs stroke
  labs(title = "Heart disease vs stroke") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

stroke_ds$heart_disease = factor(stroke_ds$heart_disease,
                                 levels = c('No', 'Yes'),
                                 labels = c('0', '1'))
#marital status bar chart
ggplot(stroke_ds, aes(x = ever_married, fill = as.factor(ever_married))) + geom_bar() + theme_minimal() + 
  labs(title = "Marital Status Bar chart") + #marital status vs stroke
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#Work type bar chart
ggplot(stroke_ds, aes(x = work_type, fill = work_type)) + geom_bar() + theme_minimal() +  #work type vs stroke
  labs(title = "Work type Bar chart") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#Residence type bar chart
ggplot(stroke_ds, aes(x = Residence_type, fill = Residence_type)) + geom_bar() + 
  theme_minimal() + labs(title = "Residence Type Bar chart") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#Glucose level distribution histogram
ggplot(stroke_ds, aes(x = avg_glucose_level)) + geom_histogram(fill = "cadetblue", binwidth = 10) + 
  theme_minimal() + labs(title = "Average Glucose Level Histogram") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

ggplot(stroke_ds,aes(avg_glucose_level, colour ="red")) + 
  geom_density(alpha=0.5) + theme_minimal() + labs(title = "Average Glucose Level Density") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold")) #density glocose level vs stroke

#BMI distribution histogram
ggplot(stroke_ds, aes(x = bmi)) + geom_histogram(fill="cadetblue") + theme_minimal() + 
  labs(title = "BMI Histogram") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

ggplot(stroke_ds,aes(bmi, group=stroke, colour = factor(stroke), fill = factor(stroke))) + 
  geom_density(alpha=0.5) + theme_minimal() + labs(title = "BMI Density graph") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#Smoking status bar chart
ggplot(stroke_ds, aes(x = smoking_status, y=(..count..)/sum(..count..),fill = smoking_status)) + geom_bar() + 
  theme_minimal() + labs(title = "Smoking Status Bar chart") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

ggplot(stroke_ds, aes(x = smoking_status, fill = smoking_status)) + geom_bar() + 
  theme_minimal() + labs(title = "Smoking Status Bar chart") + 
  facet_wrap(~Residence_type, ncol = 1)+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

ggplot(stroke_ds, aes(x = smoking_status, fill = factor(stroke))) + geom_bar(position = 'dodge') + 
  theme_minimal() + labs(title = "Smoking Status Bar chart") + 
  #facet_wrap(~Residence_type, ncol = 1)+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#Stroke status bar chart
stroke_ds$stroke<-factor(stroke_ds$stroke,levels = c(0,1),labels=c("No","Yes"))

#distribution of stroke
ggplot(stroke_ds, aes(x = stroke, fill = stroke)) + geom_bar(position = "dodge") + 
  theme_minimal() + 
  labs(title = "Stroke Status Bar chart") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))


stroke_ds$stroke = factor(stroke_ds$stroke,
                                 levels = c('No', 'Yes'),
                                 labels = c('0', '1'))

# heatmap
library(reshape2)
corrMap <- round(cor(stroke_ds %>% select_if(is.numeric)), 3)
corrMap[lower.tri(corrMap)] <- NA
#NA prompt is right, no worries
meltedMap <- melt(corrMap)
ggplot(meltedMap, aes(x = Var1, y =Var2, fill=value)) + geom_tile() + geom_text(aes(Var1, Var2, label=value), color="white", size=4) + 
  scale_fill_distiller(palette="Reds") +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))
  
#----------------------------------------------------------------------------------------------
#--------------------------------- DATA TRANSFORMATION ----------------------------------------
#--------------------------------- DATA PRE-PROCESSING ----------------------------------------------
# remove index column
stroke_ds <- dplyr::select(stroke_ds, -id)

#adjust spelling for
#1. work type
#2. smoking status
#3. gender
#4. Residence_type
stroke_ds <- stroke_ds %>% mutate(work_type = ifelse (work_type == 'Govt_job','gov_job',work_type))%>%
                           mutate(work_type = ifelse (work_type == 'Never_worked','never_worked',work_type))%>%
                           mutate(work_type = ifelse (work_type == 'Private','private',work_type))%>%
                           mutate(work_type = ifelse (work_type == 'Self-employed','self_employed',work_type))%>%
                           mutate(gender = ifelse (gender == 'Male','male',gender))%>%
                           mutate(gender = ifelse (gender == 'Female','female',gender))%>%
                           mutate(smoking_status = ifelse (smoking_status == 'Unknown','unknown', smoking_status))%>%
                           mutate(Residence_type = ifelse (Residence_type == 'Unknown','unknown', Residence_type))%>%
                           mutate(Residence_type = ifelse (Residence_type == 'Urban','urban', Residence_type))%>%
                           mutate(Residence_type = ifelse (Residence_type == 'Rural','rural', Residence_type))
# Change the column name
colnames(stroke_ds)[colnames(stroke_ds)=="Residence_type"] <- "residence_type"
  

#Encoding
stroke_ds$ever_married = factor(stroke_ds$ever_married,
                                levels = c('No', 'Yes'),
                                labels = c(0, 1)) 

stroke_ds$gender = factor(stroke_ds$gender,
                                  levels = c('female', 'male'),
                                  labels = c(0, 1))

stroke_ds$smoking_status = factor(stroke_ds$smoking_status,
                          levels = c('unknown', 'never smoked', 'formerly smoked', 'smokes'),
                          labels = c(0, 1, 2, 3))


#----------------------------------------------------------------------------------------------
#----------------------------------- One-Hot Encoding ------------------------------------------
# install.packages("remotes")
library(remotes)
# install.packages("fastDummies")
library(fastDummies)
#install.packages("caret")
library(caret)

# 1. One-hot encoding -work_type
work_type_dummy <- dummyVars(" ~ work_type", data = stroke_ds, sep = "_")
work_type_encoded <- predict(work_type_dummy, stroke_ds)
# Add the encoded columns to original dataset
stroke_ds <- cbind(stroke_ds, work_type_encoded)

# 2. One-hot encoding -residence_type
residence_type_dummy <- dummyVars(" ~ residence_type", data = stroke_ds, sep = "_")
residence_type_encoded <- predict(residence_type_dummy, stroke_ds)
# Add the encoded columns to original dataset
stroke_ds <- cbind(stroke_ds, residence_type_encoded)

#remove work_type and residence_type original raw column
stroke_ds <- dplyr::select(stroke_ds, -work_type)
stroke_ds <- dplyr::select(stroke_ds, -residence_type)

#----------------------------------------------------------------------------------------------
#------------------------------- Categorize + (Label Encoding/One-Hot Encoding) ---------------------------------

#1. Categorize age status
stroke_ds <- stroke_ds %>% mutate(age_status = ifelse(age >=0 & age <= 2, "babies/toodler",
                                       ifelse(age <= 12, "children",
                                       ifelse(age <= 17, "adolescent",
                                       ifelse(age <=30, "young adult",       
                                       ifelse(age <=64, "middle-aged adult",
                                       ifelse(age >64, "older adult", "older adult")))))))
#Label Encoding age status    
stroke_ds$age_status = factor(stroke_ds$age_status,
                       levels = c('babies/toodler', 'children', 'adolescent', 'young adult', 'middle-aged adult', 'older adult'),
                       labels = c(0, 1, 2, 3, 4, 5)) 

#2. Categorize avg_glucose_level_status
stroke_ds <- stroke_ds %>% mutate(avg_glucose_level_status = ifelse(avg_glucose_level <=79, "hypoglycemia",
                                                             ifelse(avg_glucose_level <= 99, "normal level",
                                                             ifelse(avg_glucose_level < 125, "pre diabetic",
                                                             ifelse(avg_glucose_level >=125, "diabetic", "diabetic")))))

#One-Hot Encoding avg_glucose_level_status
stroke_ds$avg_glucose_level_status = factor(stroke_ds$avg_glucose_level_status,
                            levels = c('hypoglycemia', 'normal level', 'pre diabetic', 'diabetic'),
                            labels = c(0, 1, 2, 3))

# bmi encoding----
#1. Categorize bmi_status
stroke_ds <- stroke_ds %>% mutate(bmi_status = ifelse(bmi <18.5, "underweight",
                                                      ifelse(bmi <= 24.9, "normal weight",
                                                             ifelse(bmi <= 29.9, "overweight",
                                                                    ifelse(bmi >= 30, "obese", "obese")))))
#Label Encoding bmi_status    
stroke_ds$bmi_status = factor(stroke_ds$bmi_status,
                              levels = c('underweight', 'normal weight', 'overweight', 'obese'),
                              labels = c(0, 1, 2, 3)) 

#----------------------------------------------------------------------------------------------
#------------------------------- Glucose level outlier detection====
ggplot(stroke_ds, aes(y = avg_glucose_level, fill = "cadetblue")) + geom_boxplot() + theme_minimal() + 
  labs(title = "Average Glucose Level Boxplot") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#Glucose level distribution transformation reciprocal
stroke_ds <- stroke_ds %>% mutate(avg_glucose_level = 1/avg_glucose_level)

#Glucose level distribution histogram
ggplot(stroke_ds, aes(x = avg_glucose_level)) + geom_histogram(bins=30, fill="cadetblue") + 
  theme_minimal() +  labs(title = "Average Glucose Level Histogram") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))
#Glucose level boxplot
ggplot(stroke_ds, aes(y = avg_glucose_level)) + geom_boxplot(fill = "cadetblue") + 
  theme_minimal() +  labs(title = "Average Glucose Level Boxplot") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#----------------------------------------------------------------------------------------------
#-------------------------------- BMI outlier detection====
ggplot(stroke_ds, aes(y = bmi, fill="cadetblue")) + geom_boxplot() + theme_minimal() + 
  labs(title = "BMI Boxplot") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))
quantile(stroke_ds$bmi)# quantile checking
sort(boxplot.stats(stroke_ds$bmi)$out) #outlier value checking
# replace outliers with median
stroke_ds <- stroke_ds %>% 
  mutate(bmi = 
           ifelse(bmi >= quantile(stroke_ds$bmi, probs = 0.75) + 1.5 * IQR(stroke_ds$bmi), 
                  median(stroke_ds$bmi, na.rm = T), 
                  ifelse(bmi <= quantile(stroke_ds$bmi, probs = 0.25) - 1.5 * IQR(stroke_ds$bmi),
                         median(stroke_ds$bmi, na.rm = T), bmi)))

#BMI distribution histogram
ggplot(stroke_ds, aes(x = bmi, fill = gender)) + geom_histogram(binwidth = 1) + theme_minimal() + 
  labs(title = "BMI Histogram")+
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#BMI new boxplot
ggplot(stroke_ds, aes(y = bmi)) + geom_boxplot(fill="cadetblue") + theme_minimal() + 
  labs(title = "BMI Boxplot") +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

#------------------------------------ normalize ====
normalize <- function(x){
  return (x - min(x)) / ((max(x) - min(x)))
}
stroke_ds <- as.data.frame(lapply(stroke_ds, as.numeric))
stroke_ds <- as.data.frame(lapply(stroke_ds[,],normalize))
quantile(stroke_ds$bmi) # new quantile checking

#----------------------------------------------------------------------------------------------
#---------------------------------- SMOTE and splitting====
stroke_ds$stroke = factor(stroke_ds$stroke,
                          levels = c(1,0),
                          labels = c('Yes', 'No'))

library(caTools)
set.seed(123)
split <- sample.split(stroke_ds$stroke, SplitRatio = 0.7)
strokeTrain <- subset(stroke_ds, split==TRUE)
strokeTest <- subset(stroke_ds, split==FALSE)
dim(strokeTrain)
dim(strokeTest)

#-------------------------- Checking if there is any bias in sampling ====
# check proportion of all variable in column
prop.table(table(strokeTrain$stroke))
prop.table(table(strokeTest$stroke))

# install.packages("grid")
library(lattice, grid)
#remotes::install_github("cran/DMwR")
library(DMwR)
#for training dataset
table(strokeTrain$stroke)
strokeTrain <- SMOTE(stroke~., strokeTrain,perc.over = 1600, perc.under = 100)
as.data.frame(table(strokeTrain$stroke))

 #for testing dataset
table(strokeTest$stroke)
strokeTest <- SMOTE(stroke~., strokeTest,perc.over = 1600, perc.under = 100)
as.data.frame(table(strokeTest$stroke))

#---------------------------- Rank Features By Importance ====
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(stroke~., data=stroke_ds, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

#----------------------------------------------------------------------------------------------
# KNN ====
# install.packages('class')
library(class)

strokeTrainknn <- strokeTrain
strokeTestknn <- strokeTest

set.seed(123)
# model tuning
control <- trainControl(method = "repeatedcv",
                        summaryFunction = defaultSummary,
                        number = 25,
                        repeats = 10,
                        search = "grid")
grid <- data.frame(k = seq(1, 24))
knn_tune <- train(stroke ~ ., data = strokeTrainknn, method = "knn",
                  trControl = control, tuneGrid = grid)
knn_tune$bestTune

strokeTrainknn$stroke = factor(strokeTrain$stroke,
                          levels = c('Yes', 'No'),
                          labels = c(1,0))
strokeTestknn$stroke = factor(strokeTest$stroke,
                            levels = c('Yes', 'No'),
                            labels = c(1,0))
strokeTrainknn <- as.data.frame(lapply(strokeTrainknn, as.numeric))
strokeTestknn <- as.data.frame(lapply(strokeTestknn, as.numeric))
strokeTrainknn$stroke <- as.factor(strokeTrainknn$stroke)
strokeTestknn$stroke <- as.factor(strokeTestknn$stroke)
# k = 1
knn_1 <- knn(train=strokeTrainknn, test=strokeTestknn, cl=strokeTrain$stroke,k=1, prob=T)
cm_1 <- confusionMatrix(table(strokeTest$stroke, knn_1))

mse_1 <- mean((as.numeric(strokeTestknn$stroke) - as.numeric(knn_1))^2)
mae_1 <- mean(abs(as.numeric(strokeTestknn$stroke) - as.numeric(knn_1)))
rmse_1 <- sqrt(mean((as.numeric(strokeTestknn$stroke) - as.numeric(knn_1))^2))
cat("MSE_DT: ", mse_1, "\nMAE_DT: ", mae_1, "\nRMSE_DT: ", rmse_1)

classError <- mean(as.factor(as.integer(knn_1)) != strokeTestknn$stroke)
print(paste("Accurary = ", round(1-classError,5)))

# install.packages(c("pROC", "ROCR))
library(pROC, ROCR)
prob <- attr(knn_1, "prob")
prob
prob <- 2 * ifelse(knn_1 == "-1", 1- prob, prob) - 1

auc_knn <- roc(strokeTest$stroke, prob)$auc
plot(roc(strokeTest$stroke, prob), main = "KNN ROC Curve")
text(0.5, 0.4, paste("AUC =", round(auc_knn, 3)))

# install.packages("yardstick")
library(yardstick)
cm <- conf_mat(table(strokeTestknn$stroke, as.numeric(knn_1)))
autoplot(cm ,type='heatmap')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") +
  xlab("Predicted value") + ylab("Actual value")

# Calculate precision, recall and F1 score
TN_1 <- cm_1$table[1,1] # True negatives
FP_1 <- cm_1$table[1,2] # False positives
FN_1 <- cm_1$table[2,1] # False negatives
TP_1 <- cm_1$table[2,2] # True positives

precision_1 <- TP_1 / (TP_1 + FP_1)
recall_1 <- TP_1 / (TP_1 + FN_1)
F1_score_1 <- 2 * precision_1 * recall_1 / (precision_1 + recall_1)

cat("Precision: ", precision_1, "\nRecall: ", recall_1, "\nF1 score: ", F1_score_1, "\n")

#----------------------------------------------------------------------------------------------
# lightGBM ====
#install.packages("lightgbm")
library(R6)
library(lightgbm)
train_x <- dplyr::select(strokeTrain, -stroke)
train_y <- as.integer(strokeTrain$stroke)

test_x <- dplyr::select(strokeTest, -stroke)
test_y <- as.integer(strokeTest$stroke)

dtrain <- lgb.Dataset(data = as.matrix(train_x), label=train_y)
dtest <- lgb.Dataset.create.valid(dtrain, as.matrix(test_x), label=test_y)

params <- list(objective = "regression", metric = "l2", min_data = 1L,
              learning_rate = .0015, force_row_wise = T,max_depth = 100,min_data_in_lead = 50,
              boosting_type = 'gbdt', bagging_fraction = 0.6, force_row_wise = T,
              unbalance = F, num_leaves = 10, max_bin = 10, num_iterations = 100, 
              feature_fraction = 0.8)
set.seed(123)
valids <- list(test = dtest)
model <- lgb.train(params = params, data = dtrain, nrounds = 5L, valids = valids)
lgb.get.eval.result(model, "test", "l2")

pred_y <- predict(model, as.matrix(test_x), reshape = T)
pred_y <- as.numeric(round(pred_y, 0))

mse_gbm <- mean((test_y - pred_y)^2)
mae_gbm <- caret::MAE(test_y, pred_y)
rmse_gbm <- caret::RMSE(test_y, pred_y)
cat("MSE: ", mse_gbm, "\nMAE: ", mae_gbm, "\nRMSE: ", rmse_gbm)

# install.packages("pROC")
library(pROC)
gbm_auc <- roc(test_y, pred_y)
gbm_auc$auc
plot(gbm_auc, main = "LightGBM Prediction ROC curve", print.auc = T)

cm_gbm <- confusionMatrix(as.factor(test_y), as.factor(pred_y))
cm_gbm

library(yardstick)
cm <- conf_mat(table(test_y, pred_y))
autoplot(cm ,type='heatmap')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") +
  xlab("Predicted value") + ylab("Actual value")

df <- data.frame(test_y, pred_y)
df$id <- 1:nrow(df)

ggplot() + geom_line(data = df, aes(x = id, y = test_y, color = 'test_y')) +
  geom_line(data = df, aes(x=id, y = pred_y, color = 'pred_y')) +
  ggtitle("Stroke data prediction") + theme_minimal() + ylab('stroke') +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

tree_imp <- lgb.importance(model, percentage = TRUE)
lgb.plot.importance(tree_imp, top_n = 5L, measure = "Gain")

# Calculate precision, recall and F1 score
TN_gbm <- cm_gbm$table[1,1] # True negatives
FP_gbm <- cm_gbm$table[1,2] # False positives
FN_gbm <- cm_gbm$table[2,1] # False negatives
TP_gbm <- cm_gbm$table[2,2] # True positives

precision_gbm <- TP_gbm / (TP_gbm + FP_gbm)
recall_gbm <- TP_gbm / (TP_gbm + FN_gbm)
F1_score_gbm <- 2 * precision_gbm * recall_gbm / (precision_gbm + recall_gbm)

cat("Precision: ", precision_gbm, "\nRecall: ", recall_gbm, "\nF1 score: ", F1_score_gbm, "\n")

#----------------------------------------------------------------------------------------------
# Decison tree ====
# install.packages('rpart')
# install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

testing<- rpart(stroke ~.,data=strokeTrain,method = 'class') #build decison tree model
rpart.plot(testing)  #plot decision tree
predict<-predict(testing,strokeTest,type='class') #prediction

predicted_probs<-as.numeric(predict)   #output in prob for ROC plot purpose
predict_value<-predict(testing,strokeTest,type='vector')  
#output in vector for finding MSE, MAE, RMSE purpose

table_mat <- table(strokeTest$stroke, predict) #show confusion matrix table
table_mat

accuracy<-sum(diag(table_mat)/sum(table_mat)) # show accurracy
print(paste('Accuracy for test :', 1- accuracy))

cm_dt <- confusionMatrix(table(strokeTest$stroke, predict)) #show confusion matrix table
cm_dt  #accuracy 

# calculate·MSE MAE RMSE
x<- as.numeric(strokeTest$stroke) #change target variable from yes/no to 1/2
mse_dt <- mean((x - predict_value)^2)
mae_dt <- mean(abs(x - predict_value))
rmse_dt <- sqrt(mean((x - predict_value)^2))
cat("MSE_DT: ", mse_dt, "\nMAE_DT: ", mae_dt, "\nRMSE_DT: ", rmse_dt)

library(pROC)
roc_obj=roc(strokeTest$stroke,predicted_probs ) #AUC/ROC score
plot(roc_obj, main = "ROC Curve", print.auc = TRUE, legacy.axes = TRUE) #plot ROC

cm <- conf_mat(table(strokeTest$stroke, predict))
autoplot(cm ,type='heatmap') +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")

# Calculate precision, recall and F1 score
TN_dt <- cm_dt$table[1,1] # True negatives
FP_dt <- cm_dt$table[1,2] # False positives
FN_dt <- cm_dt$table[2,1] # False negatives
TP_dt <- cm_dt$table[2,2] # True positives

precision_dt <- TP_dt / (TP_dt + FP_dt)
recall_dt <- TP_dt / (TP_dt + FN_dt)
F1_score_dt <- 2 * precision_dt * recall_dt / (precision_dt + recall_dt)

cat("Precision: ", precision_dt, "\nRecall: ", recall_dt, "\nF1 score: ", F1_score_dt, "\n")

#----------------------------------------------------------------------------------------------
# SVM ====
#install.packages('e1071')
#install.packages('yardstick')
library(e1071)
library(yardstick)

svmtrain <- svm(formula = stroke ~ age, #build svm model 
                data = strokeTrain,
                type = 'C-classification',
                kernel = 'linear')

svmpredict <- predict(svmtrain, newdata = strokeTest)#predict 
svmpredict_value<-as.numeric(svmpredict)
#convert output to numeric for finding MSE,MAE,RMSE purpose

cm_svm <- confusionMatrix(table(strokeTest$stroke, svmpredict))
#use conf_mat from yardstick library to visualize
cm_svm

# visualize accuracy====
cm <- conf_mat(table(strokeTest$stroke, svmpredict))
autoplot(cm ,type='heatmap')+
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")

library(pROC)
roc_obj=roc(strokeTest$stroke,svmpredict_value ) #AUC/ROC score
plot(roc_obj, main = "ROC Curve", print.auc = TRUE, legacy.axes = TRUE) #plot ROC 

x<- as.numeric(strokeTest$stroke)
mse_svm <- mean((x - svmpredict_value)^2)
mae_svm<- mean(abs(x - svmpredict_value))
rmse_svm <- sqrt(mean((x - svmpredict_value)^2))
cat("MSE_SVM: ", mse_svm, "\nMAE_SVM: ", mae_svm, "\nRMSE_SVM: ", rmse_svm)

# Calculate precision, recall and F1 score
TN_svm <- cm_svm$table[1,1] # True negatives
FP_svm <- cm_svm$table[1,2] # False positives
FN_svm <- cm_svm$table[2,1] # False negatives
TP_svm <- cm_svm$table[2,2] # True positives

precision_svm <- TP_svm / (TP_svm + FP_svm)
recall_svm <- TP_svm / (TP_svm + FN_svm)
F1_score_svm <- 2 * precision_svm * recall_svm / (precision_svm + recall_svm)

cat("Precision: ", precision_svm, "\nRecall: ", recall_svm, "\nF1 score: ", F1_score_svm, "\n")

str(strokeTrain)
#-----------------------------------------------------------------------------------------------
#ruis
#----------------------------------------------------------------------------
colnames(strokeTrain)
#Encoding to numerical for stroke (Train)
strokeTrain$stroke = factor(strokeTrain$stroke,
                            levels = c('No', 'Yes'),
                            labels = c(0, 1)) 
#Encoding to numerical for stroke (Test)
strokeTest$stroke = factor(strokeTest$stroke,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1)) 


# Random Forest====
# install.packages("randomForest")
library(randomForest)
library(e1071)
library(yardstick)

#Train the random forest model
rf_model <- randomForest(stroke ~ ., data = strokeTrain)
plot(rf_model)
print(rf_model)
varImpPlot(rf_model)
var_importance <- importance(rf_model)
print(var_importance)
sorted_importance <- sort(var_importance, decreasing = TRUE)
print(sorted_importance)

for (i in seq_along(sorted_importance)) {
  var_name <- names(strokeTrain)[i]
  importance_value <- sorted_importance[i]
  print(paste(var_name, ":", importance_value))
}

#Make predictions on test set
rf_preds <- predict(rf_model,strokeTest)
table_mat <-confusionMatrix(table(strokeTest$stroke, rf_preds))
table_mat

# Evaluate the performance for model
# Example of using AUC as a metric
library(pROC)
rf_preds <- as.numeric(as.character(rf_preds))
rf_preds <- ordered(rf_preds, levels = c("0", "1"))
print(rf_preds)

table(strokeTest$stroke)

rf_auc <- auc(strokeTest$stroke, rf_preds)
str(rf_auc)

# Evaluate the accuracy of the random forest model
# Convert rf_preds to numeric
rf_preds <- as.numeric(as.character(rf_preds))
# Evaluate the accuracy of the random forest model
rf_acc <- sum(rf_preds == strokeTest$stroke) / nrow(strokeTest)
print(rf_acc)

# Calculate MSE, MAE, RMSE ----
test_rf <- as.integer(strokeTest$stroke)
mse_rf <- mean((test_rf - rf_preds)^2)
mae_rf <- mean(abs(test_rf - rf_preds))
rmse_rf <- sqrt(mean((test_rf - rf_preds)^2))
cat("MSE_rf: ", mse_rf, "\nMAE_rf: ", mae_rf, "\nRMSE_rf: ", rmse_rf)

# Create confusion matrix
cm_rf <- confusionMatrix(as.factor(rf_preds), strokeTest$stroke)
print(cm_rf)

# Visualize CM and ROC ----
library(ggplot2)
library(ROCR)
# Confusion Matrix plot
cmv_rf <- conf_mat(table(strokeTest$stroke, rf_preds))
autoplot(cmv_rf, type = 'heatmap', label = TRUE) +
  scale_fill_gradient(low = "#F3E5AB", high = "#F1A680") +
  labs(title = paste("Confusion Matrix\nAccuracy:", round(cm_rf$overall["Accuracy"], 3)),
       x = "Predicted Class", y = "Actual Class")+
  theme(plot.title = element_text(hjust = 0.5))

# ROC curve plot
roc_rf <- roc(strokeTest$stroke, rf_preds, levels = rev(levels(strokeTest$stroke)))
plot(roc_rf, print.auc = TRUE, legacy.axes = TRUE,
     main = "ROC Curve for Random Forest Model",
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     print.thres = c(0.1, 0.5, 0.9))

# Calculate precision, recall and F1 score
TN_rf <- cm_rf$table[1,1] # True negatives
FP_rf <- cm_rf$table[1,2] # False positives
FN_rf <- cm_rf$table[2,1] # False negatives
TP_rf <- cm_rf$table[2,2] # True positives

precision_rf <- TP_rf / (TP_rf + FP_rf)
recall_rf <- TP_rf / (TP_rf + FN_rf)
F1_score_rf <- 2 * precision_rf * recall_rf / (precision_rf + recall_rf)

cat("Precision: ", precision_rf, "\nRecall: ", recall_rf, "\nF1 score: ", F1_score_rf, "\n")

#----------------------------------------------------------------------------------------------
# Logistic Regression ====

# install.packages("MASS")
# install.packages("caret")
library(caret)
library(MASS)
library(e1071)
library(yardstick)
#Fit a logistic regression model:
model <- glm(stroke ~ ., data = strokeTrain, family = binomial())
summary(model)
#Make Predictions on test set + evaluate
probabilities <- predict(model, newdata = strokeTest, type = "response")
predicted_classes <- ifelse(probabilities > 0.25, 1, 0)

cm_lr <- confusionMatrix(as.factor(as.matrix(predicted_classes)), strokeTest$stroke)
cm_lr_metrics <- cm_lr$byClass
#Tune the model by selecting the best combination of hyperparameters:
control <- trainControl(method="cv", number=5, verboseIter = FALSE)
tuned_model <- train(stroke ~ ., data = strokeTrain, 
                     method = "glm", 
                     family = "binomial",
                     trControl = control,
                     tuneLength = 10)
print(tuned_model)

# Calculate MSE, MAE, RMSE ----
test_lr <- as.integer(strokeTest$stroke)
mse_lr <- mean((test_lr - probabilities)^2)
mae_lr <- mean(abs(test_lr - probabilities))
rmse_lr <- sqrt(mean((test_lr - probabilities)^2))
cat("MSE_lr: ", mse_lr, "\nMAE_lr: ", mae_lr, "\nRMSE_lr: ", rmse_lr)

# Visualize CM and ROC ----
library(ggplot2)
library(ROCR)
# Confusion Matrix plot
cmv_lr <- conf_mat(table(strokeTest$stroke, predicted_classes))
autoplot(cmv_lr, type = 'heatmap', label = TRUE) +
  scale_fill_gradient(low = "#F3E5AB", high = "#beaed4") +
  labs(title = paste("Confusion Matrix\nAccuracy:", round(cm_lr$overall["Accuracy"], 3)),
       x = "Predicted Class", y = "Actual Class") +
  theme(plot.title = element_text(hjust = 0.5))

# ROC curve plot
roc_lr <- roc(strokeTest$stroke, probabilities)
# Plot ROC curve and calculate AUC
plot(roc_lr, main = "ROC Curve for Tuned Logistic Regression Model\n",
     print.auc = TRUE, col = "black")

# Calculate precision, recall and F1 score
TN_lr <- cm_lr$table[1,1] # True negatives
FP_lr <- cm_lr$table[1,2] # False positives
FN_lr <- cm_lr$table[2,1] # False negatives
TP_lr <- cm_lr$table[2,2] # True positives

precision_lr <- TP_lr / (TP_lr + FP_lr)
recall_lr <- TP_lr / (TP_lr + FN_lr)
F1_score_lr <- 2 * precision_lr * recall_lr / (precision_lr + recall_lr)

cat("Precision: ", precision_lr, "\nRecall: ", recall_lr, "\nF1 score: ", F1_score_lr, "\n")

# ----------------------------------model comparison--------------------------------------------
Accuracy <- c(cm_1$overall[1],cm_gbm$overall[1],
             cm_dt$overall[1],cm_svm$overall[1],
             cm_rf$overall[1],cm_lr$overall[1])

MSE <- c(mse_1, mse_gbm, mse_dt, mse_svm, mse_rf, mse_lr)
MAE <- c(mae_1, mae_gbm, mae_dt, mae_svm, mae_rf, mae_lr)
RMSE <- c(rmse_1, rmse_gbm, rmse_dt, rmse_svm, rmse_rf, rmse_lr)
Precision <- c(precision_1, precision_gbm, precision_dt, precision_svm, precision_rf, 
               precision_lr)
Recall <- c(recall_1, recall_gbm, recall_dt, recall_svm, recall_rf, recall_lr)
F1_score <- c(F1_score_1, F1_score_gbm, F1_score_dt, F1_score_svm, F1_score_rf, F1_score_lr)
Model <- c('KNN 1','LightGBM',
          'Decision Tree', 'SVM',
          'Random Forest', 'Logistic Regression')

model_compare <- data.frame(Model, Accuracy)
ggplot(aes(x=Model, y=Accuracy, fill=Model), data=model_compare) +
  geom_bar(stat='identity') + ggtitle('Accuracy Comparison of Models') +
  xlab('Models') + ylab('Overall Accuracy') + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))
#gbm#3lr#3gbm
model_compare <- data.frame(Model, MSE)
ggplot(aes(x=Model, y=Accuracy, fill=Model), data=model_compare) +
  geom_bar(stat='identity') + ggtitle('MSE Comparison of Models') +
  xlab('Models') + ylab('MSE') + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

model_compare <- data.frame(Model, MAE)
ggplot(aes(x=Model, y=Accuracy, fill=Model), data=model_compare) +
  geom_bar(stat='identity') + ggtitle('MAE Comparison of Models') +
  xlab('Models') + ylab('MAE') + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

model_compare <- data.frame(Model, RMSE)
ggplot(aes(x=Model, y=Accuracy, fill=Model), data=model_compare) +
  geom_bar(stat='identity') + ggtitle('RMSE Comparison of Models') +
  xlab('Models') + ylab('RMSE') + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

model_compare <- data.frame(Model, Precision)
ggplot(aes(x=Model, y=Accuracy, fill=Model), data=model_compare) +
  geom_bar(stat='identity') + ggtitle('Precision Comparison of Models') +
  xlab('Models') + ylab('Precision') + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

model_compare <- data.frame(Model, Recall)
ggplot(aes(x=Model, y=Accuracy, fill=Model), data=model_compare) +
  geom_bar(stat='identity') + ggtitle('Recall Comparison of Models') +
  xlab('Models') + ylab('Recall') + theme_minimal() + 
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

model_compare <- data.frame(Model, F1_score)
ggplot(aes(x=Model, y=Accuracy, fill=Model), data=model_compare) +
  geom_bar(stat='identity') + ggtitle('F1 score Comparison of Models') +
  xlab('Models') + ylab('F1 score') + theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust=0.5, face = "bold"))

# Random Forest Model Enhancement ====
featureselect <- strokeTrain %>% 
  dplyr::select(gender,age,ever_married,stroke,heart_disease,bmi,avg_glucose_level)
featureselect_test<- strokeTest %>% 
  dplyr::select(gender,age,ever_married,stroke,heart_disease,bmi,avg_glucose_level)
rf_model <- randomForest(stroke ~ ., data = featureselect)
write.csv(featureselect, file = "strokeTrain.csv", row.names = TRUE)

#Make predictions on test set
rf_preds <- predict(rf_model,strokeTest)

library(pROC)
rf_preds <- as.numeric(as.character(rf_preds))
rf_preds <- ordered(rf_preds, levels = c("0", "1"))
rf_auc <- auc(strokeTest$stroke, rf_preds)

# Evaluate the accuracy of the random forest model
# Convert rf_preds to numeric
rf_preds <- as.numeric(as.character(rf_preds))
# Evaluate the accuracy of the random forest model
rf_acc <- sum(rf_preds == strokeTest$stroke) / nrow(strokeTest)
print(rf_acc)

# Calculate MSE, MAE, RMSE ----
test_rf <- as.integer(strokeTest$stroke)
mse_rf <- mean((test_rf - rf_preds)^2)
mae_rf <- mean(abs(test_rf - rf_preds))
rmse_rf <- sqrt(mean((test_rf - rf_preds)^2))
cat("MSE_rf: ", mse_rf, "\nMAE_rf: ", mae_rf, "\nRMSE_rf: ", rmse_rf)

# Create confusion matrix
cm_rf <- confusionMatrix(as.factor(rf_preds), strokeTest$stroke)

library(ggplot2)
library(ROCR)
# Confusion Matrix plot
cmv_rf <- conf_mat(table(strokeTest$stroke, rf_preds))
autoplot(cmv_rf, type = 'heatmap', label = TRUE) +
  scale_fill_gradient(low = "#F3E5AB", high = "#F1A680") +
  labs(title = paste("Confusion Matrix\nAccuracy:", round(cm_rf$overall["Accuracy"], 3)),
       x = "Predicted Class", y = "Actual Class")+
  theme(plot.title = element_text(hjust = 0.5))

# ROC curve plot
roc_rf <- roc(strokeTest$stroke, rf_preds, levels = rev(levels(strokeTest$stroke)))
plot(roc_rf, print.auc = TRUE, legacy.axes = TRUE,
     main = "ROC Curve for Random Forest Model",
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     print.thres = c(0.1, 0.5, 0.9))

# Calculate precision, recall and F1 score
TN_rf <- cm_rf$table[1,1] # True negatives
FP_rf <- cm_rf$table[1,2] # False positives
FN_rf <- cm_rf$table[2,1] # False negatives
TP_rf <- cm_rf$table[2,2] # True positives

precision_rf <- TP_rf / (TP_rf + FP_rf)
recall_rf <- TP_rf / (TP_rf + FN_rf)
F1_score_rf <- 2 * precision_rf * recall_rf / (precision_rf + recall_rf)
cat("Precision: ", precision_rf, "\nRecall: ", recall_rf, "\nF1 score: ", F1_score_rf, "\n")
