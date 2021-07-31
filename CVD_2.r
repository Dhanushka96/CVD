cvd_data <- read.csv("cardio_train.csv", sep = ";")
head(cvd_data,10)

##Libraries
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(viridis)
library(scales)
library(RColorBrewer)
library(corrplot)
library(GoodmanKruskal)
library(class)
  
##Identifying and removing unusual observation
##checking missing values
any(is.na(cvd_data))
##checking duplicates

duplicated(cvd_data) %>% sum()
#summary of quantitative data
quantitative <- data.frame(cvd_data$age,cvd_data$height,cvd_data$weight,cvd_data$ap_hi,cvd_data$ap_lo)
summary(quantitative)
#Removing heights < 100cm
cvd_data <- cvd_data%>% filter(cvd_data$height>=100)
#Removing weights < 20Kg
cvd_data <- cvd_data%>% filter(cvd_data$weight >= 20)
#Removing diastolic blood pressure which are equal to zero
cvd_data <- cvd_data%>% filter(cvd_data$ap_lo != 0)
#Make negative systolic and diastolic blood pressure values positive.
cvd_data<-abs(cvd_data)
#Removing systolic BP< diastolic BP
cvd_data <- cvd_data%>% filter(cvd_data$ap_hi > cvd_data$ap_lo)
#Removing systolic BP values > 370
cvd_data <- cvd_data%>% filter(cvd_data$ap_hi<=370)
#Removing systolic BP values < 60
cvd_data <- cvd_data%>% filter(cvd_data$ap_hi >=60)
#Removing diastolic BP values < 30
cvd_data <- cvd_data%>% filter(cvd_data$ap_lo >=30)


##Data preparation
##Removing ID
cvd_data <- cvd_data[-c(1)]
##Changing the age in days into years
cvd_data$age <- cvd_data$age/365.25
cvd_data$age <- round(cvd_data$age,digits = 0)
##Organizing categorical variables
cvd_data$cholesterol <- factor(cvd_data$cholesterol,levels = c(1,2,3), labels = c("Normal","Above Normal","Well Above Normal"))
cvd_data$gluc <- factor(cvd_data$gluc,levels = c(1,2,3), labels = c("Normal","Above Normal","Well Above Normal"))
cvd_data$gender <- factor(cvd_data$gender, levels = c(1,2), labels = c("Women","Men"))
cvd_data$smoke <- factor(cvd_data$smoke,levels = c(0,1), labels = c("Not Smoke","Smoke"))
cvd_data$alco <- factor(cvd_data$alco,levels = c(0,1), labels = c("Non alcoholic","alcoholic"))
cvd_data$active <- factor(cvd_data$active,levels = c(0,1), labels = c("Not active","Active"))
cvd_data$cardio <- factor(cvd_data$cardio, levels = c(0,1),labels = c("Absent","Present"))
cvd_data2<-cvd_data

  
#GENERATING TRAIN AND TEST SET 
#Selecting 80% of data as sample from total 'n' rows of the data 
data <- floor(0.8 * nrow(cvd_data2))
set.seed(1234)
train_set <- sample(seq_len(nrow(cvd_data2)), size = data)
train <- cvd_data2[train_set, ]
test <- cvd_data2[-train_set,] 

blank_theme<- theme(axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.border = element_blank(),
                    panel.grid=element_blank(),
                    axis.ticks = element_blank(),
                    plot.title=element_text(size=14, face="bold"))

      ##Descriptive analysis
  ##Univariate
#cardio-piechart
data_c<-data.frame(group=c("Present","Absent"),value=c(27124,27821))
ggplot(data_c, aes(x="", y=value, fill=group))+
  coord_polar("y", start=0)+blank_theme+
  geom_bar(width = 1, stat = "identity")+
  scale_fill_manual(values =c("steelblue1","dodgerblue4")) +
  theme(axis.text.x=element_blank())+
  geom_text(aes(label = paste0(value, " (",scales::percent(value / sum(value)),")")),fontface="bold",size=4,position = position_stack(vjust = 0.5))

#age-boxplot
ggplot(train,aes(x=age))+geom_boxplot(fill="steelblue1",alpha=1)+
  labs(x="Age(yrs)")+scale_x_continuous(breaks=seq(20,70,by=10))+ scale_y_continuous() +
  theme(axis.title = element_text(face="bold",size = "14"))

#age-histogram
ggplot(train, aes(x=age)) + geom_histogram(aes(fill=..count..),color="#000000",bins=12)+
  geom_vline(aes(xintercept=mean(age)),color="red", linetype="dashed", size=1)+
  labs(x="Age(yrs)")+
  scale_fill_gradient("Count",low = "steelblue1", high = "darkblue")

#Gender- pie chart
data_gender<-data.frame(group=c("Women","Men"),value=c(35678,19267))
ggplot(data_gender, aes(x="", y=value, fill=group))+
  coord_polar("y", start=0)+blank_theme+
  geom_bar(width = 1, stat = "identity")+
  scale_fill_manual(values =c("steelblue1","dodgerblue4"))+
  theme(axis.text.x=element_blank())+
  geom_text(aes(label = paste0(value, " (",scales::percent(value / sum(value)),")")),fontface="bold",size=4,position = position_stack(vjust = 0.5))

#Cholesetrol- piechart
data_chole<-data.frame(group=c("Well Above Normal","Above Normal"," Normal"),value=c(6299,7436,41191))
ggplot(data_chole, aes(x="", y=value, fill=group))+
  coord_polar("y", start=0)+blank_theme+
  geom_bar(width = 1, stat = "identity")+
  scale_fill_brewer(palette = "Blues",direction = -1)+
  theme(axis.text.x=element_blank())+
  geom_text(aes(label = paste0(value, " (",scales::percent(value / sum(value)),")")),fontface="bold",size=3,position = position_stack(vjust = 0.5))

#Glucose-bar chart
data_glu<-data.frame(group=c("Well Above Normal","Above Normal"," Normal"),value=c(4185,4055,46686))
ggplot(data_glu, aes(x=group, y=value,fill=group)) +
  geom_bar(width=0.5,stat="identity")+scale_fill_brewer(palette = "Blues",direction = 1)+
  labs(X="Glucose Levels",y="No.of Patients")+ geom_text(aes(label = paste0(value, " (",scales::percent(value / sum(value)),")")),fontface="bold",size=3, vjust=1.6, color="Black")+
  theme(axis.title = element_text(face="bold",size = "8"))

#smoke-pie
data_smoke<-data.frame(group=c("Not Smoke","Smoke"),value=c(50097,4829))
ggplot(data_smoke, aes(x="", y=value, fill=group))+
  coord_polar("y", start=0)+blank_theme+
  geom_bar(width = 1, stat = "identity")+
  scale_fill_brewer(palette = "Blues",direction = -1)+
  theme(axis.text.x=element_blank())+
  geom_text(aes(label = paste0(value, " (",scales::percent(value / sum(value)),")")),fontface="bold",size=3,position = position_stack(vjust = 0.5))

#alcohol-bar
data_alc<-data.frame(group=c("alcoholic","Non alcoholic"),value=c(2958,51968))
ggplot(data_alc, aes(x=group, y=value,fill=group)) +
  geom_bar(width=0.5,stat="identity")+scale_fill_brewer(palette = "Blues",direction = -1)+
  labs(X="Glucose Levels",y="No.of Patients")+ geom_text(aes(label = paste0(value, " (",scales::percent(value / sum(value)),")")),fontface="bold",size=3, vjust=1.6, color="Black")+
  theme(axis.title = element_text(face="bold",size = "10"))

#activities
data_ac<-data.frame(group=c("Not active","Active"),value=c(10812,44114))
ggplot(data_ac, aes(x=group, y=value,fill=group)) +
  geom_bar(width=0.5,stat="identity")+scale_fill_manual(values =c("steelblue1","dodgerblue3"))+
  labs(X="Glucose Levels",y="No.of Patients")+ geom_text(aes(label = paste0(value, " (",scales::percent(value / sum(value)),")")),fontface="bold",size=3, vjust=1.6, color="Black")+
  theme(axis.title = element_text(face="bold",size = "10"))

#height
ggplot(train,aes(x=height))+geom_boxplot(fill="#80DEEA",alpha=1)+
  labs(x="Height(cm)")+scale_x_continuous(breaks=seq(100,270,by=20))+ scale_y_continuous() +
  theme(axis.title = element_text(face="bold",size = "12"))

#weight
ggplot(train,aes(x=weight))+geom_boxplot(fill="#26C6DA",alpha=1)+
  labs(x="Weight(kg)")+scale_x_continuous(breaks=seq(20,200,by=20))+ scale_y_continuous() +
  theme(axis.title = element_text(face="bold",size = "12"))

#Systolic BP
ggplot(train,aes(x=ap_hi))+geom_boxplot(fill="#00ACC1",alpha=1)+
  labs(x="Systolic BP")+scale_x_continuous(breaks=seq(60,240,by=30))+ scale_y_continuous() +
  theme(axis.title = element_text(face="bold",size = "12"))

#diastolic BP
ggplot(train,aes(x=ap_lo))+geom_boxplot(fill="#64B5F6",alpha=1)+
  labs(x="Diastolic BP")+scale_x_continuous(breaks=seq(30,190,by=30))+ scale_y_continuous() +
  theme(axis.title = element_text(face="bold",size = "12"))

  ##Univariate analysis
#cardio vs age
ggplot(train,aes(x=cardio,y=age,fill=cardio))+geom_boxplot(alpha=0.6) + labs(y="Age(yrs)",x="Cardio")+ theme(legend.position = "none")+
  scale_fill_manual(values =c("#80DEEA","#303F9F"))+theme(axis.title = element_text(face="bold",size = "12"))

#cardio vs systolic
ggplot(train,aes(x=cardio,y=ap_hi,fill=cardio))+geom_boxplot(alpha=0.9) + labs(y="Systolic BP",x="Cardio")+ theme(legend.position = "none")+
  scale_fill_manual(values =c("#00ACC1","#01579B"))+theme(axis.title = element_text(face="bold",size = "12"))

#Cardio vs diastolic
ggplot(train,aes(x=factor(cardio),y=ap_lo))+geom_boxplot(alpha=0.9,aes(fill = cardio))+ labs(y="Diastolic BP",x="Cardio")+ theme(legend.position = "none")+
  theme(axis.title = element_text(face="bold",size = "10"))+scale_fill_manual(values =c("#2196F2","#3F51B5"))

#cardio vs height
ggplot(train,aes(x=cardio,y=height,fill=cardio))+geom_boxplot(alpha=0.9) + labs(y="Height",x="Cardio")+ theme(legend.position = "none")+
  scale_fill_manual(values =c("skyblue3","dodgerblue4"))+theme(axis.title = element_text(face="bold",size = "12"))

#cardio vs weight
ggplot(train,aes(x=cardio,y=weight,fill=cardio))+geom_boxplot(alpha=0.9) + labs(y="Weight",x="Cardio")+ theme(legend.position = "none")+
  scale_fill_manual(values =c("dodgerblue3","darkblue"))+theme(axis.title = element_text(face="bold",size = "12"))

#cardio vs cholesterol
freq_chol = count(train, cholesterol, cardio)
ggplot(freq_chol, aes(x=cholesterol, y=n, fill= cardio)) + geom_bar(width = 0.6, stat = "identity", position = "fill",alpha=0.6) + ylab("Percentage") + 
  xlab("Cholesterol")+scale_y_continuous(labels = percent_format())+scale_fill_manual(values = c("skyblue3","midnightblue"))+
  theme(axis.title = element_text(face="bold",size = "12"))
chol_table<-table(train$cholesterol,train$cardio)
chol_table

#cardio vs Glucose
freq_gluc = count(train, gluc, cardio)
ggplot(freq_gluc, aes(x=gluc, y=n, fill= cardio)) + geom_bar(width = 0.6, stat = "identity", position = "fill",alpha=0.7) + ylab("Percentage") + 
  xlab("Glucose")+scale_y_continuous(labels = percent_format())+scale_fill_manual(values = c("dodgerblue3","midnightblue"))+
  theme(axis.title = element_text(face="bold",size = "12"))
gluc_table<-table(train$gluc,train$cardio)
gluc_table

#cardio vs smoke
freq_smoke = count(train, smoke, cardio)
ggplot(freq_smoke, aes(x=smoke, y=n, fill= cardio)) + geom_bar(width = 0.6, stat = "identity", position = "fill",alpha=0.7) + ylab("Percentage") + 
  xlab("Smoke")+scale_y_continuous(labels = percent_format())+scale_fill_manual(values = c("dodgerblue3","midnightblue"))+
  theme(axis.title = element_text(face="bold",size = "12"))
smoke_table<-table(train$smoke,train$cardio)
smoke_table

#cardio vs alcohol
freq_alc = count(train, alco, cardio)
ggplot(freq_alc, aes(x=alco, y=n, fill= cardio)) + geom_bar(width = 0.6, stat = "identity", position = "fill",alpha=1) + ylab("Percentage") + 
  xlab("Alcohol Use")+scale_y_continuous(labels = percent_format())+scale_fill_manual(values = c("skyblue3","midnightblue"))+
  theme(axis.title = element_text(face="bold",size = "12"))
alc_table<-table(train$alco,train$cardio)
alc_table

#cardio vs active
freq_act = count(train, active, cardio)
ggplot(freq_act, aes(x=active, y=n, fill= cardio)) + geom_bar(width = 0.6, stat = "identity", position = "fill",alpha=0.9) + ylab("Percentage") + 
  xlab("Activity status")+scale_y_continuous(labels = percent_format())+scale_fill_manual(values = c("dodgerblue3","midnightblue"))+
  theme(axis.title = element_text(face="bold",size = "12"))
ack_table<-table(train$active,train$cardio)
ack_table


#Association between Quantitative variables (Spearman correlation Plot)
library(corrplot)
corr_cts=subset(train,select = c("cardio","age","height","weight","ap_hi","ap_lo" )) 
corr_cts[] <-lapply(corr_cts,as.integer)
correlation<-cor(corr_cts)
corrplot(cor(corr_cts,method = "pearson", use = "everything"),method = "color",addCoef.col = "black",tl.col = "black")
mydata <-train[,c("cardio","age","height","weight","ap_hi","ap_lo")]
rquery.cormat(mydata)



#Association between categorical variables (Goodman Kruskal Plot)
library(GoodmanKruskal)
var_set <- c("gender","cholesterol","gluc","smoke","alco","active","cardio")
df <- subset(train, select = var_set)
GK_matrix <- GKtauDataframe(df)
plot(GK_matrix)


    ###Advanced analysis
 ##set reference level of the response as "Absent"
train$cardio = relevel(train$cardio, ref = "Absent")
#Fitting Logistic Regression Model
full_model <- glm(data=train, cardio~., family = binomial(link = "logit"))
summary(full_model)

 ##feature selecting 
#Forward selection
bestmodel_f=step(full_model,scope=~age+gender+height+weight+ap_hi+ap_lo+cholesterol+gluc+smoke+alco+active,data=train ,family =binomial )

#Backward elimination
bestmodel_b<-step(full_model, direction = "backward")

#Stepwise selection
bestmodel_s<-step(full_model, direction = "both")

 ##Prediction accuracy of the full logistic regression model
#making predicitons on the test set
pred_prob_b <- predict(full_model, newdata = test, type = "response")
pred_classes_b<- ifelse(pred_prob_b >0.5, "Present", "Absent")
y_pred_b<- factor(pred_classes_b, levels = c("Present","Absent"))
observed_classes <- test$cardio
accuracy_full = mean(y_pred_b==observed_classes)
accuracy_full
table(y_pred_b,test$cardio)


 ##REGULARIZED REGRESSION
#logistics with Ridge
# Dumy code categorical predictor variables
x = model.matrix(cardio~., train)[,-1]
x.test = model.matrix(cardio~., test)[,-1]
y = train$cardio
y.test = test$cardio

library(glmnet)
#fit model to the training data
fit.ridge = glmnet(x,y,alpha = 0,family = "binomial")
plot(fit.ridge, xvar="lambda",lw=2, label=TRUE)
#Applying cross validation to select the best lambda,use 10-fold cross validation for that
set.seed(5)
cv.ridge = cv.glmnet(x,y,alpha = 0,family = "binomial",type.measure = "class" )
plot(cv.ridge, main = "Ridge Penalty\n\n")
MSE_min_ridge = min(cv.ridge$cvm)
#Minimum MSE from the Ridge model
MSE_min_ridge
#best Lambda related to the minimum MSE
best_lamda_ridge = cv.ridge$lambda.min 
best_lamda_ridge
#Fitting the ridge regression model under the best lambda
out_ridge = glmnet(x,y, alpha = 0, family = "binomial",lambda =best_lamda_ridge )
#get the coefficients
coe<-predict(out_ridge, type = "coefficients", s= best_lamda_ridge)
coe
summary(out_ridge)

#making predictions on the test set
pred_prob_ridge <- predict(fit.ridge, newdata = test, type = "response",newx = x.test)
pred_classes_ridge<- ifelse(pred_prob_ridge >0.5, "Present", "Absent")
y_pred_ridge<- factor(pred_classes_ridge, levels = c("Present","Absent"))

#Accuracy of the model
accuracy_ridge <- mean(pred_classes_ridge == observed_classes)
accuracy_ridge
plot(fit.ridge, xvar="lambda",lw=2, label=TRUE)
abline(v = log(best_lamda_ridge), col = "red", lty = "dashed")
abline(v = log(cv.ridge$lambda.1se), col = "blue", lty = "dashed")
ridge.predict <- predict(fit.ridge, type = "class", s= best_lamda_ridge, newx = x.test)
table(ridge.predict,observed_classes)

#simple model
optimum<-cv.ridge$lambda.1se
out_lesso = glmnet(x,y, alpha = 1, family = "binomial",lambda = optimum)
co<-predict(out_lesso, type = "coefficients", s= best_lamda_ridge)
co



  ##lesso regression
fit.lesso = glmnet(x,y,alpha = 1,family = "binomial",lambda = NULL)
plot(fit.lesso, xvar="lambda",lw=2, label=TRUE)

#Applying cross validation to select the best lambda
set.seed(5)
cv.lesso = cv.glmnet(x,y,alpha = 1,family = "binomial",type.measure = "class" )
plot(cv.lesso)
MSE_min_lesso = min(cv.lesso$cvm)
#Minimum MSE from the lesso model
MSE_min_lesso
#best Lambda related to the minimum MSE
best_lamda_lesso = cv.lesso$lambda.min 
best_lamda_lesso

#Fitting the lesso regression model under the best lambda
out_lesso = glmnet(x,y, alpha = 1, family = "binomial")
#get the coefficients
co<-predict(out_lesso, type = "coefficients", s= best_lamda_lesso) 
co
#simple model
optimum<-cv.lesso$lambda.1se
out_lesso = glmnet(x,y, alpha = 1, family = "binomial",lambda = optimum)
co<-predict(out_lesso, type = "coefficients", s= best_lamda_lesso)
co

#making predicitons on the test set
pred_prob_lesso <- predict(fit.lesso, newdata = test, type = "response",newx = x.test)
pred_classes_lesso <- ifelse(pred_prob_lesso >0.5, "Present", "Absent")
y_pred_lesso <- factor(pred_classes_lesso, levels = c("Present","Absent"))
table(y_pred_lesso,test$cardio)
se<-cv.lesso$lambda.1se
co<-predict(out_lesso, type = "coefficients", s= se) 
co
lesso.predict <- predict(fit.lesso, type = "class", s= best_lamda_lesso, newx = x.test)
table(lesso.predict,observed_classes)


#Accuracy of the model
accuracy_lesso <- mean(y_pred_lesso == observed_classes)
accuracy_lesso
plot(fit.lesso, xvar="lambda",lw=2, label=TRUE)
abline(v = log(best_lamda_lesso), col = "red", lty = "dashed")
abline(v = log(cv.lesso$lambda.1se), col = "blue", lty = "dashed")

##Binary Logistics with Elastic Net

#setting the parameters for alpha
alpha_list <- seq(0,1,by=0.1)

#function of ENet
Enet <- lapply(alpha_list, function(a){
  cv.glmnet(x,y,alpha = a,lambda.min.ratio = 0.001, family = "binomial",type.measure = "class" )
})
for (i in 1:11){
  print(min(Enet[[i]]$cvm))
}

#obtaining the global minimum alpha for the Elastic Net
min_alpha <- min(min(Enet[[i]]$cvm))
min_alpha

set.seed(5)

#plotting Elastic Net with minimum Alpha
fit.Enet = glmnet(x,y,alpha = min_alpha,family = "binomial")
plot(fit.Enet, xvar="lambda",lw=2, label=TRUE)

#Applying cross validation to select the best lambda
set.seed(5)
cv.enet = cv.glmnet(x,y,alpha = min_alpha,family = "binomial",type.measure = "class" )
plot(cv.enet)
MSE_min_ENet = min(cv.enet$cvm) #Minimum MSE from the ENet model
MSE_min_ENet
best_lam_ENet = cv.enet$lambda.min #best Lambda related to the minimum MSE
best_lam_ENet

#Fitting the Elastic Net regression model under the best lambda
out_ENet = glmnet(x,y, alpha = min_alpha, family = "binomial")
co<-predict(out_ENet, type = "coefficients", s= best_lam_ENet) #using predict function to get the coefficients
co
#confusion matrix
ENet.predict <- predict(fit.Enet, type = "class", s= best_lam_ENet, newx = x.test)
table(ENet.predict,observed_classes)

#Accuracy of the model
Accuracy_ENet <- mean(ENet.predict == observed_classes)
Accuracy_ENet

plot(fit.Enet, xvar="lambda",lw=2, label=TRUE)
abline(v = log(best_lam_ENet), col = "red", lty = "dashed")
abline(v = log(cv.enet$lambda.1se), col = "blue", lty = "dashed")

#simple model
optimum<-cv.enet$lambda.1se
out_lesso = glmnet(x,y, alpha = 1, family = "binomial",lambda = optimum)
co<-predict(out_lesso, type = "coefficients", s= best_lamda_Enet)
co

 ##KNN
library(class)
train_predictors<-cbind(train$age,train$ap_hi,train$ap_lo,train$height,train$weight,train$cholesterol,train$smoke,train$alco,train$active,train$gender)
train_cardio <-train$cardio
test_predictors<-cbind(test$age,test$ap_hi,test$ap_lo,test$height,test$weight,test$cholesterol,test$smoke,test$alco,test$active,test$gender)
#set a random seed to ensure reproducibility of results. Initially set k=1
set.seed (1)
knn_pred=knn(train_predictors,test_predictors,train_cardio,k=11)
acc_knn<- mean(knn_pred == test$cardio)
acc_knn
table(knn_pred,test$cardio)

        

                






























