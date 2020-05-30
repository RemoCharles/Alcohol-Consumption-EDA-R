library(shinythemes)
library(shiny)
library(knitr)
library(pander)
library(plotly)
library(plyr)
library(magrittr)
library(readr)
library(rpart)
library(DMwR)
library(dplyr)
library(corrplot)
library(randomForest)
library(ggplot2)
library(reshape2)
library(corrgram)
library(caret)
library(tidyverse)
library(DT)

#-----------------------------------------------------------------------------------------------------------------------------------
#EXPLORE DATASET

students_mat <- read_csv("data/student-mat.csv")
students_mat$subject <- "Mat"
students_por<-read_csv("data/student-por.csv")
students_por$subject <- "Por"

df_raw = rbind(students_por, students_mat)

#Open and read file
#df_raw <- read.csv("student_merged.csv", stringsAsFactors = FALSE)

df<-df_raw %>% distinct(school,sex,age,address,famsize,Pstatus,
                  Medu,Fedu,Mjob,Fjob,reason,
                  guardian,traveltime,studytime,failures,
                  schoolsup, famsup,activities,nursery,higher,internet,
                  romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)
View(df)
str(df)

#-----------------------------------------------------------------------------------------------------------------------------------
#DATA Cleansing

#Since our GGplot later shows that all Grades have a big correlation with eachtother, we create an avg Grade = Gavg
df$Gavg<-(df$G1+df$G2+df$G3)/3 

student_df = subset(df, select = -c(G1,G2,G3))
str(student_df)
View(student_df)

#Factorize all Columns

df_factorized <- student_df



df_factorized$famsize<-as.numeric(factor(df_factorized$famsize))
df_factorized$Pstatus<-as.numeric(factor(df_factorized$Pstatus))
df_factorized$address<-as.numeric(factor(df_factorized$address))
df_factorized$sex<-as.numeric(factor(df_factorized$sex))
df_factorized$schoolsup<-as.numeric(factor(df_factorized$schoolsup))
df_factorized$famsup<-as.numeric(factor(df_factorized$famsup))
df_factorized$paid<-as.numeric(factor(df_factorized$paid))
df_factorized$activities<-as.numeric(factor(df_factorized$activities))
df_factorized$nursery<-as.numeric(factor(df_factorized$nursery))
df_factorized$higher<-as.numeric(factor(df_factorized$higher))
df_factorized$internet<-as.numeric(factor(df_factorized$internet))
df_factorized$romantic<-as.numeric(factor(df_factorized$romantic))
df_factorized$guardian<-as.numeric(factor(df_factorized$guardian))
df_factorized$Mjob<-as.numeric(factor(df_factorized$Mjob))
df_factorized$Fjob<-as.numeric(factor(df_factorized$Fjob))
df_factorized$reason<-as.numeric(factor(df_factorized$reason))


#as factor for the categorical Data

df_factorized$subject<-as.factor(factor(df_factorized$subject))
df_factorized$school<-as.factor(factor(df_factorized$school))
df_factorized$Mjob <- as.factor(df$Mjob)
df_factorized$Fjob <- as.factor(df$Fjob)
df_factorized$guardian <- as.factor(df$guardian)
df_factorized$reason <- as.factor(df$reason)


str(df_factorized)
View(df_factorized)

#Put them in Matrix Model to factorize(or just factorize by hand)
df_factorized_matrix <- data.frame(model.matrix( ~ .- 1, data=student_df)) 

str(df_factorized_matrix)
summary(df_factorized_matrix)

#Cut the categorical values in case numeric Model makes sense 
df_numeric = subset(df_factorized, select = -c(Mjob,Fjob,guardian,reason,school,subject))
str(df_numeric)

#-----------------------------------------------------------------------------------------------------------------------------------
#Create Training and Test Set
set.seed(99)
train.data <- sample_frac(df_factorized_matrix, 0.7) # select 70% random samples
test.data <- setdiff(df_factorized_matrix,train.data)

#create train and test set with numerical values
train.data_numeric <- sample_frac(df_numeric, 0.7) # select 70% random samples
test.data_numeric <- setdiff(df_numeric,train.data_numeric)


#-----------------------------------------------------------------------------------------------------------------------------------
#Visualize Correlations

#Data Exploration finding Correlations (visualized in shiny too)
ggplot(aes(x=failures,y=Dalc),data=student_df)+
  geom_point()

ggplot(aes(x=Dalc,y=Gavg, group=Dalc),data=df)+
  geom_boxplot()

#Family Relationship 
plot(famrel ~ absences,data=student_df )

res <- cor(df_factorized_matrix)
round(res, 2)

#Correlation Matrix Results: shows strong Correlation between all Grades (cut them or average them)
cormat <- cor(round(df_numeric,2))
corrplot(cormat, method = "number")
#for numeric correlation Matrix, change df to df_factorized_matrix


#Try and find other correlations (Reshape Correlation to show highest correlations first)
melted_cormat <- melt(cormat)
head(melted_cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


#Try and find Variables to best predict Grading Average (Gavg) based on correlation 


#PCA

#With PCA We create A correlation Matrix to narrow down the variable for our model
cor_df <- cor(df_factorized_matrix, df_factorized_matrix, method = "pearson")

#Determine the Independent Variables 1:41 and the dependent Gavg (42) 
student_cor_df<- data.frame(cor=cor_df[1:41,42], varn = names(cor_df[1:41,42])) 
student_cor_df<- student_cor_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))
plot(student_cor_df$cor_abs, type="l")

#As you can see there's around 7-10 Variables that can explain a correlation of over 15%

#Filter out Values with under 15% Correlation
list_var_names <- student_cor_df %>% filter(cor_abs>0.15)
filter_df <- data.frame(df_factorized_matrix) %>% select(Gavg,one_of(as.character(list_var_names$varn)))
summary(filter_df)

#These values we determine for our Best Model (lm7)

y <- filter_df %>% select(-Gavg)
pca = prcomp(y, scale. = T, center = T)
plot(pca, type="l")
summary(pca)

#We cut PC7 and PC8 cause 95% of the Variance is already explained by other Dimensions 
pca_df <- data.frame(pca$x)
pca_df <- pca_df %>% select(-PC7,-PC8) 
pca_df$Gavg = filter_df$Gavg
pca_model <- lm(data = pca_df, Gavg ~ .)
summary(pca_model)

#PCA shows we don't have many variables that correlate or explain the rest of the variables
 


#Create Model to predict a student's Average Grade, What Variables should we use? (Prediction made on df_factorized_matrix with all factorized Data ))

#Average grades with failures
lm1 <- lm(Gavg ~ failures, data = df_factorized_matrix)
summary(lm1)
#Average grades with failures and higher education (bolean)
lm2 <- lm(Gavg ~ failures+higheryes, data = df_factorized_matrix)
summary(lm2)
#Average grades with failures and higher education and mother education and studytime
lm3 <- lm(Gavg ~ failures+higheryes+Medu+studytime+Fedu, data = df_factorized_matrix)
summary(lm3)
# "" and father education and school 1 and school 2
lm4 <- lm(Gavg ~ failures+higheryes+Medu+studytime+Fedu+schoolGP+schoolMS, data = df_factorized_matrix)
summary(lm4)
# "" and daily alc. consumption
lm5 <- lm(Gavg ~ failures+higheryes+schoolGP+schoolMS+Medu+studytime+Fedu+Dalc, data = df_factorized_matrix)
summary(lm5)
#Taking the Variables with highest correlation values from PCA
lm7 <- lm(Gavg ~ failures+higheryes+schoolGP+Medu+subjectPor+studytime+Fedu+Dalc, data = df_factorized_matrix)
summary(lm7)


# Testing Models on the numeric Dataset that we prepared
lm_numeric <- lm(Gavg ~ .,data = df_numeric)
summary(lm_numeric)

lm_all <- lm(Gavg ~ .,data = df_factorized_matrix)
summary(lm_all)

#The numeric model is worse
#This shows that we can't narrow our selection of variables to numeric, cause the model is in need of
#the categorical data points given in our dataset
#We will not continue tests without categorical data

anova(lm1,lm2, lm3, lm4,lm5, lm7)


#Move on with model 7 because lowest RSS
summary(lm7)
summary(lm7)$coef
#As you can see. All Variables chosen in the Model have some kind of significants (t value) to the dependent variable (Gavg)
#Because of the low correlation between datapoints and
#R2 Value of 0.2722 we can expect our models accuracy to be sparse (R2 = 0.2722). It's the best we could build.

#predict new Value --> shiny: used pasted parameter values of newdata
summary(df_factorized_matrix)

predLinear <- predict(lm7, newdata = data.frame(failures=2 ,higheryes =1 , schoolGP=1 , Medu=3 ,subjectPor=1 , 
                                                studytime=2 ,Fedu=2, Dalc=1 ))
print(predLinear)

#Predicting our Test Data with the Model
fitted.results <- predict(lm7, newdata=subset(test.data, select=c(failures,higheryes,schoolGP,Medu,subjectPor,
                                                                  studytime,Fedu,Dalc)))
#Model Performance Evaluation
RMSE(fitted.results, test.data$Gavg)
Percentage_RMSE = 2.572/mean(test.data$Gavg) 
#The RMSE shows that prediction Error, with 22% is fairly low (and surprisingly good), given the R2 Value

#R2 Value is low, meaning the observed Values aren't very correlated with the predicted values (as expected from the Model Evaluation)
R2(fitted.results, test.data$Gavg)


