library()

#Open and read file
df <- read.csv("student_merged.csv", stringsAsFactors = FALSE)
View(df)

#Filtering Data to only numeric Data
##dfnum <- Filter(is.numeric, df)

#factorizing all Data categorical data
df_factorized <- df

df_factorized$class<-as.numeric(factor(df_factorized$class,labels=c(-1,1)))
df_factorized$famsize<-as.numeric(factor(df_factorized$famsize,labels=c(4,3)))
df_factorized$Pstatus<-as.numeric(factor(df_factorized$Pstatus,labels=c(-1,1)))
df_factorized$address<-as.numeric(factor(df_factorized$address,labels=c(-1,1)))
df_factorized$sex<-as.numeric(factor(df_factorized$sex,labels=c(-1,1)))
df_factorized$school<-as.numeric(factor(df_factorized$school,labels=c(2,1)))
df_factorized$schoolsup<-as.numeric(factor(df_factorized$schoolsup, labels=c(0,1)))
df_factorized$famsup<-as.numeric(factor(df_factorized$famsup, labels=c(0,1)))
df_factorized$paid<-as.numeric(factor(df_factorized$paid, labels=c(0,1)))
df_factorized$activities<-as.numeric(factor(df_factorized$activities, labels=c(0,1)))
df_factorized$nursery<-as.numeric(factor(df_factorized$nursery, labels=c(0,1)))
df_factorized$higher<-as.numeric(factor(df_factorized$higher, labels=c(0,1)))
df_factorized$internet<-as.numeric(factor(df_factorized$internet, labels=c(0,1)))
df_factorized$romantic<-as.numeric(factor(df_factorized$romantic, labels=c(0,1)))
df_factorized$guardian<-as.numeric(factor(df_factorized$guardian))
df_factorized$Mjob<-as.numeric(factor(df_factorized$Mjob))
df_factorized$Fjob<-as.numeric(factor(df_factorized$Fjob))
df_factorized$reason<-as.numeric(factor(df_factorized$reason))
df_factorized$Gintv<-as.numeric(factor(df_factorized$Gintv))
df_factorized$class<-as.numeric(factor(df_factorized$class, labels=c(-1,1)))

summary(df_factorized)



#Cut the categorical values 
df_clean = subset(df_factorized, select = -c(G2,G3,Mjob,Fjob,guardian,reason))

#Correlation Matrix Results: shows strong Correlation between all Grades (cut them or average them)
df_cor <- cor(round(df_clean,2))
corrplot(df_cor, method = "number")




#Try and find other correlations (Reshape Correlation to show highest correlations first)
melted_cormat <- melt(df_cor)
head(melted_cormat)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

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

reorder_cormat <- function(df_cor){
  # Use correlation between variables as distance
  dd <- as.dist((1-df_cor)/2)
  hc <- hclust(dd)
  df_cor <-df_cor[hc$order, hc$order]
}

# Reorder the correlation matrix
df_cor <- reorder_cormat(df_cor)
upper_tri <- get_upper_tri(df_cor)
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


PCA

df.pca <- prcomp(c1:7,)
summary(mtcars.pca)

#

plot(famrel ~ absences,data=df )

res <- cor(df)
round(res, 2)

#
correlat


#
