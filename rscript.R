#R SCRIPT
#FILE COMPLETE OF THE DAfb project about Brazilian_Houses

#Packages

library(corrplot) #used for the correlation matrix
library(tidyverse)# used for duplicated()
library(dplyr) 
library(magrittr)
library(gridExtra)#used to plot 
library(heatmaply)#used for heat map
library(ggplot2)#used for plots
library(mice)#used for substitution of NA values on floor 
library(gridExtra)#used for plot arrange
library(DataExplorer) #used for EDA
library(factoextra)#used for cluster
library(cluster)#used for cluster
library(dendextend)#used for cluster
library(mclust)#used for cluster
library(caret)#used for KNN
library(scatterplot3d)#used for scatterplot
library(glmnet) #used for LASSO AND RIDGE
library(xgboost) #used for XGBoost
library(GGally) #used in clustering 
library(purrr)#used for heirarchical
library(randomForest) #used for random forest
library(kernlab)#used for svm

#The read.csv function in R is used to read data from a CSV (Comma-Separated Values) file and store it as a data frame in R. 
Data <- read.csv('BrazHousesRent.csv')
#little OVERVIEW of the dataset: structure and summary 
#The str() function in R is used to display the structure of an R object. It provides a concise summary of the object's internal structure, including its type, dimensions, and the data contained within it.
str(Data)
summary(Data)

#Cleaning dataset
#Convert floor in numeric 
Data$floor <- as.numeric(Data$floor)
#Convert animal,city,furniture as factors
Data$animal <- as.factor(Data$animal)
Data$city <- as.factor(Data$city)
Data$furniture <- as.factor(Data$furniture)
#checking null values 
sapply(Data, function(x) sum(is.na(x)))
#We can see that only floor contains null values, we will deal with them later!
Data <- Data[!duplicated(Data),] #dataframe with no duplicates
##Let's move the target(rent) variable to the last column 
Data <- Data[, c(names(Data)[names(Data) != "rent.amount..R.."], "rent.amount..R..")]

#lets CHANGE NAMES to the columns for the sake of simplicity 
Data <- rename(Data, monthly.tax = hoa..R..)
Data <- rename(Data, rent.amount = rent.amount..R..)
Data <- rename(Data, property.tax = property.tax..R..)
Data <- rename(Data, fire.insurance = fire.insurance..R..)

#Dealing with NULL Values
#using PMM to face the NA values problem in the FLOOR column
library(mice) 
brz_data <- Data
# Create the imputation object. We will use a simple pmm.
set.seed(123)
imp <- mice(brz_data, method = "pmm")
imputed <- complete(imp)[,"floor"]
# replaced imputed data with previous 
brz_data$floor[is.na(brz_data$floor)] <- imputed[is.na(brz_data$floor)] #replacing the new imputed with previous
head(brz_data)
str(brz_data)
#a little summary of the new dataset
summary(brz_data)

#Outliers
# We select numeric features
numeric_cols <- sapply(Data, is.numeric)
numeric_features <- names(numeric_cols)[numeric_cols]
numeric_features <-  numeric_features[-9] #we exclude the target variable
# We set the layout for the plots
par(mfrow = c(3, 3), mar=c(2,2,4,1))
#We create box plots for all numerical features
for (i in 1:length(numeric_features)) {
  feature <- numeric_features[i]
  boxplot(Data[[feature]], main = feature, ylab = "Value",horizontal= TRUE)
}

# Remove OUTLIERS using interquartiles
#looking for outliers
outlier_cols <- c('area', 'monthly.tax','property.tax','fire.insurance','rent.amount','parking.spaces','rooms','floor')
brz_col <- brz_data[, outlier_cols]
q1 <- apply(brz_col, 2, quantile, probs = 0.25, na.rm = TRUE)
q3 <- apply(brz_col, 2, quantile, probs = 0.75, na.rm = TRUE)
iqr <- q3 - q1
upper <- q3 + 1.5 * iqr
lower <- q1 - 1.5 * iqr
outliers <- apply(brz_col, 1, function(x) any(x < lower | x > upper, na.rm = TRUE))
# Identify outliers rows
outlier_rows <- row.names(brz_col)[outliers] # Print the number of outliers detected
cat("Number of initial outliers detected:", length(outlier_rows), "\n")
# Drop rows with outliers
#Cleaned Data is our new dataframe containing all independent variables encoded and without outliers 
brz_cleaned <- brz_data[!outliers,]

# We select numeric features
numeric_cols <- sapply(brz_cleaned, is.numeric)
numeric_features <- names(numeric_cols)[numeric_cols]
numeric_features <-  numeric_features[-9] #we exclude the target variable
# Set the layout for the plots
par(mfrow = c(3, 3), mar=c(2,2,4,1))
# Create box plots for all numerical features
for (i in 1:length(numeric_features)) {
  feature <- numeric_features[i]
  boxplot(brz_cleaned[[feature]], main = feature, ylab = "Value",horizontal= TRUE)
}

#CATEGORICAL VARIABLES
#This graph shows the mean rent amount and how it changes for every city 
brz_cleaned %>%
  group_by(city) %>%
  mutate(mean_by_city = mean(rent.amount)) %>%
  ungroup() %>%
  mutate(city = fct_reorder(city, mean_by_city)) %>%
  ggplot(aes(city, rent.amount, colour = city,
             show.legend = F)) +
  coord_flip() +
  geom_jitter(show.legend = F,
              size = 4,
alpha = 0.2,
              width = 0.05) +
  stat_summary(fun = mean, geom = "point", size = 8, show.legend = F) +
  geom_hline(aes(yintercept = mean(rent.amount)),
             colour = "gray70",
             size = 0.9) +
  geom_segment(aes(x = city, xend = city,
                   y = mean(rent.amount), yend = mean_by_city),
               size = 2, show.legend = F) +
  labs(title = "Rent amount of houses by City",
       x = "City",
       y = "Rent amount of houses") +
  theme(legend.position = "none") +
  theme_bw()


#Bar plot for the different categories
plot_bar(brz_cleaned,ncol = 1)

# Create the box plot for rent by city with colors
rent_city <- brz_cleaned %>%
  group_by(city) %>%
  ggplot(aes(x = city, y = rent.amount, fill = city)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw() +
  ggtitle("Rent by City") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the box plot for rent by furniture with colors
rent_furniture <- brz_cleaned %>%
  group_by(furniture) %>%
  ggplot(aes(x = furniture, y = rent.amount, fill = furniture)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw() +
  ggtitle("Rent by Furniture") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the box plot for rent by animal with colors
rent_animal <- brz_cleaned %>%
  group_by(animal) %>%
  ggplot(aes(x = animal, y = rent.amount, fill = animal)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw() +
  ggtitle("Rent by Animal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Arrange the plots in a grid
grid.arrange(rent_city, rent_furniture, rent_animal, ncol = 2)

#NUMERICAL VARIABLES: DISCRETE
Let's now take into consideration discrete variables how are the values distributed for each feature.
Let's use a barplot:
discrete_feat = c("floor","parking.spaces","rooms","bathroom")
p_list <- list()
for (x in discrete_feat){
  p <- plot_bar(as.factor(brz_cleaned[,x]), title=x, order_bar = TRUE)
  p_list[x] <- p
}
# Arrange the plots in a grid
grid.arrange(grobs = p_list, ncol = 2)
#brz_cleaned is the dataset with no outliers
#datset containing onlu numerical variables
num_v = subset(brz_cleaned, select = -c(city,animal,furniture))
# Distribution of the response variable according to the parking spaces
parking = num_v %>% group_by(parking.spaces) %>% ggplot(aes(parking.spaces, rent.amount, group=parking.spaces)) + geom_boxplot() + scale_y_log10()+ theme_bw() + ggtitle("Rent by parking space")
#Distribution of response variable according to the number of rooms
#bigger number of rooms, higher rent value
room<- num_v %>% group_by(rooms) %>%  ggplot(aes(rooms, rent.amount, group= rooms)) + geom_boxplot() +  scale_y_log10()+ theme_bw() +ggtitle("Rent by room number")
#Distribution of response variable according to the number of floors
floors <- num_v %>% group_by(floor) %>%  ggplot(aes(floor, rent.amount, group= floor)) + geom_boxplot() +  scale_y_log10()+ theme_bw() +ggtitle("Rent by floor number")
#Distribution of response variable according to the number of bathrooms 
bath <- num_v %>% group_by(bathroom) %>%  ggplot(aes(bathroom, rent.amount, group= bathroom)) + geom_boxplot() +  scale_y_log10()+ theme_bw() +ggtitle("Rent by bathroom  number")
grid.arrange(parking, room,floors,bath, ncol = 2)

#NUMERICAL VARIABLES: CONTINOUS
continous_feat = c("area", "monthly.tax", "property.tax", "fire.insurance")
par(mfrow = c(1,4))
for(i in continous_feat){
  hist(brz_cleaned[,i], freq = F, main = names (brz_cleaned[i]),col = rgb(.7,.7,.7), border = "white", xlab = "")
  
  abline(v = mean(brz_cleaned[,i]), lwd = 2)
  abline(v = median(brz_cleaned[,i]), lwd = 2, col = rgb(.7,0,0))
  legend("topright", c("Mean", "Median"), lwd = 2, col = c(1, rgb(.7,0,0)),cex = .8, bty = "o", border = 'black')
}

#Scaling Data
variables_to_scale <- c("area", "monthly.tax", "property.tax", "fire.insurance")
scaled_var <- scale(brz_cleaned[, variables_to_scale])
finaldata <- brz_cleaned 
finaldata[, variables_to_scale] <- scaled_var
#finaldata is the scaled dataset with no outliers and with scaled variables <-final version

#CORRELATION MATRIX
#we used the numerical data scaled
#correlation matrix and heatmap, the data are scaled in the heatmaply() function 
plot_correlation(brz_cleaned, maxcat =1L, theme_config = list(legend.position = "right", axis.text.x = element_text(angle = 90)))

#Create a scatterplot to plot relation between fire.insurance and rent.amount
scatterplot_insurance<-ggplot(finaldata, aes(x = fire.insurance, y = rent.amount)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color= 'red') +
  xlab("fire.insurance") +
  ylab("rent.amount") +
  ggtitle("Fire Insurance vs Rent Amount")
#Create a scatterplot to plot relation between area and rent.amount
scatterplot_area<-ggplot(brz_cleaned, aes(x = area, y = rent.amount)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color= 'red') +
  xlab("area") +
  ylab("rent.amount") +
  ggtitle("Area vs Rent Amount")
#Arrange plot in a grid
grid.arrange(scatterplot_insurance, scatterplot_area, ncol = 2)


## 4. Lower-dimensional model

#Splitting the dataset
set.seed(123)
idx <- sample.int(n = nrow(finaldata), size = floor(.75*nrow(finaldata)), replace = F)  
test_set <- finaldata[-idx,]
x <- finaldata[,1:11]
y <- finaldata$rent.amount
# Training set
x_train <- x[idx, ] 
y_train <- y[idx]
# Test set
x_test <- x[-idx, ]
y_test <- y[-idx]
brz_train_sc <- x_train
brz_train_sc$rent.amount <- y_train #we insert rent.amount in the training set because we need it to train the model
#while is important instead that y_test is not in x_test

#Linear Regression with 'fire.insurance'
#we train the model with brz_train_sc
linear_model <- lm(rent.amount ~ fire.insurance, brz_train_sc)
summary(linear_model)
linear_prediction <- predict(linear_model, newdata = x_test)
rmse_linear = sqrt(mean((y_test - linear_prediction)^2))
cat("RMSE with linear model on test set:", rmse_linear)

#Linear Regression with 'area'
linear_model <- lm(rent.amount ~ area, brz_train_sc)
summary(linear_model)
linear_prediction <- predict(linear_model, newdata = x_test)
rmse_linear = sqrt(mean((y_test - linear_prediction)^2))
cat("RMSE with linear model on test set:", rmse_linear)

#Multiple regression: fire.insurance + area
#Building the models
#We start building the model with predictors: area and fire.insurance
multiple_linear_model <- lm(rent.amount ~ fire.insurance + area , data = brz_train_sc)
summary(multiple_linear_model)
multiple_linear_model_cv <- predict(multiple_linear_model, newdata = test_set)
s3d <- scatterplot3d(x_test$area, x_test$fire.insurance, y_test,  xlab = 'Area', ylab = 'Fire Insurance', zlab = 'Rent Amount', angle = 250, color = 'blue')
s3d$points3d(x_test$area, x_test$fire.insurance, multiple_linear_model_cv, col = 'red')
rmse_mll = sqrt(mean((y_test - multiple_linear_model_cv)^2))

# Calculate R-squared 
r_squared <- summary(multiple_linear_model)$r.squared
# Calculate adjusted R-squared
n <- length(finaldata$rent.amount)
p <- length(coef(multiple_linear_model)) - 1  # Number of predictors (excluding intercept)
adjusted_r_squared <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))
# Print the evaluation metrics
cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adjusted_r_squared, "\n")

# Calculate R-squared on the training set
predicted_train <- predict(multiple_linear_model, data = x_train)
# Calculate root mean squared error (RMSE) on TRAIN SET
actual_train <- y_train
rmse_train <- sqrt(mean((actual_train - predicted_train)^2))
cat("RMSE on train set:", rmse_train, "\n")

#Let's predict on test set
predicted_test <- predict(multiple_linear_model, newdata = x_test)
# Calculate root mean squared error (RMSE) on TEST SET
actual_test <- y_test
rmse_test <- sqrt(mean((actual_test- predicted_test)^2))
cat("RMSE on the test data:", rmse_test, "\n")

## 5. Complex models
#Multiple linear regression model using all predictors
#multiple linear regression with all the variables
all_multi_linear_mod = lm(rent.amount~., data = brz_train_sc)
# predictions on the train set with previous model
all_multi_linear_pred_train = predict(all_multi_linear_mod, newdata = brz_train_sc)
all_ml_RMSE_train = sqrt(mean((y_train - all_multi_linear_pred_train)^2))
R2_all_ml = summary(all_multi_linear_mod)$r.squared
cat('R-Squared with multiple regression: ', R2_all_ml)
cat('\nRMSE (train set) with multiple regression:', all_ml_RMSE_train)
# predictions on the test set with previous model
all_multi_linear_pred = predict(all_multi_linear_mod, newdata = test_set)
all_ml_RMSE = sqrt(mean((y_test - all_multi_linear_pred)^2))
cat('\nRMSE (test set) with multiple regression:', all_ml_RMSE)

# AIC 
multi_lin_aic <- step(lm(brz_train_sc$rent.amount ~ 1,
                         data = brz_train_sc), scope = formula(all_multi_linear_mod),
                      direction = "forward")

aic_coeff = sort(coefficients(multi_lin_aic))

# Comparison based on deviance test (Chi-square test) for nested models
anova = anova(all_multi_linear_mod, multi_lin_aic, test = "Chisq") 
aic_pred = predict(multi_lin_aic, newdata = test_set)
aic_RMSE = sqrt(mean((y_test - aic_pred)^2))
cat('RMSE with aic:', aic_RMSE)


#Elastic Net
set.seed(21)
#Cross validation
tr_contr = trainControl(method  = "cv",
                        number  = 10) # number of folds
en <- train(rent.amount~.,
            brz_train_sc,
            method='glmnet',
            # alpha and lambda parameters tuning
            tuneGrid =expand.grid(alpha=seq(0,1,length=10), 
                                  lambda = seq(0.0001,0.2,length=5)),
            trControl=tr_contr) # cross validation
tr_RMSE = mean(en$resample$RMSE) #RMSE on train set
en_pred = predict(en, newdata = test_set) #prediction
en_RMSE = sqrt(mean((y_test - en_pred)^2))
cat('RMSE (train set) with elastic net:', tr_RMSE,"\n")
cat('RMSE (test set) with elastic net:', en_RMSE)

# XGBoost
#we make x_train and x_test two matrix 
set.seed(123)
x_train_matrix <- data.matrix(x_train)
x_test_matrix  <- data.matrix(x_test)
xgb_train = xgb.DMatrix(data = x_train_matrix, label = y_train)
xgb_test = xgb.DMatrix(data =x_test_matrix, label = y_test)
params <- list(booster = "gbtree", objective = "reg:squarederror", gamma=0, max_depth=6, alpha = c(0,1))
xgbcv <- xgb.cv( params = params, data = xgb_train, nrounds = 300, nfold = 10,  metrics = 'rmse', print_every_n = 10, showsd = T)
xgb_model = xgboost(data = xgb_train, max.depth = 6, nrounds = which.min(xgbcv$evaluation_log$test_rmse_mean), verbose = 0, objective = "reg:squarederror")
xgb_pred = predict(xgb_model, xgb_test)
rmse_xgboost <- sqrt(mean((y_test - xgb_pred)^2))
#we print the RMSE
cat('RMSE with XGBoost:', rmse_xgboost)

### SVM
set.seed(123)
ctrl = trainControl(method  = "boot",
                    number  = 10)
svm_model <- train(
  rent.amount ~ .,
  data = brz_train_sc,
  method = 'svmRadial',
  trCtrl = ctrl
)
svm_pred_cv <- predict(svm_model, newdata = x_test)
#RMSE SVM = 369.2112
rmse_svm_cv = sqrt(mean((y_test - svm_pred_cv)^2))
cat("RMSE with SVM",rmse_svm_cv)

#Random forest using all the predictors
set.seed(123)
# random forest with cross validation 
#(tried with oob tuning but RMSE doesn't change)
rf_fit <- randomForest(brz_train_sc$rent.amount ~ ., trControl  =
                         tr_cont, data = brz_train_sc)
rf_pred = predict(rf_fit, newdata = test_set)
rf_RMSE = sqrt(mean((y_test - rf_pred)^2))
cat('RMSE with classic random forest:', rf_RMSE)

## Random Forest based on importance
# Train a Random Forest model
model <- randomForest(brz_train_sc$rent.amount ~ ., data = brz_train_sc )
importance <- data.frame(importance(model))
importance<- data.frame(Variable = row.names(importance), IncNodePurity =importance$IncNodePurity)
# Print the variable importance scores
print(importance)

# Plot variable importance
varImpPlot(model)
set.seed(132)
# random forest with cross validation 
rf_fit2 <- randomForest(rent.amount ~  fire.insurance + area + property.tax + monthly.tax + parking.spaces + rooms, trControl  = tr_cont, data = brz_train_sc)
rf_pred2 = predict(rf_fit2, newdata = test_set)
rf_RMSE2 = sqrt(mean((y_test - rf_pred2)^2))
cat('RMSE with importance feature selection:', rf_RMSE2)

## 7. Clustering
#we scale the data
sc_num_v<- data.frame(scale(num_v[ , c('area', 'monthly.tax', 'property.tax', 'fire.insurance', 'rent.amount')]))
#add the other variables to the dataset
sc_num_v$rooms <- num_v$rooms
sc_num_v$bathroom <- num_v$bathroom
sc_num_v$parking.spaces <- num_v$parking.spaces
sc_num_v$floor <- num_v$floor

## K-Means
#elbow rule plot
fviz_nbclust(sc_num_v, kmeans, method = "wss")+
  labs(subtitle = "WSS - Elbow method")
# avg silhouette plot
fviz_nbclust(sc_num_v, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

set.seed(123)
# k-means with 2 clusters
km_1 = kmeans(sc_num_v, 2, nstart = 1, iter.max = 1e2)
s_i1 = silhouette(km_1$cluster, 
                  dist(sc_num_v))
# k-means with 3 clusters
km_2 = kmeans(sc_num_v, 3, nstart = 1, iter.max = 1e2)
s_i2 = silhouette(km_2$cluster, 
                  dist(sc_num_v))
a = fviz_silhouette(s_i1, main = 'Kmean with 2 cluster', print.summary = FALSE)
b = fviz_silhouette(s_i2, main = 'Kmean with 3 cluster', print.summary = FALSE)
grid.arrange(a,b, ncol = 2)

#Total within-cluster sum of squares (WSS) values
cat("WSS values with 2 clusters", km_1$tot.withinss, "\n")
cat("WSS values with 3 clusters", km_2$tot.withinss)

#K-means visualization
fviz_cluster(km_1, sc_num_v, main = "K-means 2 clusters")

## Hierarchical clustering methods

#Correlation based method
# Convert all columns to numeric
sc_num_v[] <- lapply(sc_num_v, as.numeric)
# Calculate correlation matrix
correlation_matrix <- cor(t(sc_num_v))
d2= as.dist(1 - correlation_matrix)
h2 = hclust(d2, method="ward.D2")
plot(h2)

#Distance based method
dist_euc <- dist(sc_num_v, method = "euclidean")
hc_euclidean <- hclust(dist_euc, method = "ward.D2")
hc1_euclidean <- hclust(dist_euc, method = "average")
plot(hc_euclidean, cex = 0.6, main = "Dendrogram (Euclidean distance with ward.D2 method)")
plot(hc1_euclidean, cex = 0.6, main = "Dendrogram (Euclidean distance with average method)")
#draws a colorfull rectangle around the two clusters identified by the cutting threshold 
plot(hc_euclidean, cex = 0.6, main = "Dendrogram (Euclidean distance with ward method)")
rect.hclust(hc_euclidean, k = 2, border = 2:6) 
#Cut the dendrogram into 3 clusters
groups <- cutree(hc_euclidean, k=2)

# Table data
proportions <- table(groups) / sum(table(groups))
labs <- sprintf("%s\n%2f", c("Cluster 1", "Cluster 2"), proportions)
# Create Pie Chart
pie(table(groups), labels= labs , col = rainbow(length(table(groups))), 
        main = "Pie chart")
#find number of observations in each cluster
table(groups)

#append cluster labels to original data
final_data <- cbind(sc_num_v, cluster = groups)
#find mean values for each cluster
aggregate(final_data, by=list(cluster=final_data$cluster), mean)

## 8. Conclusions on Clustering
print("The cluster's proportion for ward.D2 method:")
table(final_data$cluster)/8250
print("The cluster's proportion for for k-means:")
table(km_1$cluster)/8250

sc_num_v$city <- finaldata$city
# Create the contingency table with k-means
cont_table_kmeans <- table(km_1$cluster, sc_num_v$city)
# Create the contingency table with hierarchical clustering
cont_table_hierarchical <- table(final_data$cluster, sc_num_v$city)
# Set up the plotting layout with two plots side by side
par(mfrow = c(1, 2))
# Plot the barplot for k-means
barplot(cont_table_kmeans, beside = TRUE, legend = rownames(cont_table_kmeans),
        args.legend = list(x = "topleft"),
        main = "K-means Contingency Table",
        xlab = "K-means Cluster", ylab = "Frequency", col = rainbow(2))

# Plot the barplot for hierarchical clustering
barplot(cont_table_hierarchical, beside = TRUE, legend = rownames(cont_table_hierarchical),
        args.legend = list(x = "topleft"),
        main = "Hierarchical Contingency Table",
        xlab = "Hierarchical Cluster", ylab = "Frequency", col = rainbow(2))

#Adjusted Rand Index
print('The adjusted Rand index for the k_means is:')
adjustedRandIndex(km_2$cluster, sc_num_v$city)
print('The adjusted Rand index for the ward.D2 method is:')
adjustedRandIndex(final_data$cluster, sc_num_v$city)













