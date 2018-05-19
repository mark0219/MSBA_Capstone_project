rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}


needed <- c("stringr", 
            "ggplot2", 
            "GGally", 
            "glmnet", 
            "boot", 
            "ISLR",
            "leaps",
            "doParallel")
installIfAbsentAndLoad(needed)


###################
#Read files into R#
###################
competitors <- read.csv("COMPETITORS - Tiger Data 201703.csv", sep = ',', stringsAsFactors = F)
sales_cust_type <- read.csv("Sales_GP_CustomerType.csv", sep = ',', stringsAsFactors = F)
showroom_db <- read.csv("Showroom_Registry_Database with Total Rentable Square Footage.csv", sep = ',', stringsAsFactors = F)
zip_demography <- read.csv("DemographicsZip.csv", sep = ',', stringsAsFactors = F)
married_owned <- read.csv("MarriedOwned.csv", sep = ',', stringsAsFactors = F)
demand_adjusted <- read.csv("MarketDemandGrowth.csv", sep = ',', stringsAsFactors = F)
msa_demand <- read.csv("D:/Cap_Stone/work_space/external_data/marketareademand.csv", sep = ',', stringsAsFactors = F, header = F)


################
#Data Cleansing#
################


#Showroom Registry Database table
db_features_subset <- c('Date.Created', 
                        'Location.Directory...Showroom', 
                        'Rentable.Area', 
                        'Actual.Showroom.Square.Footage', 
                        'Location.Directory...Zip')
showroom_db <- showroom_db[,db_features_subset]
colnames(showroom_db)[2] <- 'Showroom.Loc.ID'
colnames(showroom_db)[5] <- 'ZIP'
showroom_db$Date.Created <- as.Date(showroom_db$Date.Created, '%m/%d/%Y')
showroom_db$Rentable.Area <- as.numeric(gsub(',', '', showroom_db$Rentable.Area))
showroom_db$Actual.Showroom.Square.Footage <- as.numeric(gsub(',', '', showroom_db$Actual.Showroom.Square.Footage))
showroom_db[nchar(showroom_db$ZIP) > 5 ,]$ZIP <- substring(showroom_db[nchar(showroom_db$ZIP) > 5 ,]$ZIP, 1, 5)
showroom_db$ZIP <- as.integer(showroom_db$ZIP)
immature_ids <- showroom_db[showroom_db$Date.Created >= '2014-02-01',]$Showroom.Loc.ID
showroom_db <- showroom_db[!showroom_db$Showroom.Loc.ID %in% immature_ids,]


#Competitor table
comp_features_subset <- c('KOB...RES.BUILD', 
                         'KOB...LIGHTING', 
                         'KOB...APPLIANCE', 
                         'ZIP5', 
                         'Zip', 
                         'Employees.At.This.Location')
competitors <- competitors[,comp_features_subset]
for (i in 1:3) {
  competitors[,i][competitors[,i] == 'x' | competitors[,i] == 'X'] <- 1
  competitors[,i][competitors[,i] != 1] <- 0
}
competitors[competitors$ZIP5 == '' & competitors$Zip != '',]$ZIP5 <- competitors[competitors$ZIP5 == '' & competitors$Zip != '',]$Zip
competitors$Zip <- NULL
competitors <- competitors[nchar(competitors$ZIP5) >= 3 & nchar(competitors$ZIP5) <= 5,]
names(competitors)[4] <- 'ZIP'
#Expect one warning occur when converting # of employee column to numeric due to NAs
competitors[,c(1,2,3,4,5)] <- sapply(competitors[,c(1,2,3,4,5)], as.numeric)
competitors$count <- 0
competitors[competitors$KOB...RES.BUILD != 0 | competitors$KOB...LIGHTING != 0 | competitors$KOB...APPLIANCE !=0,]$count <- 1
competitors <- aggregate(competitors[,c(1,2,3,5,6)], by = list(ZIP = competitors$ZIP), FUN = sum)
competitors[is.na(competitors$Employees.At.This.Location),]$Employees.At.This.Location <- 0

#Sales GP table
sales_features_subset <- c('Yearmonth',
                          'Sell.Whse.ID',
                          'Current.Net.Billing.Amount')
sales_cust_type <- sales_cust_type[,sales_features_subset]
sales_cust_type <- sales_cust_type[!sales_cust_type$Sell.Whse.ID %in% immature_ids,]
sales_cust_type[sales_cust_type$Current.Net.Billing.Amount == ' -   ',]$Current.Net.Billing.Amount <- 0
sales_cust_type[substr(sales_cust_type$Current.Net.Billing.Amount, 1, 2) == ' (',]$Current.Net.Billing.Amount <- 0
sales_cust_type[,3] <- as.numeric(gsub(',', '', sales_cust_type[,3]))
sales_cust_type$Year <- 1
sales_cust_type[sales_cust_type$Yearmonth > 201502 & sales_cust_type$Yearmonth <= 201602,]$Year <- 2
sales_cust_type[sales_cust_type$Yearmonth > 201602 & sales_cust_type$Yearmonth <= 201702,]$Year <- 3
sales_cust_type <- aggregate(sales_cust_type[,3], by = list(Year = sales_cust_type$Year,
                                                            Showroom.Loc.ID = sales_cust_type$Sell.Whse.ID), FUN = sum)
colnames(sales_cust_type)[3] <- 'Revenue'


#msa_demand table
colnames(msa_demand)[1] <- 'ZIP'
colnames(msa_demand)[2] <- 'demand.2016'
msa_demand <- msa_demand[-4210,]
msa_demand[,1] <- as.numeric(msa_demand[,1])


#########################
#Merging Tables Together#
#########################


#Sales <= ShowroomDB
DB_Sales <- merge(sales_cust_type, showroom_db, by = 'Showroom.Loc.ID', all.x = T)

#Sales <= Competitors
DB_Sales <- merge(DB_Sales, competitors, by = 'ZIP', all.x = T)

#Sales <= Zip_Demography
names(zip_demography)[1] <- 'ZIP'
DB_Sales <- merge(DB_Sales, zip_demography, by = 'ZIP', all.x = T)

#Sales <= Married_Owned
colnames(married_owned)[1] <- 'ZIP'
DB_Sales <- merge(DB_Sales, married_owned, by = 'ZIP', all.x = T)

#Sales <= Msa_demand
DB_Sales <- merge(DB_Sales, msa_demand, by = 'ZIP', all.x = T)
DB_Sales$demand.2015 <- DB_Sales$demand.2016 / 1.06
DB_Sales$demand.2014 <- DB_Sales$demand.2015 / 1.06


#####################
#Predictive Modeling#
#####################


#Cleanse the final table will be used for features selection
columns_to_exclude <- c('ZIP',
                        'Showroom.Loc.ID',
                        'Date.Created',
                        'Showroom.Designation',
                        'Showroom.Type',
                        'Appliance.Vendors',
                        'Lighting.Vendors',
                        'KOB...RES.BUILD',
                        'KOB...LIGHTING',
                        'KOB...APPLIANCE',
                        'MeanHHIncome',
                        'Service.level',
                        'Employees.At.This.Location')
DB_Sales <- DB_Sales[, -which(names(DB_Sales) %in% columns_to_exclude)]
#Expect three warnings occurs when converting columns with NAs to numeric
DB_Sales[,1:ncol(DB_Sales)] <- sapply(DB_Sales[,1:ncol(DB_Sales)], as.numeric)
DB_Sales <- na.omit(DB_Sales)
rownames(DB_Sales) <- NULL
DB_Sales$Demand_Merged <- NA
DB_Sales[DB_Sales$Year == 1,]$Demand_Merged <- DB_Sales[DB_Sales$Year == 1,]$demand.2014
DB_Sales[DB_Sales$Year == 2,]$Demand_Merged <- DB_Sales[DB_Sales$Year == 2,]$demand.2015
DB_Sales[DB_Sales$Year == 3,]$Demand_Merged <- DB_Sales[DB_Sales$Year == 3,]$demand.2016
DB_Sales <- DB_Sales[,-c(28,29,30)]


# #Adding calculated fields
# DB_Sales$Service.Level <- DB_Sales$Revenue + DB_Sales$Employees.At.This.Location * 800000
# DB_Sales$Service.Ratio <- DB_Sales$Service.Level / DB_Sales$Demand_Merged
# under_served_area <- DB_Sales[DB_Sales$Service.Ratio < 1 & DB_Sales$Service.Ratio > 0 & DB_Sales$Year == 3,]
# lm_fit <- lm(Service.Ratio ~ count + Demand_Merged, data = DB_Sales)
# summary(lm_fit)

# #Perform features selection on whole dataset
#Customize a predict function


# 
# #Whole Dataset
# Data_3Years <- DB_Sales[,-1]
# for (j in 1:k) {
#   best.fit <- regsubsets(Revenue ~ ., data = Data_3Years[folds != j,], nvmax = 26)
#   for (i in 1:26) {
#     pred <- predict.regsubset(best.fit, Data_3Years[folds == j,], id = i)
#     cv.errors[j,i] <- mean((Data_3Years$Revenue[folds == j] - pred)^2)
#   }
# }
# mean.cv.errors <- apply(cv.errors, 2, mean)
# plot(mean.cv.errors, type = 'b', pch = 16)
# best.num.vars <- which.min(mean.cv.errors)
# fit.best <- regsubsets(Revenue ~ ., data = Data_3Years, nvmax = 26)
# best.var.names.3Y <- names(coef(fit.best, best.num.vars))[-1]
# best.var.names.3Y
# 
# #Remove one with high correlation with features and low correlation with response
# Data_subset <- data.frame(Revenue = DB_Sales$Revenue,
#                           DB_Sales[,best.var.names.3Y])
# features_to_exclude <- c('Homes...Built.1970.to.1979',
#                          'Homes...Built.1960.to.1969',
#                          'Homes...Built.1980.to.1989',
#                          'Homes...Built.2010.to.2013',
#                          'MalePop',
#                          'FemalePop',
#                          'MedianAge',
#                          'MarriedOwnedHomes',
#                          'CurrentResident...Moved.in.1980.to.1989',
#                          'MedianHHIncome',
#                          'Homes...Built.1950.to.1959')
# Data_subset <- Data_subset[,-which(colnames(Data_subset) %in% features_to_exclude)]



#Perform features selection on each of three data subset by year
# predict.regsubset <- function (object, newdata, id) {
#   form <- as.formula(object$call[[2]])
#   mat <- model.matrix(form, newdata)
#   coefi <- coef(object, id = id)
#   xvars = names(coefi)
#   mat[,xvars] %*% coefi
# }
grid <- 10 ^ seq(10, -2, length = 100)
names_list <- list()
# k <- 10
# folds <- sample(1:k, nrow(DB_Sales[DB_Sales$Year == 1,]), replace = T)
# cv.errors <- matrix(NA, k, 26, dimnames = list(NULL, paste(1:26)))
# #Year 1
# Data_Year_1 <- DB_Sales[DB_Sales$Year == 1, -1]
# #Leaps - Best Subset Selection
# processing_time <- system.time({
#   for (j in 1:k) {
#     best.fit <- regsubsets(Revenue ~ ., data = Data_Year_1[folds != j,], nvmax = 26)
#     for (i in 1:26) {
#       pred <- predict.regsubset(best.fit, Data_Year_1[folds == j,], id = i)
#       cv.errors[j,i] <- mean((Data_Year_1$Revenue[folds == j] - pred)^2)
#     }
#   }
# })
# processing_time
# mean.cv.errors <- apply(cv.errors, 2, mean)
# plot(mean.cv.errors, type = 'b', pch = 16)
# best.num.vars <- which.min(mean.cv.errors)
# fit.best <- regsubsets(Revenue ~ ., data = Data_Year_1, nvmax = 26)
# best.var.names.Y1 <- names(coef(fit.best, best.num.vars))[-1]
# best.var.names.Y1
#Lasso
for (i in 1:10) {
  X <- DB_Sales[DB_Sales$Year == 1,-c(1,2)]
  y <- DB_Sales[DB_Sales$Year == 1,]$Revenue
  Ls.train <- glmnet(as.matrix(X), y, lambda = grid, alpha = 1, family = 'gaussian')
  Ls.cv.output <- cv.glmnet(as.matrix(X), y, alpha = 1, family = 'gaussian')
  best.lambda <- Ls.cv.output$lambda.min
  Ls.coefs <- predict(Ls.train, type = 'coefficients', s = best.lambda)[,1]
  coef_names <- names(Ls.coefs[Ls.coefs!=0][-1])
  names_list[[i]] <- coef_names
}
varnames_final_Y1 <- Reduce(intersect, names_list)


#Year 2
# Data_Year_2 <- DB_Sales[DB_Sales$Year == 2, -1]
# #Leaps - Best Subset Selection
# processing_time <- system.time({
#   for (j in 1:k) {
#     best.fit <- regsubsets(Revenue ~ ., data = Data_Year_2[folds != j,], nvmax = 26)
#     for (i in 1:26) {
#       pred <- predict.regsubset(best.fit, Data_Year_2[folds == j,], id = i)
#       cv.errors[j,i] <- mean((Data_Year_2$Revenue[folds == j] - pred)^2)
#     }
#   }
# })
# processing_time
# mean.cv.errors <- apply(cv.errors, 2, mean)
# plot(mean.cv.errors, type = 'b', pch = 16)
# best.num.vars <- which.min(mean.cv.errors)
# fit.best <- regsubsets(Revenue ~ ., data = Data_Year_2, nvmax = 26)
# best.var.names.Y2 <- names(coef(fit.best, best.num.vars))[-1]

#Lasso
for (i in 1:10) {
  X <- DB_Sales[DB_Sales$Year == 2,-c(1,2)]
  y <- DB_Sales[DB_Sales$Year == 2,]$Revenue
  Ls.train <- glmnet(as.matrix(X), y, lambda = grid, alpha = 1, family = 'gaussian')
  Ls.cv.output <- cv.glmnet(as.matrix(X), y, alpha = 1, family = 'gaussian')
  best.lambda <- Ls.cv.output$lambda.min
  Ls.coefs <- predict(Ls.train, type = 'coefficients', s = best.lambda)[1:26,]
  Ls.coefs[Ls.coefs!=0][-1]
  coef_names <- names(Ls.coefs[Ls.coefs!=0][-1])
  names_list[[i]] <- coef_names
}
varnames_final_Y2 <- Reduce(intersect, names_list)


#Year 3
# Data_Year_3 <- DB_Sales[DB_Sales$Year == 3, -1]
# #Leaps - Best Subset Selection
# processing_time <- system.time({
#   for (j in 1:k) {
#     best.fit <- regsubsets(Revenue ~ ., data = Data_Year_3[folds != j,], nvmax = 26)
#     for (i in 1:26) {
#       pred <- predict.regsubset(best.fit, Data_Year_3[folds == j,], id = i)
#       cv.errors[j,i] <- mean((Data_Year_3$Revenue[folds == j] - pred)^2)
#     }
#   }
# })
# processing_time
# mean.cv.errors <- apply(cv.errors, 2, mean)
# plot(mean.cv.errors, type = 'b', pch = 16)
# best.num.vars <- which.min(mean.cv.errors)
# fit.best <- regsubsets(Revenue ~ ., data = Data_Year_3, nvmax = 26)
# best.var.names.Y3 <- names(coef(fit.best, best.num.vars))[-1]

#Lasso
for (i in 1:10) {
  X <- DB_Sales[DB_Sales$Year == 3,-c(1,2)]
  y <- DB_Sales[DB_Sales$Year == 3,]$Revenue
  Ls.train <- glmnet(as.matrix(X), y, lambda = grid, alpha = 1, family = 'gaussian')
  Ls.cv.output <- cv.glmnet(as.matrix(X), y, alpha = 1, family = 'gaussian')
  best.lambda <- Ls.cv.output$lambda.min
  Ls.coefs <- predict(Ls.train, type = 'coefficients', s = best.lambda)[1:26,]
  Ls.coefs[Ls.coefs!=0][-1]
  coef_names <- names(Ls.coefs[Ls.coefs!=0][-1])
  names_list[[i]] <- coef_names
}
varnames_final_Y3 <- Reduce(intersect, names_list)

#Extract final result of variable selection (Lasso)
common_variables <- intersect(intersect(varnames_final_Y1, varnames_final_Y2), varnames_final_Y3)
common_variables <- c(common_variables, 'Demand_Merged')
Data_subset <- data.frame(Revenue = DB_Sales$Revenue,
                          DB_Sales[,common_variables])
Data_subset <- data.frame(Data_subset[,-1],
                          Revenue = Data_subset$Revenue)

#PCA
#pca.output <- prcomp(DB_Sales[DB_Sales$Year == 3, common_variables])
#biplot(pca.output, ylim = c(-0.2, 0.3))


#Extract final result of variable selection (Best)
# best_variables <- intersect(intersect(best.var.names.Y1, best.var.names.Y2), best.var.names.Y3)
# best_variables <- c(best_variables, 'Demand_Merged')
# Data_subset <- data.frame(Revenue = DB_Sales$Revenue,
#                           DB_Sales[,best_variables])

rownames(Data_subset) <- NULL

#ggpairs(Data_subset, upper = list(continuous = wrap("cor", size = 8)))

#ggcorr(Data_subset, label = T, label_size = 2, label_round = 2, label_alpha = T, hjust = 1, size = 3)

#Initial fit for Linear Regression
lm.fit <- glm(Revenue ~ ., data = Data_subset)
summary(lm.fit)
plot(lm.fit)
cv.errors <- cv.glm(glmfit = lm.fit, data = as.data.frame(Data_subset))
cv.errors$delta[1]^0.5

# #Find detailed infor about outliers
# outliers_showrooms <- Data_subset[c(229,248,274),]


#Take out 3 outliers and fit Linear Regression again
Data_subset <- Data_subset[-c(229,248,274),]
lm.fit <- glm(Revenue ~ ., data = Data_subset)
summary(lm.fit)
cv.errors <- cv.glm(glmfit = lm.fit, data = Data_subset)
cv.errors$delta[1]^0.5


# #Coefficient Plot
# dwplot(list(lm.fit)) %>% 
#   relabel_predictors(c(Actual.Showroom.Square.Footage = "Showroom Square Footage",                       
#                        count  = "Count of Competitors", 
#                        MedianHomeValue = "Median Home Value", 
#                        CurrentResident...Moved.in.1980.to.1989    = "Resident Moved in 1980 - 1989", 
#                        Demand_Merged = "Demand")) +
#   theme_bw() + xlab("Coefficient Estimate") + ylab("") +
#   geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
#   ggtitle("Predicting Revenue") +
#   theme(plot.title = element_text(face="bold"))



#Tune the poly degree for Showroom Square Footage
tunning_errors <- c()
for (i in 1:15) {
  poly.fit <- glm(Revenue ~ poly(Actual.Showroom.Square.Footage, i), data = Data_subset)
  tunning_errors[i] <- cv.glm(poly.fit, data = Data_subset)$delta[1]^0.5
}
optimal.degree <- which.min(tunning_errors)
plot(tunning_errors, type = 'b', pch = 16, xlab = 'Polynomial Degree', ylab = 'Sqrt of LOOCV MSE')


#Plot Showroom Squared Footage as polynomial
poly.fit.plot <- glm(Revenue ~ poly(Actual.Showroom.Square.Footage, optimal.degree), data = Data_subset)
plot(Data_subset$Actual.Showroom.Square.Footage, Data_subset$Revenue, xlab = 'Showroom Square Footage', ylab = 'Revenue')
lines(sort(Data_subset$Actual.Showroom.Square.Footage),
      fitted(poly.fit.plot)[order(Data_subset$Actual.Showroom.Square.Footage)], col='red', type='l', lwd = 2)


# #Best Subset
# lm.poly.fit <- glm(Revenue ~ poly(Actual.Showroom.Square.Footage, optimal.degree, raw = T) +
#                      Rentable.Area +
#                      count + 
#                      FemalePop + 
#                      Homes.Built.2014.or.later + 
#                      CurrentResident...Moved.in.2015.or.later + 
#                      CurrentResident...Moved.in.1979.and.earlier +
#                      MedianHomeValue + 
#                      Demand_Merged, data = Data_subset)

#Final Multiple Regression with Polynomial
lm.poly.fit <- glm(Revenue ~ poly(Actual.Showroom.Square.Footage, optimal.degree, raw = T) +
                     count + 
                     MedianHomeValue + 
                     CurrentResident...Moved.in.1980.to.1989 + 
                     Demand_Merged, data = Data_subset)

confint(lm.poly.fit)


summary(lm.poly.fit)
plot(lm.poly.fit)
cv.errors <- cv.glm(glmfit = lm.poly.fit, data = Data_subset)
cv.errors$delta[1]^0.5


#Confidence Interval for predicted values
stdev <- cv.errors$delta[1]^0.5
plot(sort(lm.poly.fit$fitted.values), type = 'l', lwd = 2)
lines(sort(lm.poly.fit$fitted.values)+stdev, type = 'l', lwd = 1, col = 2)
lines(sort(lm.poly.fit$fitted.values)-stdev, type = 'l', lwd = 1, col = 2)


predframe <- data.frame(index = c(1:nrow(Data_subset)), predicted = sort(lm.poly.fit$fitted.values))
predframe$lwr = predframe$predicted - stdev
predframe$upr = predframe$predicted + stdev
predframe <- predframe[order(predframe$predicted, decreasing = F),]

ggplot(predframe, aes(index, predicted))+
  geom_point()+
  geom_line(data=predframe)+
  geom_ribbon(data=predframe,aes(ymin=lwr,ymax=upr),alpha = 0.2)


# #Coefficient Plot
# dwplot(list(lm.poly.fit)) %>% 
#   relabel_predictors(c(count  = "Count of Competitors", 
#                        MedianHomeValue = "Median Home Value", 
#                        CurrentResident...Moved.in.1980.to.1989    = "Resident Moved in 1980 - 1989", 
#                        Demand_Merged = "Demand")) +
#   theme_bw() + xlab("Coefficient Estimate") + ylab("") +
#   geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
#   ggtitle("Predicting Revenue") +
#   theme(plot.title = element_text(face="bold"))
# 
# 
# 
# pred_table <- data.frame(Predicted = lm.poly.fit$fitted.values, X = c(1:nrow(Data_subset)))
# 
# actual.pred <- data.frame(Actual = Data_subset$Revenue, Predicted = lm.poly.fit$fitted.values)
# actual.pred <- actual.pred[order(actual.pred$Actual, decreasing = F),]
# plot(actual.pred$Actual, actual.pred$Predicted, type = 'l')
# abline(lm(actual.pred$Predicted ~ actual.pred$Actual))
# 
# plot(actual.pred$Actual, actual.pred$Predicted, type = 'l')
# lines(actual.pred$Actual, actual.pred$Actual)
