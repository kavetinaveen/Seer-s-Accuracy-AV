###################################### AV - Seer's Accuracy ########################################

#... Setting working directory
####################################
filepath <- c("/Users/nkaveti/Documents/Kaggle/AV_Seers_Accuracy")
setwd(filepath)

#... Loading required packages
##################################
library(readr)

#... Reading data
####################
train <- read_csv("Train_seers_accuracy.csv")

train$trans_year <- as.numeric(paste0(20, format(train$Transaction_Date, "%y"))) # Adding year of transaction date

#... Frequency of each client
##################################
client_freq <- as.data.frame(table(train$Client_ID))
colnames(client_freq)[1] <- c("Client_ID")

#... Generating features based on inter-arrival times
#########################################################
clients <- unique(train$Client_ID)
train$DOB <- as.Date(train$DOB, "%d-%b-%y")
train$Transaction_Date <- as.Date(train$Transaction_Date, "%d-%b-%y")
inter_arrival_times <- do.call(rbind, lapply(clients, FUN = function(x){
  if(nrow(unique(train[train$Client_ID == x, "Transaction_Date"])) != 1){
    temp <- as.data.frame(train[train$Client_ID == x, "Transaction_Date"])
    temp <- sort(temp[,1])
    temp2 <- difftime(as.Date("2007-12-31"), temp[length(temp)])
    temp <- diff(temp)
    temp3 <- c(min(temp), max(temp), mean(temp), ifelse(is.na(var(temp)), 0, var(temp)), temp2)
    return(temp3)
  }else{
    temp <- unique(train[train$Client_ID == x, "Transaction_Date"])
    # cat(temp, "\n")
    temp <- difftime(as.Date("2007-12-31"), temp[[1]])
    temp2 <- c(rep(0, 4), temp)
    return(temp2)
  }
}))
colnames(inter_arrival_times) <- c("Min", "Max", "Mean", "Var", "diff_from_2007_dec")

#... Total amount purchased per client
############################################
money_info <- as.data.frame(train %>% group_by(Client_ID) %>% summarise(Total_Amount = sum(Transaction_Amount)))

#... Client demographic information (Age, Gender)
#####################################################
client_age <- unique(train[, c("Client_ID", "DOB")])
client_age$age <- difftime(as.Date("2007-12-31"), client_age$DOB, units = "days")/365
client_age$age[client_age$age < 0] <- client_age$age[client_age$age < 0] + 100
client_age$DOB <- NULL
#.... Gender
client_gender <- unique(train[, c("Client_ID", "Gender")])

#... Sum of masked variables
################################
client_mask <- train %>% group_by(Client_ID) %>% summarise(total_var1 = sum(Var1), total_var2 = sum(Var2), total_var3 = sum(Var3))


#.... Client information
client_info <- read_csv("client_info_2.0.csv")
# client_info <- data.frame(Client_ID = clients, inter_arrival_times)
client_info <- merge(client_info, money_info, by = "Client_ID")
client_info <- merge(client_info, client_age, by = "Client_ID")
client_info <- merge(client_info, client_freq, by = "Client_ID")
client_info <- merge(client_info, client_gender, by = "Client_ID")
client_info <- merge(client_info, client_mask, by = "Client_ID")

client_info$target <- ifelse(client_info$Max > 0 & client_info$Min <= 365, 1, 0)

write.csv(client_info, file = "client_info.csv", row.names = FALSE)

#... Experiment 1
#######################
client_info$Gender <- as.factor(client_info$Gender)
client_info$total_var1 <- as.factor(client_info$total_var1)
client_info$total_var2 <- as.factor(client_info$total_var2)
client_info$total_var3 <- as.factor(client_info$total_var3)

fit_glm <- glm(target ~ ., data = client_info[, -c(1:5)], family = quasibinomial("logit"), maxit = 100)

pred <- predict(fit_glm, client_info[, -1], type = "response")

result <- data.frame(Client_ID = client_info$Client_ID, Cross_Sell = pred)
result[is.na(result)] <- 0

#... Experiment 2
#######################
Cross_Sell <- client_info$diff_from_2007_dec + client_info$Total_Amount + client_info$Freq
Cross_Sell <- Cross_Sell/max(Cross_Sell)
result <- data.frame(Client_ID = client_info$Client_ID, Cross_Sell = Cross_Sell)

temp <- classIntervals(client_info$diff_from_2007_dec, n = 5, style = "quantile")
client_info$recency <- findInterval(client_info$diff_from_2007_dec, temp$brks)
client_info$recency[client_info$recency == 6] <- 1

temp <- classIntervals(client_info$Total_Amount, n = 5, style = "quantile")
client_info$money <- findInterval(client_info$Total_Amount, temp$brks)
client_info$money[client_info$money == 6] <- 1

# temp <- classIntervals(client_info$Freq[client_info$Freq != 1], n = 5, style = "quantile")
# client_info$frequency <- findInterval(client_info$Freq, temp$brks)
# client_info$frequency[client_info$frequency == 6] <- 1

# temp <- classIntervals(client_info$Freq[client_info$Freq != 1], n = 5, style = "quantile")
client_info$frequency <- client_info$Freq
client_info$frequency[client_info$frequency %in% c(3:5)] <- 3
client_info$frequency[client_info$frequency %in% c(6:10)] <- 4
client_info$frequency[client_info$frequency %in% c(11:19)] <- 5

client_info$rfm <- as.numeric(paste0(client_info$recency, client_info$money, client_info$frequency))
client_info$rfm2 <- client_info$rfm/max(client_info$rfm)

result2 <- data.frame(Client_ID = client_info$Client_ID, Cross_Sell = client_info$rfm2)

write.csv(result2, file = "submission_4.0.csv", row.names = FALSE)

#... Experiment 3
#####################
temp <- unique(as.data.frame(train[train$trans_year == 2006, "Client_ID"]))
temp2 <- unique(as.data.frame(train[train$trans_year != 2006, "Client_ID"]))
temp3 <- intersect(temp, temp2)

client_info$target <- ifelse(client_info$Client_ID %in% temp3$Client_ID, 1, 0)

temp4 <- unique(train[train$trans_year == 2006, c("Client_ID", "Transaction_Date")])
temp5 <- as.data.frame(table(temp4$Client_ID))
temp6 <- as.integer(as.character(temp5[temp5$Freq > 1, "Var1"]))
client_info[client_info$Client_ID %in% temp6, "target"] <- 1

train$Purchased_in_Sale[train$Purchased_in_Sale == "Y"] <- 1
train$Purchased_in_Sale[train$Purchased_in_Sale == "N"] <- 0
train$Purchased_in_Sale <- as.integer(train$Purchased_in_Sale)
temp7 <- as.data.frame(train %>% group_by(Client_ID) %>% summarise(prop_purchase_sale = mean(Purchased_in_Sale)))

temp8 <- as.data.frame(train %>% group_by(Client_ID) %>% summarise(Total_Num_EMI = sum(Number_of_EMI)))


client_info <- merge(client_info, temp7, by = "Client_ID")
client_info <- merge(client_info, temp8, by = "Client_ID")

train$Referred_Friend[train$Referred_Friend == "YES"] <- 1
train$Referred_Friend[train$Referred_Friend == "NO"] <- 0
train$Referred_Friend <- as.integer(train$Referred_Friend)

temp9 <- as.data.frame(train %>% group_by(Client_ID) %>% summarise(prop_refer = mean(Referred_Friend)))

client_info <- merge(client_info, temp9, by = "Client_ID")

train$Payment_Mode[train$Payment_Mode == "Cash"] <- 2
train$Payment_Mode[train$Payment_Mode == "Cheque"] <- 3
train$Payment_Mode[train$Payment_Mode == "Credit/Debit Card"] <- 4
train$Payment_Mode[train$Payment_Mode == "Other"] <- 1
train$Payment_Mode[is.na(train$Payment_Mode)] <- 4
train$Payment_Mode <- as.integer(train$Payment_Mode)

temp10 <- as.data.frame(train %>% group_by(Client_ID) %>% summarise(payment_mode = as.integer(names(sort(table(Payment_Mode), decreasing = TRUE))[1])))
client_info <- merge(client_info, temp10, by = "Client_ID")


#...
features <- client_info[, c("Client_ID", "diff_from_2007_dec", "Total_Amount", "Freq", "Gender", "prop_purchase_sale", "Total_Num_EMI", "prop_refer", "target", "payment_mode")]
features$Gender[features$Gender == "M" | features$Gender == "C"] <- 1
features$Gender[features$Gender == "F"] <- 2
features$Gender[is.na(features$Gender)] <- 1
features$Gender <- as.factor(features$Gender)

features$payment_mode <- as.factor(features$payment_mode)

# features$age <- as.numeric(features$age)

features[, -c(1, 5, 9, 10)] <- scale(features[, -c(1, 5, 9, 10)])

features$target2 <- scale(1/features$diff_from_2007_dec, center = FALSE) + scale(features$Total_Amount, center = FALSE) + scale(features$Freq, center = FALSE)



fit_glm <- glm(as.factor(target) ~ ., data = features[, -c(1)], family = quasibinomial("logit"), maxit = 200)

pred <- predict(fit_glm, features[, -1], type = "response")
pred[is.na(pred)] <- 0

result3 <- data.frame(Client_ID = client_info$Client_ID, Cross_Sell = pred)
write.csv(result3, "submission_10.0.csv", row.names = FALSE)
write.csv(features, "features.csv", row.names = FALSE)
write.csv(client_info, file = "client_info_2.0.csv", row.names = FALSE)


#... Outprocessing

temp <- as.data.frame(train[train$trans_year == 2003, c("Client_ID")])
temp2 <- table(temp)
temp3 <- as.integer(names(temp2[temp2 == 1]))

result3[result3$Client_ID %in% temp3, "Cross_Sell"] <- 0
write.csv(result3, "submission_6.0.csv", row.names = FALSE)

#... Experiment 4 -- xgboost
#################################
params <- list(booster = "gbtree", eta = 0.01, gamma = 0.1, max_depth = 10, min_child_weight = 3, subsample = 0.6, colsample_bytree = 0.8, nthread = 4)

train_sparse <- sparse.model.matrix(~., data = features[, -c(1, 6)])
train_xgb <- xgb.DMatrix(train_sparse, label = features$target)

xgb_model <- xgb.train(data = train_xgb, params = params, eval_metric = "auc", nrounds = 200, objective = "binary:logistic", verbose = 1)

pred <- predict(xgb_model, train_xgb)
result4 <- data.frame(Client_ID = features$Client_ID, Cross_Sell = pred)
write.csv(result4, file = "submission_8.0.csv", row.names = FALSE)


