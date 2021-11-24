
#libraries
{
  library(DataExplorer)
  
  library(dplyr)
  library(ggplot2)
  library(corrgram)
  library(corrplot)
  library(factoextra)
  
  library(randomForest)
  library(rpart)
  library(rpart.plot)
  library(naivebayes)
  library(e1071)
  
  
  library(pROC)
  
  library(caTools)
  library(fastDummies)
  
  library(arules)
  library(cluster)
}

# set working directory
{
  setwd("D:/projects/OZ_analysis")
}

# loading data 
{
  kickstarterData2016 <- read.csv("Data/ks-projects-201612.csv")
  kickstarterData2018 <- read.csv("Data/ks-projects-201801.csv")
}


# start analysis
{
  # kickstarterData2016
  {
    plot_str(kickstarterData2016, fontSize=40)
    str(kickstarterData2016)
  
    plot_missing(kickstarterData2016)
    profile_missing(kickstarterData2016)
    k <- plot_bar(kickstarterData2016)
    k <- k[["page_1"]][["data"]]
  }
  
  
  # kickstarterData2018
  {
    plot_str(kickstarterData2018, fontSize=40)
    str(kickstarterData2018)
    
    plot_missing(kickstarterData2018)
    profile_missing(kickstarterData2018)
    k <- plot_bar(kickstarterData2018)
    k <- k[["page_1"]][["data"]]
  }
    
}



# selection
{
 kickstarterData2016_1 <- kickstarterData2016 %>% select(-X, -X.1, -X.2, -X.3, -name, -deadline, -launched, -category, -usd.pledged)
 kickstarterData2018_1 <- kickstarterData2018 %>% select(-name, -deadline, -launched, -category, -usd.pledged, -usd_pledged_real, -usd_goal_real)
 

}

#print unique values
{
 unique(kickstarterData2016$state)
 unique(kickstarterData2016$currency)
 
 unique(kickstarterData2018$state)
 unique(kickstarterData2018$currency)
}

# select needed data
{
  #2016
  {
    kickstarterData2016_1 <- kickstarterData2016_1 %>% filter(state == list("successful", "failed"))  
    unique(kickstarterData2016_1$state) 
    unique(kickstarterData2016_1$currency)
  }
  #2018
  {
    kickstarterData2018_1 <- kickstarterData2018_1 %>% filter(state == list("successful", "failed")) 
    unique(kickstarterData2018_1$state) 
    unique(kickstarterData2018_1$currency)
  }
}

# type conversion
{
  kickstarterData2016_1$ID <- as.numeric(kickstarterData2016_1$ID)  
  kickstarterData2016_1$goal <- as.numeric(kickstarterData2016_1$goal)
  kickstarterData2016_1$pledged <- as.numeric(kickstarterData2016_1$pledged)
  kickstarterData2016_1$backers <- as.numeric(kickstarterData2016_1$backers)
  
  str(kickstarterData2016_1)
  
  kickstarterData2018_1$ID <- as.numeric(kickstarterData2018_1$ID)  
  kickstarterData2018_1$goal <- as.numeric(kickstarterData2018_1$goal)
  kickstarterData2018_1$pledged <- as.numeric(kickstarterData2018_1$pledged)
  kickstarterData2018_1$backers <- as.numeric(kickstarterData2018_1$backers)
  
  str(kickstarterData2018_1)
}

# currency conversion 
{
  # conversion for 2016
  {
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "EUR"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "EUR"] * 1.085, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "EUR"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "EUR"] * 1.085, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "GBP"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "GBP"] * 1.474, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "GBP"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "GBP"] * 1.474, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "CAD"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "CAD"] * 0.721, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "CAD"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "CAD"] * 0.721, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "AUD"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "AUD"] * 0.73, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "AUD"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "AUD"] * 0.73, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "NZD"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "NZD"] * 0.684, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "NZD"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "NZD"] * 0.684, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "CHF"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "CHF"] * 0.998, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "CHF"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "CHF"] * 0.998, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "DKK"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "DKK"] * 0.145, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "DKK"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "DKK"] * 0.145, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "SEK"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "SEK"] * 0.188, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "SEK"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "SEK"] * 0.188, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "NOK"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "NOK"] * 0.113, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "NOK"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "NOK"] * 0.113, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "HKD"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "HKD"] * 0.129, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "HKD"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "HKD"] * 0.129, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "SGD"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "SGD"] * 0.707, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "SGD"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "SGD"] * 0.707, digits=2)
  
    kickstarterData2016_1$goal[kickstarterData2016_1$currency == "MXN"] <- round(kickstarterData2016_1$goal[kickstarterData2016_1$currency == "MXN"] * 0.058, digits=2)
    kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "MXN"] <- round(kickstarterData2016_1$pledged[kickstarterData2016_1$currency == "MXN"] * 0.058, digits=2)
 
  }
  
  # conversion for 2018
 { 
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "EUR"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "EUR"] * 1.085, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "EUR"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "EUR"] * 1.085, digits=2)
 
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "GBP"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "GBP"] * 1.474, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "GBP"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "GBP"] * 1.474, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "CAD"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "CAD"] * 0.721, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "CAD"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "CAD"] * 0.721, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "AUD"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "AUD"] * 0.73, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "AUD"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "AUD"] * 0.73, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "NZD"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "NZD"] * 0.684, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "NZD"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "NZD"] * 0.684, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "CHF"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "CHF"] * 0.998, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "CHF"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "CHF"] * 0.998, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "DKK"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "DKK"] * 0.145, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "DKK"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "DKK"] * 0.145, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "SEK"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "SEK"] * 0.188, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "SEK"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "SEK"] * 0.188, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "NOK"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "NOK"] * 0.113, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "NOK"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "NOK"] * 0.113, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "HKD"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "HKD"] * 0.129, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "HKD"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "HKD"] * 0.129, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "SGD"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "SGD"] * 0.707, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "SGD"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "SGD"] * 0.707, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "MXN"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "MXN"] * 0.058, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "MXN"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "MXN"] * 0.058, digits=2)
  
    kickstarterData2018_1$goal[kickstarterData2018_1$currency == "JPY"] <- round(kickstarterData2018_1$goal[kickstarterData2018_1$currency == "JPY"] * 0.009, digits=2)
    kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "JPY"] <- round(kickstarterData2018_1$pledged[kickstarterData2018_1$currency == "JPY"] * 0.009, digits=2)
  
  }
}

# selection №2
{
  kickstarterData2016_1 <- kickstarterData2016_1 %>% select(-currency, -country)
  kickstarterData2018_1 <- kickstarterData2018_1 %>% select(-currency, -country)
}

# dummify data for analysis
{
  kickstarterData2016_1_dum <- dummy_cols(kickstarterData2016_1, "state")
  kickstarterData2018_1_dum <- dummy_cols(kickstarterData2018_1, "state")
}

# analysis №2
{
  # kickstarter2016
  {
    cor(kickstarterData2016_1_dum[6:8])
    corrgram(kickstarterData2016_1_dum)
    summary(kickstarterData2016_1_dum)
    
    plot_histogram(kickstarterData2016_1[2:6], geom_histogram_args = list(bins = 100))
    k <- plot_bar(kickstarterData2016_1)
    k <- k[["page_1"]][["data"]]
  
    arr <- kickstarterData2016_1[, c("pledged", "backers")]
    plot_scatterplot(arr, by="backers")
    
    arr1 <- kickstarterData2016_1[, c("main_category", "backers")]
    plot_scatterplot(arr1, by="backers")
    
    # chi-square independence test of attributes
    {    
      cont_table_2016 <- table(kickstarterData2016_1$state, kickstarterData2016_1$main_category)
      chisq_2016 <- chisq.test(cont_table_2016, simulate.p.value=FALSE)
    
      chisq_2016$observed
      chisq_2016$expected
      chisq_2016
    
      contrib_2016 <- 100*chisq_2016$residuals^2/chisq_2016$statistic
  
      corrplot(chisq_2016$residuals, is.cor = FALSE)
      corrplot(round(contrib_2016, 3), is.cor = FALSE)
    }
  }
  
  
  # kickstarter2018
  {
    cor(kickstarterData2018_1_dum[6:8])
    corrgram(kickstarterData2016_1_dum)
    summary(kickstarterData2018_1_dum)
    
    plot_histogram(kickstarterData2018_1[2:6], geom_histogram_args = list(bins = 100))
    k <- plot_bar(kickstarterData2018_1)
    k <- k[["page_1"]][["data"]]
    
    arr <- kickstarterData2018_1[, c("pledged", "backers")]
    plot_scatterplot(arr, by="backers")
  
    arr1 <- kickstarterData2018_1[, c("main_category", "backers")]
    plot_scatterplot(arr1, by="backers")
    
    
    # chi-square independence test of attributes
    {
      cont_table_2018 <- table(kickstarterData2018_1$state, kickstarterData2018_1$main_category)
      chisq_2018 <- chisq.test(cont_table_2018, simulate.p.value=FALSE)
    
      chisq_2018$observed
      chisq_2018$expected
      chisq_2018
    
      contrib_2018 <- 100*chisq_2018$residuals^2/chisq_2018$statistic
    
      corrplot(chisq_2018$residuals, is.cor = FALSE)
      corrplot(round(contrib_2018, 3), is.cor = FALSE)
    }

  }
}

# join tables and prepare data
{
  kickstarterDataIn <- kickstarterData2016_1 %>% inner_join(kickstarterData2018_1, by="ID")
  
  # columns comparison
  { 
    unique(kickstarterDataIn$state.x == kickstarterDataIn$state.y)
    unique(kickstarterDataIn$goal.x == kickstarterDataIn$goal.y)
    unique(kickstarterDataIn$pledged.x == kickstarterDataIn$pledged.y)
  }
  
  # join tables for final dataset
  kickstarterData <- kickstarterData2016_1 %>% full_join(kickstarterData2018_1, by="ID")
  
  # join columns 
  { 
    kickstarterData$goal.x <- ifelse(is.na(kickstarterData$goal.x), kickstarterData$goal.y, kickstarterData$goal.x)
    kickstarterData$pledged.x <- ifelse(is.na(kickstarterData$pledged.x), kickstarterData$pledged.y, kickstarterData$pledged.x)
    kickstarterData$main_category.x <- ifelse(is.na(kickstarterData$main_category.x), kickstarterData$main_category.y, kickstarterData$main_category.x)
    kickstarterData$state.x <- ifelse(is.na(kickstarterData$state.x), kickstarterData$state.y, kickstarterData$state.x)
    kickstarterData$backers.x <- ifelse(is.na(kickstarterData$backers.x), kickstarterData$backers.y, kickstarterData$backers.x)
  }
  
}

# selection №3
{
  kickstarterData <- kickstarterData %>% select(-goal.y, -pledged.y,-main_category.y, -state.y, -backers.y, -pledged.x, -main_category.x)
  kickstarterData <- kickstarterData %>% rename(goal = goal.x, backers = backers.x, state = state.x)
}


# dummify state
{
  kickstarterData$state <- as.factor(kickstarterData$state)
  kickstarterData <- kickstarterData[c("ID", "goal", "backers", "state")]
  
}
  
#train-test split
{
  sample <- sample.split(kickstarterData$state, SplitRatio=0.75)
 
  
  train = subset(kickstarterData, sample == TRUE)
  test = subset(kickstarterData, sample == FALSE)
  
  dim(train)
  dim(test)
  
}

# train models
{
  # decision tree
  tree <- rpart(state ~., data=train)


  # random forest
  rf <- randomForest(state ~., data=train)
  
  
  # naive bayes
  nb <- naive_bayes(state ~., data=train)
  
  
  # logistic regression
  log_reg <- glm(state ~., data=train, family="binomial")
  
  
  
}

# prediction by models
{
  # decision tree prediction
  pred_tree <- predict(tree, newdata = test, type="class")
  
  
  # random forest prediction
  pred_rf <- predict(rf, newdata = test[-4])
  
  
  # naive bayes prediction
  pred_nb <- predict(nb, newdata = test[-4])
  
  
  # logistic regression prediction
  pred_log_reg <- predict(log_reg, newdata = test, type="response")
  pred_log_reg<-ifelse(pred_log_reg > 0.5, "failed", "succesful")
  
}


#evaluation
{
  # decision tree evaluation
  {
    cm_tree <- table(test[,4], pred_tree)
    
    diag_tree = diag(cm_tree)
    num_p = apply(cm_tree, 2, sum)
    num_in = apply(cm_tree, 1, sum)
    
    print(diag_tree)
    print(num_p)
    {
      precision_tree = diag_tree / num_p
      print(as.numeric(precision_tree[1]))
      
      recall_tree = diag_tree / num_in
      print(as.numeric(recall_tree[1]))
      
      specificity_tree = rev(diag_tree) / rev(num_p)
      print(as.numeric(specificity_tree[1]))
      
      roc_tree <- roc(test[,4]~factor(pred_tree, ordered=TRUE), plot=TRUE, print.auc=TRUE, col="blue", 
                    lwd=4, legacy.axes=TRUE, main="ROC curve of Decision Tree Classificator")
      
      rpart.plot(tree)
      
    }
    
  }
  
  
  # random forest evaluation
  {
    cm_rf <- table(test[,4], pred_rf)
    
    diag_rf = diag(cm_rf)
    num_p = apply(cm_rf, 2, sum)
    num_in = apply(cm_rf, 1, sum)
  
    print(diag_rf)
    print(num_p)
  
    {
      precision_rf = diag_rf / num_p
      print(as.numeric(precision_rf[1]))
  
      recall_rf = diag_rf / num_in
      print(as.numeric(recall_rf[1]))
      
      specificity_rf = rev(diag_rf) / rev(num_p)
      print(as.numeric(specificity_rf[1]))
  
      roc_rf <- roc(test[,4]~factor(pred_rf, ordered=TRUE), plot=TRUE, print.auc=TRUE, col="green", 
                    lwd=4, legacy.axes=TRUE, main="ROC curve of Random Forest Classificator")
  
    }
  }
  
  # naive bayes evaluation
  {
    cm_nb <- table(test[,4], pred_nb)
    
    diag_nb = diag(cm_nb)
    num_p = apply(cm_nb, 2, sum)
    num_in = apply(cm_nb, 1, sum)
    
    print(diag_nb)
    print(num_p)
    
    {
      precision_nb = diag_nb / num_p
      print(as.numeric(precision_nb[1]))
      
      recall_nb = diag_nb / num_in
      print(as.numeric(recall_nb[1]))
      
      specificity_nb = rev(diag_nb) / rev(num_p)
      print(as.numeric(specificity_nb[1]))
      
      roc_nb <- roc(test[,4]~factor(pred_nb, ordered=TRUE), plot=TRUE, print.auc=TRUE, col="red", 
                    lwd=4, legacy.axes=TRUE, main="ROC curve of Naive Bayes Classification")
      
    }
  }
  
  # logistic regression evaluation
  {
    cm_log_reg <- table(test[,4], pred_log_reg)
    
    diag_log_reg = diag(cm_log_reg)
    num_p = apply(cm_log_reg, 2, sum)
    num_in = apply(cm_log_reg, 1, sum)
    
    print(diag_log_reg)
    print(num_p)
    
    {
      precision_log_reg = diag_log_reg / num_p
      print(as.numeric(precision_log_reg[1]))
      
      recall_log_reg = diag_log_reg / num_in
      print(as.numeric(recall_log_reg[1]))
      
      specificity_log_reg = rev(diag_log_reg) / rev(num_p)
      print(as.numeric(specificity_log_reg[1]))
      
      roc_log_reg <- roc(test[,4]~factor(pred_log_reg, ordered=TRUE), plot=TRUE, print.auc=TRUE, col="orange", 
                    lwd=4, legacy.axes=TRUE, main="ROC curve of Logistic Regression")
      
      
    }
  }
  
}


# description task

{
  
  
  # selection for description task
  {
    kickstarterData2016_des <- kickstarterData2016 %>% select(-name, -launched, -deadline, -X.1, -X.2, -X.3, -X, -usd.pledged, -category)
    kickstarterData2018_des <- kickstarterData2018 %>% select(-name, -launched, -deadline, -usd.pledged, -usd_pledged_real, -usd_goal_real, -category)
    
  }
  
  kickstarterData2016_des$goal <- as.numeric(kickstarterData2016_des$goal)
  kickstarterData2016_des$pledged <- as.numeric(kickstarterData2016_des$pledged)
  
  kickstarterData2018_des$goal <- as.numeric(kickstarterData2018_des$goal)
  kickstarterData2018_des$pledged <- as.numeric(kickstarterData2018_des$pledged)
  
  {
    kickstarterData2016_des <- kickstarterData2016_des %>% filter(state == list("successful", "failed", "live", "canceled")) 
    kickstarterData2018_des <- kickstarterData2018_des %>% filter(state == list("successful", "failed", "live", "canceled")) 
  }
  
  
  # currency conversion
  {
    
    {
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "EUR"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "EUR"] * 1.085, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "EUR"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "EUR"] * 1.085, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "GBP"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "GBP"] * 1.474, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "GBP"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "GBP"] * 1.474, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "CAD"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "CAD"] * 0.721, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "CAD"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "CAD"] * 0.721, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "AUD"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "AUD"] * 0.73, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "AUD"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "AUD"] * 0.73, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "NZD"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "NZD"] * 0.684, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "NZD"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "NZD"] * 0.684, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "CHF"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "CHF"] * 0.998, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "CHF"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "CHF"] * 0.998, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "DKK"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "DKK"] * 0.145, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "DKK"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "DKK"] * 0.145, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "SEK"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "SEK"] * 0.188, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "SEK"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "SEK"] * 0.188, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "NOK"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "NOK"] * 0.113, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "NOK"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "NOK"] * 0.113, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "HKD"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "HKD"] * 0.129, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "HKD"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "HKD"] * 0.129, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "SGD"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "SGD"] * 0.707, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "SGD"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "SGD"] * 0.707, digits=2)
      
      kickstarterData2016_des$goal[kickstarterData2016_des$currency == "MXN"] <- round(kickstarterData2016_des$goal[kickstarterData2016_des$currency == "MXN"] * 0.058, digits=2)
      kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "MXN"] <- round(kickstarterData2016_des$pledged[kickstarterData2016_des$currency == "MXN"] * 0.058, digits=2)
      
      
    }
    
    
    {
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "EUR"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "EUR"] * 1.085, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "EUR"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "EUR"] * 1.085, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "GBP"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "GBP"] * 1.474, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "GBP"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "GBP"] * 1.474, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "CAD"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "CAD"] * 0.721, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "CAD"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "CAD"] * 0.721, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "AUD"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "AUD"] * 0.73, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "AUD"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "AUD"] * 0.73, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "NZD"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "NZD"] * 0.684, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "NZD"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "NZD"] * 0.684, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "CHF"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "CHF"] * 0.998, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "CHF"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "CHF"] * 0.998, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "DKK"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "DKK"] * 0.145, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "DKK"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "DKK"] * 0.145, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "SEK"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "SEK"] * 0.188, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "SEK"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "SEK"] * 0.188, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "NOK"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "NOK"] * 0.113, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "NOK"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "NOK"] * 0.113, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "HKD"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "HKD"] * 0.129, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "HKD"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "HKD"] * 0.129, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "SGD"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "SGD"] * 0.707, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "SGD"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "SGD"] * 0.707, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "MXN"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "MXN"] * 0.058, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "MXN"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "MXN"] * 0.058, digits=2)
      
      kickstarterData2018_des$goal[kickstarterData2018_des$currency == "JPY"] <- round(kickstarterData2018_des$goal[kickstarterData2018_des$currency == "JPY"] * 0.009, digits=2)
      kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "JPY"] <- round(kickstarterData2018_des$pledged[kickstarterData2018_des$currency == "JPY"] * 0.009, digits=2)
    }
    
  }
  
  kickstarterData2016_des <- kickstarterData2016_des %>% select(-ID, -currency)
  kickstarterData2018_des <- kickstarterData2018_des %>% select(-ID, -currency)
  
  
  
  # factor for rules
  {
    kickstarterData2016_des$main_category <- as.factor(kickstarterData2016_des$main_category)
    kickstarterData2016_des$country <- as.factor(kickstarterData2016_des$country)
    kickstarterData2016_des$goal <- as.numeric(kickstarterData2016_des$goal)
    kickstarterData2016_des$pledged <- as.numeric(kickstarterData2016_des$pledged)
    kickstarterData2016_des$backers <- as.numeric(kickstarterData2016_des$backers)
    kickstarterData2016_des$state <- as.factor(kickstarterData2016_des$state)
    
    kickstarterData2018_des$main_category <- as.factor(kickstarterData2018_des$main_category)
    kickstarterData2018_des$country <- as.factor(kickstarterData2018_des$country)
    kickstarterData2018_des$goal <- as.numeric(kickstarterData2018_des$goal)
    kickstarterData2018_des$pledged <- as.numeric(kickstarterData2018_des$pledged)
    kickstarterData2018_des$backers <- as.numeric(kickstarterData2018_des$backers)
    kickstarterData2018_des$state <- as.factor(kickstarterData2018_des$state)
  }
  
  # percentage pledged and discretization backers
  { 
    kickstarterData2016_des$pledged_p <- round((kickstarterData2016_des$pledged / kickstarterData2016_des$goal) * 100, digits=0)
    kickstarterData2018_des$pledged_p <- round((kickstarterData2018_des$pledged / kickstarterData2018_des$goal) * 100, digits=0)
    max(kickstarterData2018_des$pledged_p)
  
    kickstarterData2016_des$backers_d <- kickstarterData2016_des$backers %>% discretize(method = "interval", breaks=15)
    kickstarterData2018_des$backers_d <- kickstarterData2018_des$backers %>% discretize(method = "interval", breaks=15)
  }
  
  
  # association rules
  {
    
    kickstarterData_asrules.all <- apriori(kickstarterData2016_des[c(-2, -3, -5)], parameter=list(minlen=2, conf=0.7, supp=0.05), appearance = list(rhs=c("state=successful", "state=failed", "state=live", "state=canceled")))
    kickstarterData_asrules.sorted <- sort(kickstarterData_asrules.all, by="lift")
    inspect(kickstarterData_asrules.sorted)
    
    
    kickstarterData_asrules_2018.all <- apriori(kickstarterData2018_des[c(-2, -3, -5)], parameter=list(minlen=2, conf=0.7, supp=0.05), appearance = list(rhs=c("state=successful", "state=failed", "state=live", "state=canceled")))
    kickstarterData_asrules_2018.sorted <- sort(kickstarterData_asrules_2018.all, by="lift")
    inspect(kickstarterData_asrules_2018.sorted)
    
  }
  
  # clustering
  {
  
    # clustering 2016  
    { 
      kickstarterData2016_des[c(-2, -7, -8, -1, -4, -6)]
      kmeans.kickstarterData2016 <- kmeans(kickstarterData2016_des[c(-2, -7, -8, -1, -4, -6)], 4)
      kickstarterData2016_des$cluster <- as.character(kmeans.kickstarterData2016$cluster)
    
      table(kickstarterData2016_des$state, kmeans.kickstarterData2016$cluster)

      ggplot(kickstarterData2016_des[c(-2, -7, -8, -1, -4, -6)], aes(x=backers, y= pledged, color=cluster)) + geom_point()
    }
    
    # clustering 2018
    {
      kickstarterData2018_des[c(-2, -7, -8, -1, -4, -6)]
      kmeans.kickstarterData2018 <- kmeans(kickstarterData2018_des[c(-2, -7, -8, -1, -4, -6)], 4)
      kickstarterData2018_des$cluster <- as.character(kmeans.kickstarterData2018$cluster)
    
      table(kickstarterData2018_des$state, kmeans.kickstarterData2018$cluster)
    
      ggplot(kickstarterData2018_des[c(-2, -7, -8, -1, -4, -6)], aes(x=backers, y= pledged, color=cluster)) + geom_point()
    }
  }
}







