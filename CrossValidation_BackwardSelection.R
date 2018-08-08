drop1_loop <- function(outcome, allVariables, data, trace=F,
                       family="binomial", test="Chisq", alpha=0.05){
  require(dplyr)
  try(if(!is.character(outcome) | length(outcome)>1) 
    stop("The option `outcome` must be a single string for the outcome of interest."))
  try(if(!is.character(allVariables)) 
    stop("The option `allVariables` must be a string or a vector of strings for predictor variables."))
  try(if(is.null(data)) 
    stop("The option `data` is required and has no default."))
  m1 <- glm(eval(parse(text=paste0(outcome,"~", paste0(paste0('`',allVariables,'`'),collapse="+")))),
              data=data,
              family=family)
  d1 <- drop1(m1,test=test)
  while(any(d1$`Pr(>Chi)`>=alpha,na.rm = T)){
    var <- d1 %>% 
      mutate(var = row.names(d1)) %>% 
      filter(!is.na(Df)) %>% 
      arrange(`Pr(>Chi)`) %>% 
      head(-1) %>% 
      pull(var)
    m1 <- glm(eval(parse(text=paste0(outcome,"~",paste0(var,collapse="+")))),
              data=data,
              family=family)
    d1 <- drop1(m1,test=test)
    if(trace) print(d1)
  }
  # if(!trace) print(d1)
  return(m1)
}


# Run backward selection based on either AIC or LRT (drop1).
# Input:
## data: Dataframe containing the data of interest.
## outcome: String of the name of the variable you want to use as the outcome.
## vars: A vector of strings of variables names you want to use as a starting point. If NULL, will default to all columns not the outcome.
## alpha.unadjusted: The threshold below which variables in unadjusted models must be in order to be included in the adjusted model. Default 1, i.e. all variables in `vars` included.
## nfold: The number of folds. Default 5.
## method: The method used, either "AIC" or "LRT".
# Output:
## List with 4 entries:
### data: The dataset used, after subsetting on complete cases.
### folds: The row numbers from 'data' (after filtering on complete cases) used for the testing dataset in each fold, as a list.
### models: The resulting final model objects for each fold based on the training dataset after backward selection for each fold, as a list.
### train.roc: The ROC object from the final model fit on the training dataset for each fold, as a list.
### test.roc: The ROC object from the final model fit on the testing dataset for each fold, as a list.
cv.backwardSelect <- function(data, outcome, vars = NULL, alpha.unadjusted = 1, nfold = 5, method){
  if(! method %in% c("AIC","LRT") || length(method) != 1) stop("Please choose method of 'AIC' or 'LRT'.")
  require(caret); require(pROC)
  # If variables not specified, include all variables in the data that are not the outcome.
  if(is.null(vars)) vars <- colnames(data[,colnames(data) != outcome])
  # Only include complete cases for all potential variables. This may not be the right time to do this, but I think is OK.
  data <- data.frame(data[complete.cases(data[,c(outcome,vars)]),c(outcome,vars)], check.names = F)
  # Create folds.
  folds <- createFolds(y = 1:nrow(data), k = nfold,list = TRUE)
  # Start modeling.
  models <- list()
  train.roc <- list()
  test.roc <- list()
  for(i in 1:nfold){
    # Make test and train split.
    if(nfold > 1) {
      train <- data[-folds[[i]],]
      test <- data[folds[[i]],]
    } else{
      train <- data
    }
    # If variables need to subset by some sort of alpha threshold in unadjusted analyses (i.e. 0.25).
    if(alpha.unadjusted != 1){
      vars = vars[sapply(X = vars,
                FUN = function(x) summary(glm(eval(parse(text=paste0(outcome,' ~ `',x,'`'))),
                                               data=train,
                                               family="binomial"))$coefficients[2,4]) < alpha.unadjusted]
    }
    # Run model with all variables of interest.
    if(method == "AIC"){
      model <- glm(eval(parse(text=paste0(outcome,' ~ ',paste0('`',vars,'`',collapse = " + ")))),
                 data=train,
                 family="binomial")
      final <- step(model,direction = "backward", data = train,trace = 0)
    } else{
      final <- drop1_loop(outcome = outcome, allVariables = vars, data = train, trace = F)
    }
    models[[names(folds)[i]]] <- final
    # Create a Receiver Operator Curve on the original training data.
    train.roc[[names(folds)[i]]] <- roc(response = as.numeric(train[,outcome]),
                                       predictor = predict(glm(final$formula, data=train,
                                                               family="binomial"),type = "response"))
    # Validate the model by creating a Receiver Operator Curve on the holdout test data.
    if(nfold > 1) {
      test.roc[[names(folds)[i]]] <- roc(response = as.numeric(test[,outcome]),
                                       predictor = predict(glm(final$formula, data=test,
                                                               family="binomial"),type = "response"))
    }
  }
  return(list(data = data, folds = folds,models = models,train.roc = train.roc,test.roc = test.roc))
}
