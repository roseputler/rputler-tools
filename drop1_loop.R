# drop1_glm_loop
## Function to automate the drop1 steps for backward model selection. 
## Examines a model, and drops each least significant variable, until an alpha threshold for significance is met.
## Returns: final model, with option to print interim models to the screen.
## Options:
### outcome - string of variable name to be included in the model as an outcome. Default: none.
### allVariables - vector of character strings of all predictor variables to be included in the original full model. Default: none.
### data - dataframe containing the data to be modeled. Default: none.
### trace - logical value indicating whether all steps in the drop1 loop should be displayed or just the final model. Default: FALSE.
### family - string indicating family to be used in glm model. Default: "binomial".
### test - string indicating test statistic to be used in the drop1 function. Default: "Chisq".
### alpha - significance level required for each predictor, will not drop any predictors with p < alpha. Default: 0.05.
drop1_glm_loop <- function(outcome, allVariables, data, trace=F, family="binomial", test="Chisq", alpha=0.05){
  try(if(!is.character(outcome) | length(outcome)>1) stop("The option `outcome` must be a single string for the outcome of interest."))
  try(if(!is.character(allVariables)) stop("The option `allVariables` must be a string or a vector of strings for predictor variables."))
  try(if(is.null(data)) stop("The option `data` is required and has no default."))
  m1 <- glm(eval(parse(text=paste0(outcome,"~", paste0(paste0('`',allVariables,'`'),collapse="+")))),
              data=data,
              family=family)
  d1 <- drop1(m1,test=test)
  while(any(d1$`Pr(>Chi)`>=alpha,na.rm = T)){
    var <- d1 %>% mutate(var = row.names(d1)) %>% filter(!is.na(Df)) %>% arrange(`Pr(>Chi)`) %>% head(-1) %>% pull(var)
    m1 <- glm(eval(parse(text=paste0(outcome,"~",paste0(var,collapse="+")))),
              data=data,
              family=family)
    d1 <- drop1(m1,test=test)
    if(trace) print(d1)
  }
  if(!trace) print(d1)
  return(m1)
}
