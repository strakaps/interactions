require(plyr)
require(glinternet)
require(pROC)
require(parallel)
require(Hmisc)

# prepares dataset
get_data <- function(){
  # setwd("H:/Documents/Analysis data")
  # real <- read.csv("original.csv")
  real <- read.csv("Data/synthetic.csv")
  # Real data has 68 variables (as opposed to 69 in synthetic). Identifier is missing
  # code Female as 1
  as.integer(real$Participant.gender == "Female") -> real$Participant.gender
  # code gold standard yes = 1
  as.integer(real$gold_standard == "Yes") -> real$gold_standard
  # All variables are now integers. Remove the constant variables
  which(lapply(real, var)== 0) -> idx
  real[,-idx] -> real
  # Now has 66 variables, 2 removed were comed_fluphenazine and comed_tuberculosis because all values were 0
  # Check whether more variables were removed from synthetic data
  # dd <- read.csv("synthetic.csv")
  # which(lapply(dd, var)== 0) -> idx
  # dd[,-idx] -> dd
  # Yes, 66 variables here, while it had 69 to begin with (participant ID was the extra). ECT was removed from dd.
  # Discard class variable
  real[,names(real) !="Class"] -> real
  
  # Fit a Group Lasso Interaction Network to the data
  y <- real$gold_standard
  # continuous Ivs:
  x <- real[,names(real) == "age"] 
  z <- real[,names(real) != "age" & names(real) != "gold_standard"]
  z <- as.matrix(z)
  return(list(x,y,z))
}


# creates indices for k folds of a dataset of size n
make_folds <- function(n,k){
  folds <- alply(.data = matrix(sample(1:(n-n%%K)), ncol = K), .margins = 2,
                 .fun = function(x){return(x)})
  for(k in 1:(n%%K)){
    folds[[k]] <- c(folds[[k]], n+1-k)
  }
  return(folds)
}

# gets the training indices if test fold has index k
get_train_set <- function(k, folds){
  train <- vector(mode = "integer")
  j=1
  while(j <= K){
    if(j != k){
      train <- c(train, folds[[j]])
    }
    j <- j + 1
  }
  return(train)
}


# Calculates AUC is the kth fold is used as a test set
# Outputs a vector of length nLambda
calc_auc <- function(k){
  test <- folds[[k]]
  train <- get_train_set(k, folds)
  fit <- glinternet(X = cbind(x[train],z[train,]), Y = y[train],
                    numLevels = numLevels, lambda = lambda,
                    family = "binomial",
                    numCores = 1, verbose = TRUE, screenLimit = 0.2)
  yhat <-predict(object=fit, X = cbind(x[test], z[test,]), type = "response")
  
  auc_row <- sapply(1:nLambda, function(k){
    curve <- roc(y[test], yhat[,k], smooth = FALSE)
    return(curve$auc)
  })
  return(auc_row)
}
