require(glinternet)
require(pROC)
require(hash)

source("auxiliaries.R")
load("~/Documents/Research/Projects/16Depression/data7vars.RData")
filenames <- dir("./glinternet_models/")
load(paste("./glinternet_models/",filenames[1], sep = ""))
lambda <- fit$lambda
nLambda <- length(lambda)

### do one sweep over all models saved in glinternet_models:

# initialize

AUC_matrix <- matrix(nrow = length(filenames), ncol = nLambda)
lambda_matrix <- matrix(nrow = length(filenames), ncol = nLambda)

# make a list of hashtables, one for each lambda. Each hashtable
# counts the occurrences of the variables or interactions. 
h = list()
for(lam in 1:nLambda) {h[[lam]] = hash()}


# iterate over all model fits
for(model in 1:length(filenames)){
  print(paste("Processing", filenames[model]))
  # fill AUC matrix
  load(paste("./glinternet_models/",filenames[model], sep = ""))
  yhat <- predict(object = fit, X = cbind(x,z)[itest, ], type = "response")
  AUC_matrix[model, ] <- sapply(1:nLambda, function(k){
    curve <- roc(y[itest], yhat[ ,k], smooth = FALSE)
    return(curve$auc)
  })
  lambda_matrix[model, ] <- fit$lambda
  # fill hash table of variable occurrences
  coefs <- coef(fit)
  # iterate over all lambda values
  for(lam in 1:nLambda){
    print(paste("lambda:", lam))
    # main effects, categorical:
    mains <- coefs[[lam]]$mainEffects$cat
    for(imain in 1:length(mains)){
      if(length(mains) == 0){break}
      key <- toString(mains[imain])
      # cannot set value to default = 0 if key is not defined; 
      # hence the awkward code below
      oldval <- h[[lam]][[key]]
      if (is.null(oldval)) {
        h[[lam]][[key]] <- 1
      } else{
        h[[lam]][[key]] <- oldval + 1
      } 
    }
    # main effect, continuous: 
    mains <- coefs[[lam]]$mainEffects$cont
    for(imain in 1:length(mains)){
      if(length(mains) == 0){break}
      key <- toString(mains[imain])
      # cannot set value to default = 0 if key is not defined; 
      # hence the awkward code below
      oldval <- h[[lam]][[key]]
      if (is.null(oldval)) {
        h[[lam]][[key]] <- 1
      } else{
        h[[lam]][[key]] <- oldval + 1
      } 
    }
    # "catcat"-interactions:
    inter <- coefs[[lam]]$interactions$catcat # pairs of indices
    if (!is.null(inter)){
      for(iinter in 1:dim(inter)[1]){
        if(length(inter) == 0){break}
        key <- toString(inter[iinter,])
        oldval <- h[[lam]][[key]]
        if (is.null(oldval)) {
          h[[lam]][[key]] <- 1
        } else{
          h[[lam]][[key]] <- oldval + 1
        }
      }
    }
    # "catcont"-interactions
    inter <- coefs[[lam]]$interactions$catcont # pairs of indices
    if (!is.null(inter)){
      for(iinter in 1:dim(inter)[1]){
        if(length(inter) == 0){break}
        key <- toString(inter[iinter,])
        oldval <- h[[lam]][[key]]
        if (is.null(oldval)) {
          h[[lam]][[key]] <- 1
        } else{
          h[[lam]][[key]] <- oldval + 1
        }
      }
    }
  }
}

sd_CV <- apply(AUC_matrix, 2, sd)
m_CV <- apply(AUC_matrix, 2, mean)

# TODO: assert lambda_matrix has constant columns

# plot CV error: 
errbar(log(lambda), m_CV, m_CV + sd_CV, m_CV - sd_CV, xlab = "log(lambda)",
       ylab = "AUC", main = "CV output")
iMax <- which(m_CV == max(m_CV))
lambdaOpt <- lambda[iMax]
abline(v = log(lambdaOpt), lty = 2)
i1SE <- which.max(m_CV > m_CV[iMax] -sd_CV[iMax])
lambda1SE <- lambda[i1SE]
abline(v = log(lambda1SE), lty =2)


# consider up to m variables / interactions:
m <- 50
# first, find the order of variables, in order as they appear with decreasing
# lambda (increasing number of variables):
var_names <- vector() # growing in the for loop below
# increase lambdas, add variables to var_names:
for(i in 1:nLambda){
  if(length(as.list(h[[i]])) == 0) next
  candidate_names <- names(sort(unlist(as.list(h[[i]])), decreasing = TRUE))
  new_var_names <- setdiff(candidate_names, var_names)
  var_names <- append(var_names, new_var_names)
  if(length(var_names) > m) break
}
var_names <- var_names[1:m]

count_matrix <- matrix(data = NA, nrow = nLambda, ncol = m,
                       dimnames = list(round(lambda * 10^6), var_names))

# fill the count_matrix
for(i in 1:nLambda){
  if(length(as.list(h[[i]])) == 0){
    count_matrix[i, ] <-  rep(0, m)
    next
  }
  counts = unlist(as.list(h[[i]]))
  row <- counts[var_names] / length(filenames)
  row[is.na(row)] <- 0
  row -> count_matrix[i, ]
}

heatmap(x = count_matrix, Rowv = NA, Colv = NA, 
        add.expr = abline(h=c(i1SE,iMax), lty = 2))


