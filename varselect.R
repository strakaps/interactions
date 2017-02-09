require(glinternet)
require(pROC)
require(hash)

source("auxiliaries.R")
load("synth-allcols.RData")

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
      key <- "0" #ASSUMING ONLY ONE CONTINUOUS VARIABLE HERE
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
        #ASSUMING ONLY ONE CONTINUOUS VARIABLE HERE:
        key <- toString(c(inter[iinter,1],0))
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

# does this: "8, 0" -> [1] "amitriptyline, age"
get_varnames <- function(index_string, name_string){
  # takes a string of single index or pair of indices and outputs variable name
  ixi <- as.integer(unlist(strsplit(index_string, ', '))) # index integers
  out_string = laply(ixi, .fun=function(x){
    if(x==0){
      return("age")
    } else{
      return(name_string[x])
    }
  })
  paste(out_string, collapse = ", ")
}

laply(var_names, .fun = function(s){
  get_varnames(s, dimnames(z)[[2]])
}) -> dimnames(count_matrix)[[2]]

# Plot heatmap

require(ggplot2)
require(data.table)
count.m = melt(count_matrix)
count.m$Var1 <- factor(count.m$Var1)

base_size <- 9
# the plot below won't be shown if you source this file. 
# You have to manually run these lines in RStudio...
(p <- ggplot(count.m, aes(Var1, Var2)) + 
    geom_tile(aes(fill = value), colour = "white") +
    geom_vline(xintercept = length(lambda) + 1 - i1SE, 
               linetype = 2) +
    geom_vline(xintercept = length(lambda) + 1 - iMax, 
               linetype = 2) +
    scale_fill_gradient(low = "white", high = "steelblue") + 
    theme_grey(base_size=base_size) + 
    labs(x="", y="") + 
    scale_x_discrete(expand = c(0,0)) + 
    scale_y_discrete(expand = c(0, 0)) + 
    theme( #axis.ticks = element_blank(), 
          axis.text.x=element_text(size=base_size*0.8, angle=90, hjust = 0)))


# pdf: 7.7 times 4.8 inches
