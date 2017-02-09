args = commandArgs(trailingOnly = TRUE) # name of the outputfile

require(glinternet)

source('auxiliaries.R')
load("./synth-allcols.RData")

# numbers of levels; 1 is coded as continuous
numLevels <- c(1,apply(X = z, MARGIN = 2, FUN = function(x){length(unique(x))}))

n <- dim(z)[1] # number of rows

maxLambda <- exp(-6.5)
minLambda <- exp(-9)
nLambda <- 50
lambda <- exp(seq(from = log(maxLambda), to = log(minLambda), 
                  length.out = nLambda))

q <- 0.1 # fraction of test data

# if the seed is not set, strangely, all runs result in the same sample:
t <- as.numeric(Sys.time())
seed <- 1e8 * (t - floor(t))
set.seed(seed)

itest <- sample(n, size = round(n*q), replace = FALSE)
itrain <- (1:n)[-itest]
fit <- glinternet(X = cbind(x,z)[itrain,], Y = y[itrain], 
           numLevels = numLevels, 
            lambda = lambda, family = "binomial", verbose = TRUE)

save(list = c("fit", "itrain", "itest"), file = paste("glinternet_models/", args[1], sep=""))
