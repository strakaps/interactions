require(plyr)

dd <- read.csv("./Data/synthetic.csv")
# get rid of identifier "X"
dd[,-1] -> dd
# code Female as 1
as.integer(dd$Participant.gender == "Female") -> dd$Participant.gender
# code gold standard yes as 1
as.integer(dd$gold_standard == "Yes") -> dd$gold_standard
# now all variables are integers. Remove the constant variables:
which(lapply(dd, var) == 0) -> idx
dd[,-idx] -> dd
# discard class variable
dd[,names(dd) != "Class"] -> dd

Cindices <- vector("list", 7)

Cindices[[1]] <- c(grep('^Fluvoxamine', names(dd), ignore.case = TRUE),
                   grep('^Fluoxetine', names(dd), ignore.case = TRUE),
                   grep('^Sertraline', names(dd), ignore.case = TRUE),
                   grep('^Escitalopram', names(dd), ignore.case = TRUE),
                   grep('^Citalopram', names(dd), ignore.case = TRUE),
                   grep('^Paroxetine', names(dd), ignore.case = TRUE))

Cindices[[2]] <- c(grep('^Amitriptyline', names(dd), ignore.case = TRUE),
                   grep('^Clomipramine', names(dd), ignore.case = TRUE),
                   grep('^Dothiepin', names(dd), ignore.case = TRUE),
                   grep('^Doxepin', names(dd), ignore.case = TRUE),
                   grep('^Imipramine', names(dd), ignore.case = TRUE),
                   grep('^Nortriptyline', names(dd), ignore.case = TRUE))

Cindices[[3]] <- grep('^Tranylcypromine$', names(dd), ignore.case = TRUE)

Cindices[[4]] <- grep('^Moclobemide$', names(dd), ignore.case = TRUE)

Cindices[[5]] <- c(grep('^Mianserin', names(dd), ignore.case = TRUE),
                   grep('^Mirtazapine', names(dd), ignore.case = TRUE))

Cindices[[6]] <- c(grep('^Reboxetine', names(dd), ignore.case = TRUE),
                   grep('^Venlafaxine', names(dd), ignore.case = TRUE))

Cindices[[7]] <- c(grep('^Lithium', names(dd), ignore.case = TRUE),
                   grep('^Desvenlafaxine', names(dd), ignore.case = TRUE),
                   grep('^Duloxetine', names(dd), ignore.case = TRUE))

# adds Class variables to the dataframe
adply(.data = dd, .margins = 1, .fun = function(x){
  return(
    laply(.data = 1:7, .fun = function(k){
      as.integer(sum(x[Cindices[[k]]]) > 0)
    })
  )
}) -> dd
names(dd)[grep("V[0-9]", names(dd))] <- c("Class1", "Class2", "Class3", 
                                          "Class4", "Class5", "Class6", "Class7")

# take out antidepressants
dd <- dd[,-unlist(Cindices)]
# take out response
y <- dd$gold_standard
dd <- dd[,-grep('gold_standard', names(dd))]
# save continuous variable
x <- dd$age
# save categorical variable
z <- as.matrix(dd[,-grep('age', names(dd))])

save.image("synth-allcols.RData")
