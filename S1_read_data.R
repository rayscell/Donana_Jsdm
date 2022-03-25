# This model is linear model for HMSC ROIZ DATA without the culex theilerri data that affect model fittings.


# You need to provide an SXY file.
# The files TP and P are optional, so indicate with TRUE/FALSE if they are included or not
is.TP = FALSE
is.P = FALSE


getwd()
# READING IN SXY: study design (S) and/or covariates (X) and species data (Y) 
SXY = read.csv("Msq_donana_data.csv", stringsAsFactors=TRUE, header = TRUE) # or use read.csv2 when values are semicolon-separated
# Modify the next three lines to split your SXY file to components that relate to
# S: study design, including units of study and their possible coordinates (named as Route_x and Route_y to indicate that they relate to the units of Route)
# X: covariates to be used as predictors
# Y: species data
# If you don't have variables that define the study design, indicate this by S=NULL
# If you don't have covariate data, indicate this by X=NULL
View(SXY)

#variable selection.
#checking collinearity in the predictor

tPred<- SXY[, 18:37]
View(tPred)
tPred <-tPred[, -c(5:9, 18,19)]
View(tPred)

library(car) # to use the variance inflation factors to check multicoliniarity

# install.packages('usdm')
library(usdm)
tPred_vif <- vif(tPred)
tPred_vif

tPred_vifCor <- vifcor(tPred, th=0.7)

tPred_vifCor


tPred_vifstep <- vifstep(tPred, th=5)
tPred_vifstep 

df_sp <- data.frame(tPred, SXY$Unit_code)
View(df_sp)

names(SXY)[18] <- paste0('HIDRO_MEAN_250')
names(SXY)[35] <- paste0("North")
names(SXY)[36] <- paste0("East")

# studyDesign=SXY[, c("Unit_Class", "trap")]
# studyDesign= data.frame(apply(studyDesign, 2, as.factor))
# class= HmscRandomLevel(units = studyDesign$Unit_Class)
# trap= HmscRandomLevel(units= studyDesign$trap)


S=SXY[, c(3, 35:36)]
X=SXY[,c(6, 18, 31, 32, 33, 34, 37)] 
Y=SXY[,c(8:14)]  
View(Y)

Y2 <- Y
i <- c(1:7)
# convert the decimal number into nearest interger
Y2[,i ] <- apply(Y2[, i], 2, function(x)as.numeric(ceiling(x)))
View(Y2)



# What is not always easy is to decide what goes to S and what to X.
# As a general rule, include in S those variables that you think should be modelled as random effect,
# and in X those that you think should be modelled as fixed effects.
# Don't worry if you are not sure which one is the "right choice", we will discuss this with you.

# Check that the data looks as it should!
View(S)
View(X)
View(Y)
# check that community data are numeric and have finite numbers. If the script
# writes "Y looks OK", you are ok.
if (is.numeric(as.matrix(Y)) || is.logical(as.matrix(Y)) && is.finite(sum(Y, na.rm=TRUE))) {
    print("Y looks OK")
} else {
	print("Y should be numeric and have finite values")	}
if (is.numeric(as.matrix(Y2)) || is.logical(as.matrix(Y2)) && is.finite(sum(Y2, na.rm=TRUE))) {
  print("Y2 looks OK")
} else {
  print("Y2 should be numeric and have finite values")	}
# Check that the stydy design data do not have missing values (they are allowed for Y but not S, X, P or Tr)
if (any(is.na(S))) {
  print("S has NA values - not allowed for")
} else {
  print("S looks ok")	}
# Check that the covariate data do not have missing values (they are allowed for Y but not S, X, P or Tr)
if (any(is.na(X))) {
  print("X has NA values - not allowed for")
} else {
  print("X looks ok")	}




getwd()
Y=as.matrix(Y)
Y2=as.matrix(Y2)
save(S,X,Y,Y2, file = 'data/allData.R')


