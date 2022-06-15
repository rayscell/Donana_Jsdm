# This model is linear model for HMSC ROIZ DATA without the culex theilerri data that affect model fittings.

getwd()
# READING IN SXY: study design (S) and/or covariates (X) and species data (Y) 
SXY = read.csv("Msq_donana_data.csv", stringsAsFactors=TRUE, header = TRUE) # or use read.csv2 when values are semicolon-separated
# S: study design, including units of study and their possible coordinates (named as Route_x and Route_y to indicate that they relate to the units of Route)
# X: covariates to be used as predictors
# Y: species data
# If you don't have variables that define the study design, indicate this by S=NULL
# If you don't have covariate data, indicate this by X=NULL
View(SXY)
str(SXY)

# colnames(SXY) <- c("fid", "field_1","trap", "trapnb", "Unit", "Unit_Class", "Unit_code",  "Oc.detritu", "Oc.caspius", 
#                    "Cx.theiler", "Cx.pipiens", "Cx.modestu", "Cx.perexig", "An.atropar", "IShannon", "Total_mosq", "Riqueza_me", 
#                    "HIDRO_MEAN_250", "HIDRO_MEAN_500", "HIDRO_MEAN_1000", "HIDRO_MEAN_2000", "LULC_100",   "LULC_250",   "LULC_500",
#                    "LULC_1000",  "LULC_2000",  "NDVI_100",   "NDVI_250",   "NDVI_500",   "NDVI_1000",  "NDVI_2000",  "DIST_RIO",
#                    "DIST_ARROZ", "DIST_urban", "Norte","Oeste", "LST_1")

#write.csv(SXY, "Msq_donana_data.csv", row.names = FALSE)
#variable selection.
#checking collinearity in the predictor

tPred<- SXY[, 18:37]
View(tPred)
tPred <-tPred[, -c(5:9, 18,19)]
View(tPred)

# install.packages('usdm')
library(usdm) # for performing variance inflation factor etc
tPred_vif <- vif(tPred)
tPred_vif

tPred_vifCor <- vifcor(tPred, th=0.7)

tPred_vifCor


tPred_vifstep <- vifstep(tPred, th=5)
tPred_vifstep 

df_sp <- data.frame(tPred, SXY$Unit_code)
View(df_sp)

# rename column name from spanish to english
names(SXY)[35] <- paste0("North")
names(SXY)[36] <- paste0("East")

# studyDesign=SXY[, c("Unit_Class", "trap")]
# studyDesign= data.frame(apply(studyDesign, 2, as.factor))
# class= HmscRandomLevel(units = studyDesign$Unit_Class)
# trap= HmscRandomLevel(units= studyDesign$trap)


S=SXY[, c(3, 35:36)]
X=SXY[,c(6, 18, 31, 32, 33, 34, 37)] # Predictors 
Y=SXY[,c(8:14)] # response variable
View(Y)

Y2 <- Y
i <- c(1:7)
# convert the decimal number into nearest interger
Y2[,i ] <- apply(Y2[, i], 2, function(x)as.numeric(ceiling(x)))
View(Y2)




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


