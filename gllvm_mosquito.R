library(gllvm)
getwd()
localDir="."
ModeDir <- file.path(localDir, 'models') # directory for the models
DataDir <- file.path(localDir, "data") # directory for the data 

load(file = file.path(DataDir, "allData.R")) # load the saved data
head(S)
head(X)
head(Y)
dim(Y)

length(unique(S$trap))

S$North = scale(S$North)
S$East = scale(S$East)

XFormula =   ~  Unit_Class + scale(HIDRO_MEAN_250)+scale(NDVI_2000)+scale(DIST_RIO)+scale(DIST_ARROZ)+scale(DIST_urban)+poly(scale(LST_1), degree=2, raw=TRUE)  
Ypa = 1*(apply(log(Y+1), 2, function(i) i > mean(i)))


cr <- getResidualCor(m3)
library(corrplot)
corrplot(cr, diag=FALSE, type='lower', method = 'square', tl.srt=25)
