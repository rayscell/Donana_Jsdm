
#### define Model types #####
# set the working directore
 # define the path
# dir.create(paste0(path, "models/")) 
# dir.create(paste0(path, "data/"))
getwd()
localDir="."
ModeDir <- file.path(localDir, 'models') # directory for the models
DataDir <- file.path(localDir, "data") # directory for the data 
library(Hmsc)

load(file = file.path(DataDir, "allData.R")) # load the saved data
head(S)
head(X)
head(Y)
dim(Y)

length(unique(S$trap))



#for subsampling 
# set.seed(222)
# seltrap = S$trap %in% sample(unique(S$trap), 50)
# S=droplevels(S[seltrap,])
# X=droplevels(X[seltrap,])
# Y=Y[seltrap,]


# check for absent (0), or ubiquitous species (1)
range(colMeans(Y2>0))
min(colMeans(Y>0))
raretaxa = which(colSums(Y>0)<5)
length(raretaxa)

hist(colMeans(Y>0), main = 'Prevalence')
hist(log(Y[Y>0]), main = 'log abundance conditional on Presence')


hist(rowSums(Y>0)) # species richness across samples
names(X)
XFormula =   ~  Unit_Class + HIDRO_MEAN_250+NDVI_2000+DIST_RIO+DIST_ARROZ+DIST_urban+poly(LST_1, degree=2, raw=TRUE)  

sum(duplicated(S[,1:3]))
studyDesign <- data.frame(trap= as.factor(S$trap))
head(studyDesign)
xy <- data.frame(S[match(unique(S$trap), S$trap), 2:3])
dim(xy)
rownames(xy) <- unique(S$trap)
head(xy)
colnames(xy) <- c("Latitude", "Longitude")
# xy <- xy[,c(2,1)]
head(xy)
rL.trap <- HmscRandomLevel(sData = xy)

# plot(xy)

Ypa <- 1*(Y>0)
Yabu <- Y
Yabu[Y==0] =NA
Yabu = log(Yabu)

#abundance model conditioned on presence
# #abundance conditional on presence 
m1 <- Hmsc(Y=Y, XData = X, XFormula = XFormula,
           distr = "normal",
           studyDesign = studyDesign,
           ranLevels = {list("trap"=rL.trap)})
m2 = Hmsc(Y=Yabu,YScale = TRUE, XData = X, XFormula = XFormula,
          distr = "normal",
          studyDesign = studyDesign,
          ranLevels = {list("trap"=rL.trap)})

m3 = Hmsc(Y=Y2, XData = X, XFormula = XFormula,
          distr = "normal",
          studyDesign = studyDesign,
          ranLevels = {list("trap"=rL.trap)})


models <- list(m1, m2, m3)
modelnames <- c("ABU","ACOP", "AABU")
save(models, modelnames, file=file.path(ModeDir, "unfitted_models"))
m1 #observe the model fitting
m2
m3
