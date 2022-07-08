library(sjSDM)
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

m = sjSDM(Y = Ypa, 
          env = linear(X, XFormula), 
          spatial = linear(S, ~0+North*East+I(North^2)+I(East^2)), # trend surface model
          family = binomial(), 
          control = sjSDMControl(RMSprop(weight_decay = 0.0)))
summary(m)
fields::image.plot(cov2cor(getCov(m)))


an = anova(m)
plot(an)
plot(an, internal = TRUE)



m2 = sjSDM(Y = round(Y), 
           env = linear(X, XFormula), 
           spatial = linear(S, ~0+North*East+I(North^2)+I(East^2)), # trend surface model
           family = poisson(), 
           control = sjSDMControl(RMSprop(weight_decay = 0.0)), 
           dtype="float64"
           )
plot(importance(m2)) # equal to Hmsc varianceparitioning
plot(importance(m))
par(mfrow = c(1,2))
fields::image.plot(cov2cor(getCov(m)))
fields::image.plot(cov2cor(getCov(m2)))

g = plot(m2)
# an2 = anova(m2)
# plot(an2)
# plot(an2, internal = TRUE)


library(gllvm)
XFormula =   ~Unit_Class + scale(HIDRO_MEAN_250)+scale(NDVI_2000)+scale(DIST_RIO)+scale(DIST_ARROZ)+scale(DIST_urban)+North*East+I(North^2)+I(East^2) +poly(scale(LST_1), degree=2, raw=TRUE)

m3 = gllvm(y = round(Y), data = cbind(X, S), formula = XFormula, family = poisson(), num.lv = 3L)

par(mfrow = c(1,3))
fields::image.plot(cov2cor(getCov(m)))
fields::image.plot(cov2cor(getCov(m2)))
fields::image.plot( gllvm::getResidualCor(m3) )

# ploting correlations

cr <- getResidualCor(m3)
library(corrplot)
corrplot(cr, diag=FALSE, type='lower', method = 'square', tl.srt=25)

XFormula =  Cx.theiler ~  scale(HIDRO_MEAN_250)+scale(NDVI_2000)+scale(DIST_RIO)+scale(DIST_ARROZ)+scale(DIST_urban)+North*East+I(North^2)+I(East^2) +poly(scale(LST_1), degree=2, raw=TRUE) + (1|Unit_Class)

library(glmmTMB)
m4 = glmmTMB(XFormula, data = cbind(round(Y), X, S),family = poisson())
ranef(m4)


m5 <- gllvm(y=round(Y), family = poisson()) 
cr0 <- getResidualCor(m5)
corrplot(cr0, diag = FALSE, type = "full", 
         method = "square", tl.srt = 25)
library(corrplot)

par(mfrow = c(1,2))
