rm(list=ls())

## an alternative function to create folds, allows for stratification by covariate, e.g. treatment

create_folds<-function(datain=NULL, StrataVars=NULL, nfolds=2, random=FALSE, rseed=NULL){
  require(plyr)
  if (is.null(datain)) stop("Dataset not specified")
  
  if (random==TRUE){
    if (is.null(rseed)==FALSE) {set.seed(rseed)}
  }
  
  if (is.null(StrataVars)){
    StrataVars<-c("dummy")
    datain[,StrataVars]<-1
  }
  
  StratTemplate<-data.frame(plyr::count(datain, StrataVars))
  StratTemplate$nStratum<-as.numeric(rownames(StratTemplate))
  datain.temp<-merge(datain, StratTemplate, by.x=StrataVars, by.y=StrataVars, all.x=TRUE)
  
  cumn<-0
  datain.temp$recn<-as.numeric(rownames(datain.temp))
  
  for (s in 1:nrow(StratTemplate)){
    fold.Size<-StratTemplate[s,'freq'] %/% nfolds
    datain.temp[which(datain.temp$nStratum==s),'recn']<-datain.temp[which(datain.temp$nStratum==s),'recn'] - cumn
    datain.temp[which(datain.temp$nStratum==s),'fold']<-ceiling(datain.temp[which(datain.temp$nStratum==s),'recn']/fold.Size)
    datain.temp[which(datain.temp$fold > nfolds),'fold']<-nfolds
    
    if (random==TRUE){
      datain.temp[which(datain.temp$nStratum==s),'fold']<-sample(datain.temp[which(datain.temp$nStratum==s),'fold'])
    }
    
    cumn<-cumn+StratTemplate[s,'freq']
  }
  
  datain.temp[,'freq']<-NULL
  
  if (StrataVars=="dummy"){datain.temp[,'dummy']<-NULL}
  
  return(datain.temp)
}

### a helper function compute cross-fitted taus for evalITR
fun.estimate.Taus.CF <- function(data,  what.y, 
                                 ps_hat=NULL,  ## use rep(pi, n) if RCT
                                 NO.FOLDS = 10, # default
                                 BIOMARKERS,
                                 NTREE = 10000,
                                 stratify.by.trt=FALSE
) 
  
{
  i.response <- which(names(data)==what.y)
  names(data)[i.response] <- "y"
  n <- nrow(data)
  if (stratify.by.trt) {
  # alternative function that allows cross-validation stratified stratificatoin by treatment
   data=create_folds(datain=data, StrataVars="trt", nfolds=NO.FOLDS, random=TRUE, rseed=1234)
  } else { 
    # note need group parameter, group sizes for ranking/grouped data (optional)  
    require(xgboost)  
    L.folds <- xgboost:::generate.cv.folds(nrows = n, nfold = NO.FOLDS, 
                                           params=list(NULL), stratified = FALSE,group=NULL)
    ## assign each obs to a fold
    data$fold = NA # init
    for(curr.fold in 1:NO.FOLDS) {
      obs.in.current.fold <- unlist(L.folds[curr.fold])
      data$fold[obs.in.current.fold] <- curr.fold
    }
  }  
  ## now each patient assigned to a fold, not easy to read but easy to see in console
  # table(data$fold)
  X <- as.matrix(data[,BIOMARKERS])
  ## Init object TAU that later will be given to heter.cv in this way:
  TAU <- data.frame(matrix(NA, nrow(data), ncol=NO.FOLDS)) 
  names(TAU) <- paste0("tau", 1:NO.FOLDS)
  TAU$trt <- data$trt  ## needed later
  TAU$ind <- data$fold ## critical 
  TAU$Y <- data$y ## needed later
  for(iter in 1:NO.FOLDS) {
    print(paste(" ////////  Doing CrossVal.iter nr ", iter))
    THIS.TRAINING <- data$fold != iter
    data.train <- data[THIS.TRAINING, ]  
    data.test <- data[!THIS.TRAINING, ]  
    tau.forest <- causal_forest(X=X[THIS.TRAINING,], 
                                W.hat = ps_hat[THIS.TRAINING],
                                W=data.train$trt, 
                                Y=data.train$y,
                                tune.parameters = "all", # as used in Imai et all sim example, we used default "none"
                                num.trees = NTREE)
    # Estimate treatment effects for the training data using out-of-bag prediction.
    aa <- predict(tau.forest,newdata=X[THIS.TRAINING,])  # training. 
    aa0 <- predict(tau.forest, newdata=X[!THIS.TRAINING,])  ## NOT training
    muhat.cf  <- aa$predictions    # train
    muhat.cf0  <- aa0$predictions  # test predict
    ## plug-in the test-data-estimated CATE in the right spot:
    CURRENT.TEST.DATA.SITS.HERE <- TAU$ind==iter
    CURRENT.TRAIN.DATA.SITS.HERE <- TAU$ind!=iter
    CURRENT.column.in.TAU <- iter
    TAU[CURRENT.TEST.DATA.SITS.HERE, CURRENT.column.in.TAU] <- muhat.cf0
    TAU[CURRENT.TRAIN.DATA.SITS.HERE, CURRENT.column.in.TAU] <- muhat.cf # let's assume the order of subjects is ok here.
  }#EndFor.CrossFitting
  Taus <- as.matrix(TAU[,1:NO.FOLDS])
  ret <- list(TAU.allInfo=TAU, 
              Taus=Taus,
              trt=TAU$trt,
              Y=TAU$Y,
              ind=TAU$ind,
              NO.FOLDS=NO.FOLDS, method="CausalForest")
  ret


###############################################################################
##### use evalITR function  hetcv.test to evaluate presence eof treatment effect
###############################################################################

setwd('/Users/ilyalipkovich/Dropbox/Documents/Statistics/JSM 2026 tutorial/case study')
library(evalITR)
require(xgboost)
require(grf)

dd  = read.csv("casestudy_generated.csv", header = TRUE, stringsAsFactors=FALSE,na=".")


colnames(dd)
BIOMARKERS = c("age",       
               "diagyears", 
               "pansspos",  
               "panssneg",
               "panssgen",
               "nRacea",   
               "nRaceb", 
               "nGender",
               "cgis"
)

X = dd[, BIOMARKERS]  
X.matrix = as.matrix(X)

# install evalITR from gitgub
#  install.packages("remotes")
#  install.packages("devtools")

#  remotes::install_github("MichaelLLi/evalITR")
#  https://github.com/MichaelLLi/evalITR/issues

seed <- 123456
## Proportion of subjects assigned to the active 
pA = sum(dd$trt==1)/(sum(dd$trt==0)+sum(dd$trt==1))

pi = rep(pA, nrow(dd)) ## treatment propensity for each subject (based on actual randomization ratio)
set.seed(seed)
head(pi,3); length(pi)


## evalITR assumes larger Y is beneficial for patient
dd$y.panss42.rev = (-1) * dd$y.panss42         # the outcome with no HTE
dd$y.panss42.gen.rev = (-1) * dd$y.panss42.gen # outcome with subgroup effect planted

TAU.obj.CF <- fun.estimate.Taus.CF(data = dd, 
                                   what.y="y.panss42.rev",
                                   NO.FOLDS = 10,  
                                   ps_hat = pi,
                                   BIOMARKERS = BIOMARKERS,
                                   NTREE = 10000,
                                   stratify.by.trt=TRUE
) 

ht.CF <- hetcv.test(T=TAU.obj.CF[["trt"]],
                    tau=TAU.obj.CF[["Taus"]],
                    Y=TAU.obj.CF[["Y"]],
                    ind=TAU.obj.CF[["ind"]],
                    ngates=5 ## Default 
)
ht.CF

#### apply to generated outcome 
set.seed(seed)

TAU.obj.CF.alt <- fun.estimate.Taus.CF(data = dd, 
                                       what.y="y.panss42.gen.rev",
                                       NO.FOLDS = 10,  
                                       ps_hat = pi,
                                       BIOMARKERS = BIOMARKERS,
                                       NTREE = NTREE.CF,
                                       stratify.by.trt=FALSE
) 

ht.CF.alt <- hetcv.test(T=TAU.obj.CF.alt[["trt"]],
                        tau=TAU.obj.CF.alt[["Taus"]],
                        Y=TAU.obj.CF.alt[["Y"]],
                        ind=TAU.obj.CF.alt[["ind"]],
                        ngates=5 ## Default 
)
ht.CF.alt$pval

