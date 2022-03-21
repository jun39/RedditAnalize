#時間ごとのやつ

library(openxlsx)
library(ggfortify)
library(dplyr, warn.conflicts = FALSE)

data_abs<-read.csv("/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/omitNan_abs.csv")
data_mean<-read.csv("/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/omitNan_mean.csv")
data_var<-read.csv("/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/omitNan_var.csv")


NewBitcoin_abs<-data_abs[,c(4)]
NewBitcoin_abs<-as.vector(NewBitcoin_abs,mode = "numeric")

NewBitcoin_mean<-data_mean[,c(4)]
NewBitcoin_mean<-as.vector(NewBitcoin_mean,mode = "numeric")

NewBitcoin_var<-data_var[,c(4)]
NewBitcoin_var<-as.vector(NewBitcoin_var,mode = "numeric")




#時間があったらNewsentimentを１期ずらすやつやる
VarSentimentMatrix<-as.matrix(data_var[,c(6,7)],mode = "numeric")
AbsSentimentMatrix<-as.matrix(data_abs[,c(6,7)],mode = "numeric")
MeanSentimentMatrix<-as.matrix(data_mean[,c(6,7)],mode = "numeric")

library(rugarch)

Egarch_spec_std<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=VarSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=VarSentimentMatrix),
  distribution.model = "std"
)
Egarch_spec_norm<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=VarSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=VarSentimentMatrix),
  distribution.model = "norm"
)

#対象のビットコインはベクトルでなければいけない
#対数差とったあとegarch
garchE_logdiff_std<-ugarchfit(
  spec = Egarch_spec_std, data = NewBitcoin_var, solver='hybrid'
)
garchE_logdiff_norm<-ugarchfit(
  spec = Egarch_spec_norm, data = NewBitcoin_var, solver='hybrid'
)

extract.rugarch <- function(fit, 
                            include.rsquared = TRUE, include.loglike = TRUE, include.aic = TRUE, include.bic = TRUE) {
  
  # extract coefficient table from fit:
  coefnames <- rownames(as.data.frame(fit@fit$coef))
  coefs <- fit@fit$coef
  se <- as.vector(fit@fit$matcoef[, c(2)])
  pvalues <-  as.vector(fit@fit$matcoef[, c(4)])       # numeric vector with p-values
  
  # create empty GOF vectors and subsequently add GOF statistics from model:
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.rsquared == TRUE) {
    r2 <-  1 - (var(fit@fit$residuals) / var(y))
    gof <- c(gof, r2)
    gof.names <- c(gof.names, "R^2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglike == TRUE) {
    loglike <- fit@fit$LLH
    gof <- c(gof, loglike)
    gof.names <- c(gof.names, "Log likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aic == TRUE) {
    aic <- infocriteria(fit)[c(1)]
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  if (include.bic == TRUE) {
    bic <- infocriteria(fit)[c(2)]
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  # create texreg object:
  tr <- createTexreg(
    coef.names = coefnames, 
    coef = coefs,
    se = se,
    pvalues = pvalues, 
    gof.names = gof.names, 
    gof = gof, 
    gof.decimal = gof.decimal
  )
  return(tr)
}

Sgarch_spec_std<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=VarSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=VarSentimentMatrix),
  distribution.model = "std"
)
Sgarch_spec_norm<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=VarSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=VarSentimentMatrix),
  distribution.model = "norm"
)

Sgarch_std<-ugarchfit(
  spec = Sgarch_spec_std, data = NewBitcoin_var, solver='hybrid'
)
Sgarch_norm<-ugarchfit(
  spec = Sgarch_spec_norm, data = NewBitcoin_var, solver='hybrid'
)

Gjrgarch_spec_std<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=VarSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=VarSentimentMatrix),
  distribution.model = "std"
)
Gjrgarch_spec_norm<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=VarSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=VarSentimentMatrix),
  distribution.model = "norm"
)
Gjrgarch_std<-ugarchfit(
  spec = Gjrgarch_spec_std, data = NewBitcoin_var, solver='hybrid'
)

Gjrgarch_norm<-ugarchfit(
  spec = Gjrgarch_spec_norm, data = NewBitcoin_var, solver='hybrid'
)

install.packages("texreg")
library(texreg)
#print table:
texreg(extract.rugarch(garchE_logdiff_std, include.rsquared = FALSE)) #for latex # as R^2 is zero in this example.
texreg(extract.rugarch(garchE_logdiff_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_std, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_std, include.rsquared = FALSE))

save(garchE_logdiff_std,garchE_logdiff_norm,Gjrgarch_norm,Gjrgarch_std,Sgarch_norm,Sgarch_std,file="/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/NewVart6t3.RData")
#manbabuleのとこにある　ファイルのパス指定を調べる
load("NewVart6t3.RData")


#ここから先は絶対値
Egarch_spec_std_Abs<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=AbsSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=AbsSentimentMatrix),
  distribution.model = "std"
)
Egarch_spec_norm_Abs<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=AbsSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=AbsSentimentMatrix),
  distribution.model = "norm"
)

#対象のビットコインはベクトルでなければいけない
#対数差とったあとegarch
garchE_logdiff_std_Abs<-ugarchfit(
  spec = Egarch_spec_std_Abs, data = NewBitcoin_abs, solver='hybrid'
)
garchE_logdiff_norm_Abs<-ugarchfit(
  spec = Egarch_spec_norm_Abs, data = NewBitcoin_abs, solver='hybrid'
)

Sgarch_spec_std_Abs<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=AbsSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=AbsSentimentMatrix),
  distribution.model = "std"
)
Sgarch_spec_norm_Abs<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=AbsSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=AbsSentimentMatrix),
  distribution.model = "norm"
)

Sgarch_std_Abs<-ugarchfit(
  spec = Sgarch_spec_std_Abs, data = NewBitcoin_abs, solver='hybrid'
)
Sgarch_norm_Abs<-ugarchfit(
  spec = Sgarch_spec_norm_Abs, data = NewBitcoin_abs, solver='hybrid'
)

Gjrgarch_spec_std_Abs<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=AbsSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=AbsSentimentMatrix),
  distribution.model = "std"
)
Gjrgarch_spec_norm_Abs<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=AbsSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=AbsSentimentMatrix),
  distribution.model = "norm"
)
Gjrgarch_std_Abs<-ugarchfit(
  spec = Gjrgarch_spec_std_Abs, data = NewBitcoin_abs, solver='hybrid'
)

Gjrgarch_norm_Abs<-ugarchfit(
  spec = Gjrgarch_spec_norm_Abs, data = NewBitcoin_abs, solver='hybrid'
)

texreg(extract.rugarch(garchE_logdiff_std_Abs, include.rsquared = FALSE)) #for latex # as R^2 is zero in this example.
texreg(extract.rugarch(garchE_logdiff_norm_Abs, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_norm_Abs, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_std_Abs, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_norm_Abs, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_std_Abs, include.rsquared = FALSE))

save(garchE_logdiff_std_Abs,garchE_logdiff_norm_Abs,Gjrgarch_norm_Abs,Gjrgarch_std_Abs,Sgarch_norm_Abs,Sgarch_std_Abs,file="/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/NewAbst6t3.RData")
#manbabuleのとこにある　ファイルのパス指定を調べる
load("NewAbst6t3.RData")


#平均
Egarch_spec_std_Mean<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=MeanSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=MeanSentimentMatrix),
  distribution.model = "std"
)
Egarch_spec_norm_Mean<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=MeanSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=MeanSentimentMatrix),
  distribution.model = "norm"
)

garchE_logdiff_std_Mean<-ugarchfit(
  spec = Egarch_spec_std_Abs, data = NewBitcoin_mean, solver='hybrid'
)
garchE_logdiff_norm_Mean<-ugarchfit(
  spec = Egarch_spec_norm_Abs, data = NewBitcoin_mean, solver='hybrid'
)

Sgarch_spec_std_Mean<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=MeanSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=MeanSentimentMatrix),
  distribution.model = "std"
)
Sgarch_spec_norm_Mean<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=AbsSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=AbsSentimentMatrix),
  distribution.model = "norm"
)

Sgarch_std_Mean<-ugarchfit(
  spec = Sgarch_spec_std_Abs, data = NewBitcoin_mean, solver='hybrid'
)
Sgarch_norm_Mean<-ugarchfit(
  spec = Sgarch_spec_norm_Abs, data = NewBitcoin_mean, solver='hybrid'
)

Gjrgarch_spec_std_Mean<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=MeanSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=MeanSentimentMatrix),
  distribution.model = "std"
)
Gjrgarch_spec_norm_Mean<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=MeanSentimentMatrix),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=MeanSentimentMatrix),
  distribution.model = "norm"
)
Gjrgarch_std_Mean<-ugarchfit(
  spec = Gjrgarch_spec_std_Abs, data = NewBitcoin_mean, solver='hybrid'
)

Gjrgarch_norm_Mean<-ugarchfit(
  spec = Gjrgarch_spec_norm_Abs, data = NewBitcoin_mean, solver='hybrid'
)

texreg(extract.rugarch(garchE_logdiff_std_Mean, include.rsquared = FALSE)) #for latex # as R^2 is zero in this example.
texreg(extract.rugarch(garchE_logdiff_norm_Mean, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_norm_Mean, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_std_Mean, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_norm_Mean, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_std_Mean, include.rsquared = FALSE))

save(garchE_logdiff_std_Mean,garchE_logdiff_norm_Mean,Gjrgarch_norm_Mean,Gjrgarch_std_Mean,Sgarch_norm_Mean,Sgarch_std_Mean,file="/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/NewMeant6t3.RData")
