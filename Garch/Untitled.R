install.packages("tidyr")
install.packages("base R")
install.packages("tidyverse")
install.packages('openxlsx')
install.packages("ggfortify")
library(openxlsx)
library(ggfortify)
library(dplyr, warn.conflicts = FALSE)

Bitcoin<- read.csv("/Users/manbubble/Downloads/RedditAnalize/BERT_PRE/HistBitcoin.csv")
Sentiment<- read.csv("/Users/manbubble/Downloads/RedditAnalize/BERT_PRE/score.csv")
#外生変数の対数差処理 ビットコインと感情変数のlog対数差をやる
Bitcoin<-Bitcoin[,c(3)]
Bitcoin<-as.vector(Bitcoin,mode = "numeric")
BitcoinLogDiff<-diff(log(as.numeric(Bitcoin)))
Sentiment<-Sentiment[1:1309,]#一期ずらす
SentimentMatrix<-as.matrix(Sentiment$sentimentScore,mode = "numeric")

install.packages("fGarch")
install.packages("rugarch")
library(rugarch)

Gjrgarch_spec_std<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=SentimentMatrix),
  mean.model=list(armaOrder=c(0,0), include.mean=TRUE,external.regressors=SentimentMatrix),
  distribution.model = "std"
)
Gjrgarch_spec_norm<- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1),external.regressors=SentimentMatrix),
  mean.model=list(armaOrder=c(0,0), include.mean=TRUE,external.regressors=SentimentMatrix),
  distribution.model = "norm"
)
Gjrgarch_std<-ugarchfit(
  spec = Gjrgarch_spec_std, data = BitcoinLogDiff, solver='hybrid'
)

Gjrgarch_norm<-ugarchfit(
  spec = Gjrgarch_spec_norm, data = BitcoinLogDiff, solver='hybrid'
)
#対数差でegarchやる student-T分布
Egarch_spec_std<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=SentimentMatrix),
  mean.model=list(armaOrder=c(0,0), include.mean=TRUE,external.regressors=SentimentMatrix),
  distribution.model = "std"
)
Egarch_spec_norm<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=SentimentMatrix),
  mean.model=list(armaOrder=c(0,0), include.mean=TRUE,external.regressors=SentimentMatrix),
  distribution.model = "norm"
)

#対象のビットコインはベクトルでなければいけない
#対数差とったあとegarch
garchE_logdiff_std<-ugarchfit(
  spec = Egarch_spec_std, data = BitcoinLogDiff, solver='hybrid'
)
garchE_logdiff_norm<-ugarchfit(
  spec = Egarch_spec_norm, data = BitcoinLogDiff, solver='hybrid'
)

save(garchE_logdiff_std,garchE_logdiff_norm,Egarch_spec_std,Egarch_spec_norm,file="garch.RData")
#manbabuleのとこにある　ファイルのパス指定を調べる
load("garch.RData")
install.packages("stargazer")
library(stargazer)
stargazer(garchE_logdiff_norm, title="stargazer による回帰分析の結果")

install.packages("texreg")
library(texreg)

#define independent variable:


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

#print table:
texreg(extract.rugarch(garchE_logdiff_std, include.rsquared = FALSE)) #for latex # as R^2 is zero in this example.
texreg(extract.rugarch(garchE_logdiff_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_std, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_std, include.rsquared = FALSE))





Sgarch_spec_std<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=SentimentMatrix),
  mean.model=list(armaOrder=c(0,0), include.mean=TRUE,external.regressors=SentimentMatrix),
  distribution.model = "std"
)
Sgarch_spec_norm<- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),external.regressors=SentimentMatrix),
  mean.model=list(armaOrder=c(0,0), include.mean=TRUE,external.regressors=SentimentMatrix),
  distribution.model = "norm"
)

#対象のビットコインはベクトルでなければいけない
#対数差とったあとegarch
Sgarch_std<-ugarchfit(
  spec = Sgarch_spec_std, data = BitcoinLogDiff, solver='hybrid'
)
Sgarch_norm<-ugarchfit(
  spec = Sgarch_spec_norm, data = BitcoinLogDiff, solver='hybrid'
)

texreg(extract.rugarch(Sgarch_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_std, include.rsquared = FALSE))


