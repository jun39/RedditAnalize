install.packages("tidyr")
install.packages("base R")
install.packages("tidyverse")
install.packages('openxlsx')
install.packages("ggfortify")
library(openxlsx)
library(ggfortify)
library(dplyr, warn.conflicts = FALSE)

Bitcoin<- read.csv("/Users/manbubble/Downloads/RedditAnalize/BERT_PRE/HistBitcoin.csv")
Sentiment<- read.csv("/Users/manbubble/Downloads/RedditAnalize/BERT_PRE/CompleteSentiment.csv")
#外生変数の対数差処理 ビットコインと感情変数のlog対数差をやる
Bitcoin<-Bitcoin[,c(3)]
Bitcoin<-as.vector(Bitcoin,mode = "numeric")
BitcoinLogDiff<-diff(log(as.numeric(Bitcoin)))
Sentiment<-Sentiment[1:1309,]#一期ずらす
SentimentMatrix<-as.matrix(Sentiment[['sentiment']],mode = "numeric")


install.packages("fGarch")
install.packages("rugarch")
library(rugarch)
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

save(garchE_logdiff_std,garchE_logdiff_norm,Egarch_spec_std,Egarch_spec_norm,file="Completegarch.RData")
#manbabuleのとこにある　ファイルのパス指定を調べる
load("garch.RData")




#SentimentMatrixDiffでやってみる
SentimentMatrixDiff<-diff(SentimentMatrix)
Sentidiff_Egarch_spec_std<- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1),external.regressors=SentimentMatrixDiff),
  mean.model=list(armaOrder=c(2,0), include.mean=TRUE,external.regressors=SentimentMatrix),
  distribution.model = "std"
)

garchE_logdiff_std<-ugarchfit(
  spec = Sentidiff_Egarch_spec_std, data = BitcoinLogDiff, solver='hybrid'
)