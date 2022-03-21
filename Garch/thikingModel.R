library(openxlsx)
library(ggfortify)
library(dplyr, warn.conflicts = FALSE)

library(rugarch)
library(texreg)

#絶対値
load("/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/使うデータ/NewAbst6t3.RData")

#分散 モデルになにも書いていないのが分散
load("/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/使うデータ/NewVart6t3.RData")

#平均
load("/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/使うデータ/NewMeant6t3.RData")



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
#分散のやつ
texreg(extract.rugarch(garchE_logdiff_std, include.rsquared = FALSE)) #for latex # as R^2 is zero in this example.
texreg(extract.rugarch(garchE_logdiff_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_std, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_norm, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_std, include.rsquared = FALSE))

#絶対値のやつ
texreg(extract.rugarch(garchE_logdiff_std_Abs, include.rsquared = FALSE)) #for latex # as R^2 is zero in this example.
texreg(extract.rugarch(garchE_logdiff_norm_Abs, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_norm_Abs, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_std_Abs, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_norm_Abs, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_std_Abs, include.rsquared = FALSE))


#平均のやつ
texreg(extract.rugarch(garchE_logdiff_std_Mean, include.rsquared = FALSE)) #for latex # as R^2 is zero in this example.
texreg(extract.rugarch(garchE_logdiff_norm_Mean, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_norm_Mean, include.rsquared = FALSE))
texreg(extract.rugarch(Gjrgarch_std_Mean, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_norm_Mean, include.rsquared = FALSE))
texreg(extract.rugarch(Sgarch_std_Mean, include.rsquared = FALSE))

