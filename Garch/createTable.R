library(texreg)


#平均
load("/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/使うデータ/NewMeant6t3.RData")


texreg(Sgarch_norm_Mean, 
          stars = NULL,
          caption = "得票率を応答変数とする単回帰の推定結果",
          caption.above = TRUE)

#分散 モデルになにも書いていないのが分散
load("/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/使うデータ/NewVart6t3.RData")


#絶対値
load("/Users/manbubble/Downloads/RedditAnalize/BitcoinGet/omit_Nan/使うデータ/NewAbst6t3.RData")


