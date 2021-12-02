from transformers import pipeline
import pandas as pd
# とりあえず事前学習のやつで感情指数作って解析してみる garchとか
#文章部分のリサーチ(ロビンフッター)などを考察、レポート引用などする
#ビットコインのポジねがの教師データ（テキストとラベル）探す。暗号通貨ならなんでもいいかも。無かったら自分で作っていい
#prawでとれない月とってみる　あとでも全然いい
#wordcloudとかで、高頻度の言葉とかとってみる
file='/Users/manbubble/Downloads/RedditAnalize/BERT_PRE/April2016size30.csv'
comments_df=pd.read_csv(file)
print(comments_df)
comments_df.iat[4,3]
df_4_3=comments_df.iat[4,3].replace('\n','')
classifier = pipeline('sentiment-analysis')
classifier(df_4_3)

#for文で3列目のbodyを取り出して配列に追加
#janomeとかの形態素で名詞だけ取り出して配列に追加
#text = ' '.join(words)、で一列にする
#https://www.haya-programming.com/entry/2018/03/21/234126
#nltkとかでやる
import nltk
from wordcloud import WordCloud
from textblob import TextBlob
nltk.download('brown')
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')

words=nltk.word_tokenize(df_4_3)

words_tag=nltk.pos_tag(words)
print(words_tag)
print(len(words_tag))
words_tag[0][1]


wordsList=[]
for i in range(0,len(comments_df)):
    string=comments_df.iat[i,3].replace('\n','')
    words = nltk.word_tokenize(string)
    words_tag=nltk.pos_tag(words)
    for j in range(0,len(words_tag)):
        if words_tag[j][1]=='NN':
            wordsList.append(words_tag[j][0])

wordsList

wordsList.remove('Bitcoin')
wordsList.remove('http')
wordsList.remove('https')



text = ' '.join(wordsList)
# bitcoinの単語だけ除く処理してもいいかも
# httpsも除く

fpath = "~/Library/Fonts//Arial Unicode.ttf"
wordcloud = WordCloud(background_color="white",
                      font_path=fpath, width=900, height=500).generate(text)

wordcloud.to_file("./wordcloud_sample.png")

# pythonでのgarchのやり方
# https://arch.readthedocs.io/en/latest/univariate/introduction.html