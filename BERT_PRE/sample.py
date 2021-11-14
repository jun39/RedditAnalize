from transformers import pipeline
import pandas as pd
file='/Users/manbubble/Downloads/RedditAnalize/BERT_PRE/April2016size30.csv'
pd.read_csv(file)
classifier = pipeline('sentiment-analysis')

print(classifier())