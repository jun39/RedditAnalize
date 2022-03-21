from datetime import datetime
import pandas as pd
import requests #Pushshift accesses Reddit via an url so this is needed
import json #JSON manipulation
import csv #To Convert final table into a csv file to save to your machine
import time
import datetime

# 2016年はうるう年
size = 30 
timelist = []

# for i in range(1,31+1):
#     timelist.append(datetime.datetime(2018,1,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,2,1,0,0).strftime('%s'))

# ２月と３月だけとれない prawで試す　https://github.com/dmarx/psaw


for i in range(1,28+1):
    timelist.append(datetime.datetime(2018,2,i,0,0).strftime('%s'))

timelist.append(datetime.datetime(2018,3,1,0,0).strftime('%s'))

# for i in range(1,31+1):
#     timelist.append(datetime.datetime(2018,3,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,4,1,0,0).strftime('%s'))

# for i in range(1,30+1):
#     timelist.append(datetime.datetime(2018,4,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,5,1,0,0).strftime('%s'))

# for i in range(1,31+1):
#     timelist.append(datetime.datetime(2018,5,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,6,1,0,0).strftime('%s'))

# for i in range(1,30+1):
#     timelist.append(datetime.datetime(2018,6,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,7,1,0,0).strftime('%s'))

# for i in range(1,31+1):
#     timelist.append(datetime.datetime(2018,7,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,8,1,0,0).strftime('%s'))

# for i in range(1,31+1):
#     timelist.append(datetime.datetime(2018,8,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,9,1,0,0).strftime('%s'))

# for i in range(1,30+1):
#     timelist.append(datetime.datetime(2018,9,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,10,1,0,0).strftime('%s'))

# for i in range(1,31+1):
#     timelist.append(datetime.datetime(2018,10,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,11,1,0,0).strftime('%s'))

# for i in range(1,30+1):
#     timelist.append(datetime.datetime(2018,11,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,12,1,0,0).strftime('%s'))

# for i in range(1,31+1):
#     timelist.append(datetime.datetime(2018,12,i,0,0).strftime('%s'))

# timelist.append(datetime.datetime(2018,1,1,0,0).strftime('%s'))



# 7200000を引いていく これを15回
#2018/04/01 00:00:00 1522508400
#11/01から15回引いたやつ　1527692400
# 2021/11/01 00:00:00　1635692400
#1,310日
#この下が15回の次の端数のやつ
#https://min-api.cryptocompare.com/data/v2/histohour?fsym=BTC&tsym=USD&limit=1440&toTs=1527692400&api_key=3cd35a30a6ff506c1809f844f470f9115b405d3927a0d576f5d2916c16b07e94

def getPushshiftData(query, after, before):
    #Build URL
    # toTsにどこまでのタイムスタンプを入れるかでそこから前のlimitのかずだけ時間データがとれる
    url='https://min-api.cryptocompare.com/data/v2/histohour?fsym=BTC&tsym=USD&limit=2000&toTs='+str(query)+'&api_key=3cd35a30a6ff506c1809f844f470f9115b405d3927a0d576f5d2916c16b07e94'
    #Print URL to show user
    print(url)
    #Request URL
    r = requests.get(url)
    print(r.headers.get('content-type'))
    data = json.loads(r.text)
    #Load JSON data from webpage into data variable

    #return the data element which contains all the submissions data
    return data['data']

def collectSubData(subm):
    #subData was created at the start to hold all the data which is then added to our global subStats dictionary.
    subData = list() #list to store data points
    author = subm['author']
    score = subm['score']
    body = subm['body']
    comment_id = subm['id']
    created = datetime.datetime.fromtimestamp(subm['created_utc']) #1520561700.0

    #Put all data points into a tuple and append to subData
    subData.append((comment_id,author,score,body,created))
    #Create a dictionary entry of current submission data and store all data related to it
    subStats[comment_id] = subData


query = "Bitcoin" #Keyword(s) to look for in submissions


#subStats is the dictionary where we will store our data.
subStats = {}

# ここにwhileでtimelistをbefore afterに入れる
for i in range(0,len(timelist)):
    if i==(len(timelist)-1):
        break
    after = timelist[i]
    before = timelist[i+1]
    data = getPushshiftData(query, after, before)
    time.sleep(3)
    for submission in data:
        collectSubData(submission)

def updateSubs_file():
    upload_count = 0
    # location = "\\Reddit Data\\" >> If you're running this outside of a notebook you'll need this to direct to a specific location
    # print("input filename of submission file, please add .csv")



    filename="Feb2_2018size30.csv"
    # ファイルの名前を変える


    # filename = input() #This asks the user what to name the file
    file = filename
    with open(file, 'w', newline='', encoding='utf-8') as file: 
        a = csv.writer(file, delimiter=',')
        headers = ["Comment ID","Author","Score","Body","Publish Date"]
        a.writerow(headers)
        for sub in subStats:
            a.writerow(subStats[sub][0])
            upload_count+=1
            
        print(str(upload_count) + " submissions have been uploaded")


updateSubs_file()