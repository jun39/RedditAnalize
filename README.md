# Description
Readitの投稿と、Bitcoin価格の関係を分析するコードです。

## BitcoinGet
BitcoinGet DirectoryにはgetBit.ipynbファイルがあり、https://www.cryptocompare.com/からビットコインの時間ごと価格データを取得できます。その他、ビットコインの６・３時間ごと平均価格を作成するなど、分析の前処理のコードがあります。


# 必要なライブラリ

APIキーを環境変数設定して使用する場合は、仮想環境に以下のpython-dotenvをインストールしてください
$ python -m venv ~/venv
$ source ~/venv/bin/activate
(venv) $ pip install python-dotenv