'''
使用するデータ：Amazonでのインナーウェア製品ごとの各種データ（ブランド、価格、レビュー数など）
引用元：https://www.kaggle.com/PromptCloudHQ/innerwear-data-from-victorias-secret-and-others?select=amazon_com.csv
'''

#データの読み込み
ec.data <- read.csv("amazon_com.csv")
#データを見てみる
summary(ec.data)

'''
被説明変数Y : rating(ユーザーによる評価点の平均)
説明変数X : review_count(レビュー数)
コントロール変数Z : mrp(市場価格)
コントロール変数Z : product_category(製品の種類)
コントロール変数Z : brand_name(企業ブランド名)

と設定する
'''
#着目する変数のみにデータを絞る
ec.data <- ec.data[,c("product_name", "rating", "review_count", "mrp", "product_category", "brand_name")]
dim(ec.data)

#product_nameが重複しているレコードを削除する
library("dplyr")
ec.data <- ec.data[order(ec.data$product_name, ec.data$review_count, ec.data$mrp, ec.data$rating, decreasing=T),] #同じ製品の中でレビュー数、価格、ratingが最大のレコードのみ残せるように降順ソートしておく
ec.data <- ec.data %>% distinct(product_name, .keep_all = TRUE) #重複レコードの削除
dim(ec.data)

#mrpは"$xx.xx"というstring型データになっているので、"$"以降のみ抽出して数値型に変換
unique(ec.data[["mrp"]])
install.packages("stringr")
library("stringr")
ec.data$mrp <- as.numeric(str_sub(ec.data$mrp, start = 2))

# パッケージ読み込み
library(tidyverse)
install.packages("plotly")

# ヒストグラムで各データを見てみる
hist(x = ec.data[["rating"]])
hist(x = ec.data[["review_count"]])　#レビュー数がとびぬけて高いデータがある
hist(x = ec.data$mrp)

#空白セルがあるか確認
is.null(ec.data)

#productカテゴリーはブラかパンツの2種類のみ
unique(ec.data$product_category)
#ブランドネームは全5種。
unique(ec.data$brand_name)


# パッケージ読み込み
library(tidyverse) 
#散布図で見てみる
ggplot(ec.data, aes(x = review_count, y = rating)) +
  geom_point() +
  stat_smooth(method = "lm", se =TRUE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("review_count") + 
  ylab("rating") 

#散布図で見てみる
ggplot(ec.data, aes(x = mrp, y = rating)) +
  geom_point() +
  stat_smooth(method = "lm", se =TRUE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("mrp") + 
  ylab("rating") 

#散布図で見てみる
ggplot(ec.data, aes(x = mrp, y = review_count)) +
  geom_point() +
  stat_smooth(method = "lm", se =TRUE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("mrp") + 
  ylab("review_count") 

#データをみてみると、重複しているデータが多い。
library("dplyr")

