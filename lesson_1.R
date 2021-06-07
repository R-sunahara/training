#ggplot2パッケージのインストール
install.packages("ggplot2")
library(ggplot2)

#サンプルデータの読み込み（https://gihyo.jp/book/2013/978-4-7741-5896-9/support）
body.data <- read.csv("body_sample.csv", header = T, stringsAsFactors = F)
head(body.data)

#summary　（標準偏差や不偏分散は表示されない）
summary(body.data)

#標準偏差
sd(body.data$height)
sd(body.data[,"height"])

#不偏分散
var(body.data[,"height"])

#ヒストグラムでデータを見てみる
ggplot(data = body.data, aes(x = height)) + geom_histogram() +theme_bw(16) + ylab("count")

ggplot(data = body.data, aes(x = height, fill = gender)) + geom_histogram() +theme_bw(16) + ylab("count")
#2つ山がある→genderでfill分けすると山がわかれている


#箱ひげ図をみてみる
ggplot(body.data, aes(x = gender, y = height, fill = gender)) + geom_boxplot() + theme_bw(16)
#genderによってheightが異なりそうだ

#変数間の関係を見てみる
ggplot(body.data, aes(x = height, y = weight)) + geom_point() + theme_bw(16)
#heightとweightも相関関係がありそうだ

#回帰直線も加えてみる
ggplot(body.data, aes(x = height, y = weight)) + geom_point() + theme_bw(16) + geom_smooth(method = "lm")

#男女混ざったデータでも綺麗な回帰直線に見えるが、色分けしてみる
ggplot(body.data, aes(x = height, y = weight, col = gender)) + geom_point() + theme_bw(16) + geom_smooth(method = "lm")

#相関係数を算出してみる(cor()関数)
#全体
cor(body.data$height, body.data$weight)
#0.89

#男性
body.data.m <- body.data[body.data$gender == "M",]
cor(body.data.m$height, body.data.m$weight)
#0.86

#女性
body.data.f <- body.data[body.data$gender == "F",]
cor(body.data.f$height, body.data.f$weight)
#0.91

#サンプルデータの読み込み（https://gihyo.jp/book/2013/978-4-7741-5896-9/support）
amount1.data <- read.csv("amount1.csv")
head(amount1.data)
#投資と売上のデータ

#ざっくりデータをみてみる
summary(amount1.data)
ggplot(amount1.data, aes(x = invest, y = amount)) + geom_point() + theme_bw(16)

#線形回帰
amount1.lm1 <- lm(amount~invest, data=amount1.data)
summary(amount1.lm1)
#調整済み決定係数は0.80

#残差の分布を確認
plot(amount1.lm1, which = 1)
# 残差は山なりに分布。予測値が両端のときにもっと逓減するように変更するため、investを対数にする。

amount.lm2 <- lm(amount~log(invest), data = amount1.data)
summary(amount.lm2)
##調整済み決定係数は0.80->0.84に向上
#残差の分布を確認
plot(amount.lm2, which = 1)
#残差はより均等にばらけるようになった


#ロジスティック回帰
#タイタニックのデータを使用(データを整形する)
z <- data.frame(Titanic)
Titanic.data <- data.frame(
  Class=rep(z$Class, z$Freq),
  Sex=rep(z$Sex, z$Freq),
  Age=rep(z$Age, z$Freq),
  Survived=rep(z$Survived, z$Freq))

head(z)

#モデル構築
titanic.logit <- glm(Survived~., data=Titanic.data, family=binomial)
summary(titanic.logit)
#↑分からんコードあるけどとりあえず考えない

#とりあえず脳死でオッズ比を見てみよう
#パッケージの読み込み
install.packages("devtools")
library(devtools)
install_github("cran/epicalc")
library(epicalc)

#オッズ比の算出
logistic.display(titanic.logit, simplified=T)
#ORがオッズ比。性別が女性だと11.24倍生存確率が上がる、と示している

#決定木を使って生存可否の分類を可視化してみる
install.packages("partykit")
library(rpart)
library(partykit)

#決定木モデルの構築
titanic.rp <- rpart(Survived~., data=Titanic.data)
#描画
plot(as.party(titanic.rp), tp_args=T)


#主成分分析による次元縮約
#state.x77(1970年代米国50州の8項目データ)を使用
#主成分分析の実行(ここではprcomp()を使う)
state.pca <- prcomp(state.x77[, 1:6], scale = T)
#バイプロットの描画
biplot(state.pca)

#データを見てみる
head(state.x77[, 1:6])


#多次元尺度法（MDS）
#データの読み込み(北海道の都市間の直線距離データ)
hdist <- read.table("HokkaidoCitiesMDS.tsv", header = F)
hcities <- c("札幌","旭川","稚内市","釧路市","帯広市", "室蘭市", "函館","小樽")
names(hdist) <- hcities
rownames(hdist) <- hcities
head(hdist)

#MDSの実行 (cmdscale()を使用)
hdist.cmd <- cmdscale(hdist)

#描画のためのデータ整形
hdist.cmd.df <- as.data.frame(hdist.cmd)
hdist.cmd.df$city <- rownames(hdist.cmd.df)
names(hdist.cmd.df) <- c("x", "y", "city")

#描画
ggplot(hdist.cmd.df, aes(x=-x, y=-y,label=city)) + geom_text() +theme_bw(16)


#k-meansによるクラスタリング
#データはstate.x77(1970年代米国50州の8項目データ)を使用
#クラスタ数は自分で決める。今回は３
state.km <- kmeans(scale(state.x77[,1:6]), 3)

#主成分分析の結果にクラスターの情報を寄与する
state.pca.df <- data.frame(state.pca$x)
state.pca.df$name <- rownames(state.pca.df)
state.pca.df$cluster <- as.factor(state.km$cluster)

#描画
ggplot(state.pca.df, aes(x=PC1, y=PC2, label=name, col=cluster)) + geom_text() + theme_bw(16)

#レーダーチャートでクラスタの特徴をみてみる
install.packages("fmsb")
library(fmsb)

#レーダーチャート用にデータを整形
df <- as.data.frame(scale(state.km$centers))
dfmax <- apply(df, 2, max) + 1
dfmin <- apply(df, 2, min) - 1
df <- rbind(dfmax, dfmin, df)

#レーダーチャートの描画
radarchart(df, seg=5, plty=1, pcol=rainbow(3))
legend("topright", legend=1:3, col=rainbow(3), lty=1)


#caretパッケージによる機械学習　SVMとランダムフォレスト
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

install.packages("kernlab")
library(kernlab)
install.packages("randomForest")

data(spam)
head(spam)
table(spam[,58])

#学習用のデータとテスト用データに分ける
train.index <- createDataPartition(spam$type, p=0.5, list=F)
spam.train <- spam[train.index,]
spam.test <- spam[-train.index,]

#training方法のカスタマイズ：　LGOCVでtrainingを使い5回繰り返す（LGOCVって何）
fitControl <- trainControl(method="LGOTV", p=0.75, number=5)

#SVM(Support Vector Machine)
#spam.svm <- train(spam.train[, -58], spam.train$type, method="svmRadial", preProcess=c("center", "scale"), trControl = fitControl)

#ランダムフォレスト
#spam.rf <- train(spam.train[, -58], spam.train$type,method="rf",preProcess=c("center", "scale"),trControl = fitControl)

#SVMとランダムフォレストで同じエラーがでる。。
#Error: Not a recognized resampling method.
#なので、あと少しだけど諦めよう。

