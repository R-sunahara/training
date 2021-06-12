#PCA -> Kmeans -> レーダチャートで解釈 -> MDSによる知覚マップ作成 -> 選好ベクトルの推定

#ggplot2パッケージのインストール
install.packages("ggplot2")
library(ggplot2)
#データサイエンティスト養成読本p76
#Rによるマーケティング分析
#2013年頃の古き良き基礎的なマーケティング分析を学ぼう

#データの読み込み
#IMJモバイル「スマートフォンユーザ動向定点観測2011」から作成した仮想データ。スマホ保有者へのアンケート回答データ
sp.user.data <-read.csv("sp_user_research_data.csv")


#アンケート回答データから、消費行動に関するセグメンテーションを行う
#ユーザ間の類似関係（クラスタ関係）をざっくり把握するために主成分分析を行う

#主成分分析の実行
sp.user.pca <- prcomp(sp.user.data[,-1], scale = T)
#バイプロットの表示
biplot(sp.user.pca)

#今回は仮想データなのでめちゃ綺麗に4つの群に別れているのがわかる。
#k-meansによるクラスタリング
sp.user.km <- kmeans(scale(sp.user.data[,-1]), 4)
#主成分分析の結果にクラスターの情報を付与
sp.user.pca.df <- data.frame(sp.user.pca$x)
sp.user.pca.df$id <- sp.user.data$id
sp.user.pca.df$cluster <- as.factor(sp.user.km$cluster)
#描画
ggplot(sp.user.pca.df, aes(x=PC1, y=PC2, label=id, col=cluster)) + geom_text() + theme_bw(16)


#レーダーチャートでクラスタの特徴をみてみる
install.packages("fmsb")
library(fmsb)
#レーダーチャート用にデータを整形
df <- as.data.frame(scale(sp.user.km$centers))
dfmax <- apply(df, 2, max) + 1
dfmin <- apply(df, 2, min) - 1
df <- rbind(dfmax, dfmin, df)
#レーダーチャートの描画
radarchart(df, seg=5, plty=1, pcol=rainbow(4))
legend("topright", legend=1:4, col=rainbow(4), lty=1)

#このように、アンケート結果からそれぞれのクラスタの特徴を見ることができる。
#ここは人間の力が介入する部分。各クラスタの特徴を見極め、名づける。
#セグメンテーションの後は、ポジショニング。自分達のターゲットをどのクラスタにするか決める
#決まったら、ターゲットである彼らが認識している、自社サービス、競合サービスの位置づけを整理する（知覚マップを作成）
#アンケートの回答データとは別にデータを収集する必要がある
#今回は、スマホサービス（アプリ）領域のデータ

#MDSによる知覚マップの作成
#データの読み込み
target.data <- read.csv("target_preference_data.csv", header = T)

#非計量MDSの実行 (isoMDS()を使用)
library(MASS)
service.dist <- dist(t(target.data[,-1]))
service.map <- isoMDS(service.dist)
                     
                     
#描画のためのデータ整形
service.map.df <- data.frame(scale(service.map$points))
service.map.df$service_name <- names(target.data[,-1])

#描画
ggplot(service.map.df, aes(x=X1, y=X2,label=service_name)) + geom_text() +theme_bw(16)

#この知覚マップからは、どのアプリがどれくらいの知覚の近さがあるのか、といったことがわかる
#さらに、ターゲット層の選好ベクトルを描画し、何がどれだけ好まれているのか、という情報も付加する

#選好ベクトルの推定
user.preference.data <- 
  do.call(rbind,
          lapply(1:nrow(target.data),
                 function(i){
                   preference.data <- data.frame(
                     p=as.numeric(target.data[i,-1]),
                     X1=service.map.df$X1,
                     X2=service.map.df$X2
                   )
                   fit <- lm(p~., data=preference.data)
                   b <- 2 / sqrt(fit$coef["X1"]^2+fit$coef["X2"]^2)
                   data.frame(X1=b*fit$coef["X1"],
                              X2=b*fit$coef["X2"],
                              service_name=i)
                 }))

#選好ベクトルの描画
ggplot(service.map.df, aes(x=X1, y=X2,label=service_name)) + 
  geom_text() +
  theme_bw(16) +
  xlim(-2,2) +
  ylim(-2,2) +
  geom_point(data=user.preference.data, aes(x=X1,y=X2))

#X2の方向へ行くほど、ターゲットユーザーが好んでいることを意味している。これを人数分実行すると、市場規模がわかってくるらしい
#ターゲットユーザーはSNSとかよりもphoto,mapなどのアプリを使用している。サービス連携や広告戦略に活かせる示唆が得られた。


