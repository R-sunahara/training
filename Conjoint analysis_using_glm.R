#多変量テスト
#統計学では実験計画法、マーケティングリサーチではコンジョイント分析と呼ばれている
#多変量テストとは、直交表を用いることにより少ない組み合わせパターンで各要素の効果を算出する方法

#直交表の作成
install.packages("conjoint")
library(conjoint)
#構成要素 2^4=16通りできる
experiment <- expand.grid(
  imgA = c("ImageA1", "ImageA2"),
  imgB = c("ImageB1", "ImageB2"),
  txtA = c("TextA1", "TextA2"),
  txtB = c("TextB1", "TextB2")
)

?expand.grid
#Create a Data Frame from All Combinations of Factor Variables

#直行表の作成 16->8通りに減少した
design.ort <- caFactorialDesign(data = experiment, type = "orthogonal")

?caFactorialDesign
#Function caFactorialDesign creates full or fractional factorial design. Function can return orthogonal factorial design.
#orthogonal ：　直交


#この後は、この組み合わせパターンで出し分けテストする。
#出し分けテストの結果"web_test_sample.csv"の読み込み

web.test.data <- read.csv("web_test_sample.csv")
head(web.test.data)
dim(web.test.data)

#ロジスティック回帰で各変数の効果を推測する
#ロジスティック回帰モデルの構築（step()で変数選択も同時に行う）
fit <- step(glm(cv~., data =web.test.data[,-1], family = binomial))

#モデル概要
summary(fit)

#logistic.display()でオッズ比(ある辞表の起こりやすさを評価する指標の比)を確認
install.packages("epiDisplay")
library(epiDisplay)

logistic.display(fit, simplified = F)

?step
#Select a formula-based model by AIC.
#AIC（赤池情報量基準）は統計モデルの良さの評価指標

?logistic.display
