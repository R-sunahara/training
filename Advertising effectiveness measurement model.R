#広告量やほかの要因を説明変数としたKPIの予測モデルの構築と分析

#今回は線形型と逓減型の回帰モデルを作成する

library(ggplot2)
library(scales)

#[GRP(のべ視聴率)と売上データ]の読み込み
grp.data <- read.csv("grp.csv", header = T)

#散布図の描画
ggplot(grp.data, aes(x = grp, y = amount)) +
  geom_point() +
  scale_y_continuous(label = comma, limits = c(0,360000)) +
  xlab("grp") + 
  ylab("amount")


##線形モデル

#線形モデルの可視化は、geom_smotth関数ですぐに可能
ggplot(grp.data, aes(x = grp, y = amount)) +
  geom_point() +
  scale_y_continuous(label = comma, limits = c(0,360000)) +
  geom_smooth(method = "lm") +
  xlab("grp") + 
  ylab("amount")

#モデルの構築
fit <- lm(amount ~ grp, data = grp.data)

#モデル概要の表示
summary(fit)


##逓減モデル

#x,y軸をそれぞれ対数変換すればよい

#モデルの構築
fit <- lm(log(amount) ~ log(grp), data = grp.data)

#モデル概要の表示
summary(fit)

#対数変換したので、係数の読み取り方に注意が必要。係数が0.21ということは、GRPが1%増えると売上は0.21%増加するという意味

#逓減モデルの予測結果を描画
fit.data <- data.frame(grp = grp.data$grp, amount = exp(fit$fitted.values))

ggplot(grp.data, aes(x = grp, y = amount)) +
  geom_point() +
  geom_line(data = fit.data, aes(x = grp, y = amount)) +
  scale_y_continuous(label = comma, limits = c(0,360000)) +
  xlab("grp") + 
  ylab("amount")+
  theme_bw(16)
