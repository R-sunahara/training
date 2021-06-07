#データの読み込み
data <- read.csv("2019.csv")
#データを見てみる
summary(data)

# パッケージ読み込み
library(tidyverse)
install.packages("plotly")

#各データのヒストグラムを見てみる
colnames(data)
hist(x = data$Overall.rank)　#幸福度の順位
hist(x = data$Score)　#幸福度の数値
hist(x = data$GDP.per.capita) #一人当たりのGDP
hist(x = data$Social.support)　#ソーシャルサポート（困ったときに頼れる人がいるか）
hist(x = data$Healthy.life.expectancy)　#健康寿命
hist(x = data$Freedom.to.make.life.choices)　#生活の自由度（生活・人生の選択の自由に満足しているか）
hist(x = data$Generosity)　#寛容さ（過去1か月に行った寄付額のGDP比率）
hist(x = data$Perceptions.of.corruption)　#社会の不正への疑い（政府・企業で不正が広がっていると思うか）


#散布図で見てみる
#Score~GDP.per.capita
ggplot(data, aes(x = GDP.per.capita, y = Score)) +
  geom_point() +
  stat_smooth(method = "lm", se =FALSE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("GDP.per.capita") + 
  ylab("Score") 

#Score~Social.support
ggplot(data, aes(x = Social.support, y = Score)) +
  geom_point() +
  stat_smooth(method = "lm", se =FALSE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("Social.support") + 
  ylab("Score")

#Score~Freedom.to.make.life.choices
ggplot(data, aes(x = Freedom.to.make.life.choices, y = Score)) +
  geom_point() +
  stat_smooth(method = "lm", se =FALSE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("Freedom.to.make.life.choices") + 
  ylab("Score")

#Score~Generosity
ggplot(data, aes(x = Generosity, y = Score)) +
  geom_point() +
  stat_smooth(method = "lm", se =FALSE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("Generosity") + 
  ylab("Score")

#Score~Perceptions.of.corruption
ggplot(data, aes(x = Perceptions.of.corruption, y = Score)) +
  geom_point() +
  stat_smooth(method = "lm", se =FALSE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("Perceptions.of.corruption") + 
  ylab("Score")

#Healthy.life.expectancy~GDP.per.capita
ggplot(data, aes(x = GDP.per.capita, y = Healthy.life.expectancy)) +
  geom_point() +
  stat_smooth(method = "lm", se =FALSE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("GDP.per.capita") + 
  ylab("Healthy.life.expectancy")

#Healthy.life.expectancy~Social.support
ggplot(data, aes(x = Social.support, y = Healthy.life.expectancy)) +
  geom_point() +
  stat_smooth(method = "lm", se =FALSE, colour = "blue") + # lm:最小二乗法、信頼区間は表示させない(FALSE)、色は青(blue)
  xlab("Social.support") + 
  ylab("Healthy.life.expectancy")


'''
被説明変数Y : Score
説明変数X : GDP.per.capita
コントロール変数Z : Healthy.life.expectancy
と設定する
'''
install.packages("stargazer")
library(stargazer)
#textでの表示
stargazer(data, type = "text")

data2 <- data[, c("Score", "GDP.per.capita")]
stargazer(data2, type = "text")

#標準偏差
sd(data2$Score)
sd(data2$GDP.per.capita)

#ヒストグラム
hist(x = data2$Score)
hist(x = data2$GDP.per.capita)

#散布図で見てみる
#Score~GDP.per.capita
ggplot(data, aes(x = GDP.per.capita, y = Score)) +
  geom_point() +
  stat_smooth(method = "lm", se =FALSE, colour = "blue") + 
  xlab("GDP.per.capita") + 
  ylab("Score") 


data.lm1 <- lm(formula = Score ~ GDP.per.capita, data = data)
stargazer(data.lm1, type = "text")
#残差を確認
plot(data.lm1, which=1)

#Xを対数軸に
data$GDP.per.capita.log <- log(data$GDP.per.capita)
data$GDP.per.capita.log[which(data$GDP.per.capita.log == -Inf, TRUE)] <- 0
data.lm2 <- lm(Score ~ GDP.per.capita.log, data = data)
stargazer(data.lm2, type = "text")

#残差を確認
plot(data.lm2, which=1)
#より一層ひどくなった


#Score~GDP.per.capita + Healthy.life.expectancy
#普通に
data.lm3 <- lm(formula = Score ~ GDP.per.capita + Healthy.life.expectancy, data = data)
stargazer(data.lm3, type = "text")

#残差を確認
plot(data.lm3, which=1)

#Xを対数軸に
data$Healthy.life.expectancy.log <- log(data$Healthy.life.expectancy)
data$Healthy.life.expectancy.log[which(data$Healthy.life.expectancy.log == -Inf, TRUE)] <- 0
data.lm4 <- lm(Score ~ GDP.per.capita.log, data = data)
stargazer(data.lm4, type = "text")

#残差を確認
plot(data.lm4, which=1)
#より一層ひどくなった

#ロバスト標準誤差による推定
install.packages("estimatr")

# パッケージの読み込み
library("estimatr")

# ロバスト標準誤差を用いたOLS推定
data.lm_robust1 <- lm_robust(formula = Score ~ GDP.per.capita, 
                           data = data, se_type = "HC1")
data.lm_robust2 <- lm_robust(formula = Score ~ GDP.per.capita + Healthy.life.expectancy, 
                            data = data, se_type = "HC1")


install.packages("texreg")
#読み込み
library(texreg)

#結果表の出力
screenreg(list(data.lm_robust1, data.lm_robust2))

data.lm1 <- lm(formula = Score ~ GDP.per.capita, data = data)
data.lm2 <- lm(formula = Score ~ GDP.per.capita + Healthy.life.expectancy, data = data)
data.lm3 <- lm(formula = Score ~ GDP.per.capita + Healthy.life.expectancy + Social.support, data = data)

screenreg(list(data.lm1, data.lm2, data.lm3))
          
          