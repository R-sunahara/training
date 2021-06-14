# WEBサイトA,BのどちらがCV率が高いのか、A/Bテストで調査する
#今回はデータを自作し、真のCV率が分かっている状態で調べる
install.packages("plyr")
library(plyr)

#真のCV率を設定
A1.CVR <- 0.09
B1.CVR <- 0.09

#サンプル数
n <- 10000
set.seed(2)

#シミュレーション
AB1 <- data.frame(Pattern = c(rep("A", n), rep("B", n)),
                  CV = c(rbinom(n, 1, A1.CVR), rbinom(n, 1, B1.CVR))
                  ) 

#CV率の算出
ddply(AB1, .(Pattern), summarize, CVR = mean(CV))
#-> 0.3%CV率に差があるという結果。

#関数の確認
#rep()は複製、c()はcombine.この場合、縦にマージしている.rbinom(n, size, prob)は二項分布
data.frame(Pattern = c(rep("A", 5), rep("B", 5)),
           CV = c(rbinom(5, 1, A1.CVR), rbinom(5, 1, B1.CVR))) 

?ddply
#For each subset of a data frame, apply function then combine results into a data frame. To apply a function for each row, use adply with .margins set to 1.       
#ddply(.data, .variables, .fun = NULL,) 
#この場合、Patternでデータフレームを分割して、CVRという列にCVの平均を。summarizeはよく分からない


#続いて、真のCV率がBの方が2%高いとした場合

#真のCV率を設定
A2.CVR <- 0.095
B2.CVR <- 0.097

#サンプル数
n <- 10000
set.seed(2)

#シミュレーション
AB2 <- data.frame(Pattern = c(rep("A", n), rep("B", n)),
                  CV = c(rbinom(n, 1, A2.CVR), rbinom(n, 1, B2.CVR))
) 

#CV率の算出
ddply(AB2, .(Pattern), summarize, CVR = mean(CV))
#-> Aの方が0.3%CV率が高いという結果。（誤った判断をしてしまう）




#A/Bテストの結果はカイ二乗分布を用いて統計的に行う

#カイ2乗検定の実行
chisq.test(table(AB1))
chisq.test(table(AB2))

#この場合に重要なのはp-value。A/B間のCV率に差がなかった場合に、得られたデータくらいの差が生まれる確率。
#この場合、両方の結果とも48%,49%くらいの確率なので、信用度は低い。通常、5%以下であれば統計的に優位とみなしている
#サンプル数を10000から500000に変更してみる。
n <- 500000


#シミュレーション
AB1 <- data.frame(Pattern = c(rep("A", n), rep("B", n)),
                  CV = c(rbinom(n, 1, A1.CVR), rbinom(n, 1, B1.CVR))
) 
AB2 <- data.frame(Pattern = c(rep("A", n), rep("B", n)),
                  CV = c(rbinom(n, 1, A2.CVR), rbinom(n, 1, B2.CVR))
) 
#CV率の算出
ddply(AB1, .(Pattern), summarize, CVR = mean(CV))
ddply(AB2, .(Pattern), summarize, CVR = mean(CV))

#カイ2乗検定の実行
chisq.test(table(AB1))
chisq.test(table(AB2))

#このように、実際にCV率に差がないAB1は統計的な優位性は示しにくい。
#一方で、CV率に2%の差があるAB2は、p-value=0.04%となっている


#Chisq.test():Chi-squared Test for Count Data