1.参数输入
library(HMM)
## 假设有3个可能状态集合（实际的状态个数可以通过赤池信息准则或者贝叶斯信息准则计算）
state <- c("1","2","3")
## 所有可能观测集合
Symbols <- c("丰收","平收","歉收")
## 假设初始概率分布为
startprob <- c(0.375,0.35,0.275)
## 状态转移概率矩阵
transpro <- matrix(c(0.2, 0.5385, 0.3636, 0.4667, 0.1538, 0.4545,0.3333,0.3077,0.1818),nrow = 3)
transpro
## 观测概率分布：每种状态中"丰收","平收","歉收"的概率
emisspr <- matrix(c(0.25,0.4,0.3,0.25,0.3,0.6,0.5,0.3,0.1),nrow = 3)
emisspr
## 初始化隐马尔科夫模型
hmm <- initHMM(States = state,Symbols = Symbols,startProbs = startprob,transProbs = transpro,
               emissionProbs = emisspr)

2.概率计算问题
## 概率计算问题
## 输入2009~2019年的观测序列
ober <- c("丰收","平收","歉收","丰收","平收","歉收","丰收","丰收","丰收","丰收")
## 计算向前概率矩阵。第一个维度指的是状态和第二维度的时间。
exp(forward(hmm, ober))
## 状态求和，得到2009~2019年每年为该序列的概率
colSums(exp(forward(hmm, ober)))

3.学习问题
## 学习问题
## 已知观测序列，学习模型的参数 
hmm <- initHMM(States = state,Symbols = Symbols,startProbs = startprob,
               transProbs = transpro,emissionProbs = emisspr)
hmm
##(假设我们要模拟中国过去1000年以来的农业收成情况)
##北方地区有500年是丰收年，350年是平收年，150年是歉收年
##南方地区有600年是丰收年（降水更丰富），200年是平收年，250年是歉收年（水旱灾害更频繁）
ober <- c(sample(c(rep("丰收",500),rep("平收",350),rep("歉收",150))),
          sample(c(rep("丰收",600),rep("平收",200),rep("歉收",250))))
##Baum-Welch算法计算HMM参数
bw <- baumWelch(hmm,observation = ober)
bw$hmm
## $States：状态集合；Symbols：观测集合；startProbs：初始概率分布
##emissionProbs：发射概率分布

4.预测问题
##预测问题
##给定观测序列，求最有可能的对应的状态序列
## 输入2009~2019年的观测序列
ober <- c("丰收","平收","歉收","丰收","平收","歉收","丰收","丰收","丰收","丰收")
## 维特比算法求解
## 计算在一个给定的序列的观测和给定的Hidden Markov模型在时间k在状态x的后验概率
posterior(hmm,ober)
## 输出：一个字符串向量，包含最可能的状态路径
viterbi(hmm,ober)
##以上的农业收成问题还可以运用到气候变化，空气污染和历史疫灾分析

