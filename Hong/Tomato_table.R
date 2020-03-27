
#本文需要用到的 R 包
library(vegan)	#用于计算 Shannon 熵指数、Simpson 指数、Chao1 指数、ACE 指数等，同时用于抽样
library(picante)	#用于计算 PD_whole_tree，若不计算它就无需加载。事实上，picante 包加载时默认同时加载 vegan
library(ggplot2)	#用于 ggplot2 作图
library(doBy)	#用于分组统计（第 97 行使用到）
library(ggalt)	#用于绘制拟合曲线（第 138 行使用到）
library(BiodiversityR)	#用于绘制 Rank-abundance 曲线（第 160 行使用到）

getwd()
setwd("/Users/chengt/Documents/R/Numerical_Ecology/Hong")
otu <- read.delim('Tomato.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)

library(vegan)
library(picante)
#注：picante 包加载时默认同时加载 vegan，因此可省略“library(vegan)”这一步。


##物种丰富度 Richness 指数
richness <- rowSums(otu > 0)
#或
richness <- estimateR(otu)[1, ]

##Shannon（以下 Shannon 公式的对数底数均设使用 e，在 R 中即表示为 exp(1)）
#Shannon 指数
shannon_index <- diversity(otu, index = 'shannon', base = exp(1))

#Shannon 多样性
shannon_diversity <- exp(1)^shannon_index

#Shannon 均匀度（Pielou 均匀度）
pielou <- shannon_index / log(richness, exp(1))

##Simpson
#Gini-Simpson 指数（我们平时常用的 Simpson 指数即为 Gini-Simpson 指数）
gini_simpson_index <- diversity(otu, index = 'simpson')

#经典 Simpson 指数（使用频率比较低）
simpson_index <- 1 - gini_simpson_index

#Invsimpson 指数（Gini-Simpson 的倒数）
invsimpson_index <- 1 / gini_simpson_index
#或
invsimpson_index <- diversity(otu, index = 'invsimpson')

#Simpson 多样性
simpson_diversity <- 1 / (1 - gini_simpson_index)

#Simpson 均匀度（equitability 均匀度）
equitability <- 1 / (richness * (1 - gini_simpson_index))

##Chao1 & ACE
#Chao1 指数
chao1 <- estimateR(otu)[2, ]

#ACE 指数
ace <- estimateR(otu)[4, ]

##goods_coverage 指数
goods_coverage <- 1 - rowSums(otu == 1) / rowSums(otu)

##谱系多样性（与上述相比，还需指定进化树文件）
# tree <- read.tree('otu_tree.tre')
# pd_whole_tree <- pd(otu, tree, include.root = FALSE)    #测试时发现有根树和无根树的 PD_whole_tree 计算结果是一样的，但是无根树的计算会更快


#定义函数
library(picante)      #picante 包加载时默认同时加载 vegan

alpha <- function(x, tree = NULL, base = exp(1)) {
    est <- estimateR(x)
    Richness <- est[1, ]
    Chao1 <- est[2, ]
    ACE <- est[4, ]
    Shannon <- diversity(x, index = 'shannon', base = base)
    Simpson <- diversity(x, index = 'simpson')    #Gini-Simpson 指数
    Pielou <- Shannon / log(Richness, base)
    goods_coverage <- 1 - rowSums(x == 1) / rowSums(x)
    
    result <- data.frame(Richness, Shannon, Simpson, Pielou, Chao1, ACE, goods_coverage)
    if (!is.null(tree)) {
        PD_whole_tree <- pd(x, tree, include.root = FALSE)[1]
        names(PD_whole_tree) <- 'PD_whole_tree'
        result <- cbind(result, PD_whole_tree)
    }
    result
}

#加载 OTU 丰度表和进化树文件
otu <- read.delim('Tomato.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu <- t(otu)
# tree <- read.tree('otu_tree.tre')

#不包含谱系多样性，无需指定进化树；Shannon 公式的 log 底数我们使用 2
alpha_all <- alpha(otu, base = 2)
#包含谱系多样性时，指定进化树文件；Shannon 公式的 log 底数我们使用 2
alpha_all <- alpha(otu, tree, base = 2)

#输出保存在本地
write.csv(alpha_all, 'otu_alpha.csv', quote = FALSE)
