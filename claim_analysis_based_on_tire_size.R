if (!require(RMySQL)) { install.packages("RMySQL") }
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(plyr)) { install.packages("plyr") }
if (!require(ggplot2)) { install.packages("ggplot2") }
if (!require(ggthemes)) { install.packages("ggthemes") }
library(RMySQL)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(plyr)
conn <- dbConnect(MySQL(), 
                  dbname = "claim_tires_analysis",
                  username = "root",
                  password = "root",
                  host = "127.0.0.1", 
                  port = 3306)
dbSendQuery(conn,'SET NAMES gbk')
claim_data <- dbReadTable(conn, "claim_data")
claim_data <- unite(claim_data, 
                      轮胎规格,
                      轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                       sep = " ",remove = TRUE)
claim_data <- claim_data[claim_data$生产厂 == '合肥',]
sales_data <- dbReadTable(conn, "sales_data")
sales_data <- unite(sales_data, 
                       轮胎规格, 
                       轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                       sep = " ",remove = TRUE)
sum_claim <- as.data.frame(table(unlist(claim_data$轮胎规格)))
sum_sales <- as.data.frame(table(unlist(sales_data$轮胎规格)))
sum_claim <- rename(sum_claim, c(Var1 = "轮胎规格", Freq = "数量"))
sum_sales <- rename(sum_sales, c(Var1 = "轮胎规格", Freq = "数量"))
sum_claim$轮胎规格 <- paste(sum_claim$轮胎规格, "PR", sep = "")
sum_sales$轮胎规格 <- paste(sum_sales$轮胎规格, "PR", sep = "")
#sum_claim$总数量 <- tapply(sum_claim$数量, sum_claim$轮胎规格, sum)
#sum_sales$总数量 <- tapply(sum_sales$数量, sum_sales$轮胎规格, sum)
data <- merge(x = sum_claim, y = sum_sales, by = "轮胎规格", 
                 all.x = TRUE)
data <- na.omit(data )
data <- rename(data, c(数量.x = "理赔数量", 数量.y = "销售数量"))
#data <- data[, c(1,3,5)]
data <- transform(data, 理赔率 = 理赔数量 / 销售数量*100)
data$理赔率 <- round(data$理赔率, 2)
data <- data[order(data$理赔率), ]
data <- data[data$销售数量 > 5000, ]
data$id <- 1:length(data[,4])
par(mar = c(5, 14, 3, 1))
par(las = 2)
barplot(data$理赔率, names.arg = data$轮胎规格,
        main = "不同规格轮胎理赔率",
        horiz = TRUE,
        cex.names = 0.8,
        col = rainbow(length(data$轮胎规格)),
        xlab = "理赔率/%", 
        xlim = c(0, max(data$理赔率) + 0.1), xaxt = "n")
axis(1, seq(0,max(data$理赔率) + 0.1,by = 0.03))
abline(v = data$理赔率, lty = 2, col = "black")