if (!require(RMySQL)) { install.packages("RMySQL") }
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(plyr)) { install.packages("plyr") }
if (!require(zoo)) { install.packages("zoo") }
library(RMySQL)
library(tidyr)
library(plyr)
library(zoo)
conn <- dbConnect(MySQL(), 
                  dbname = "claim_tires_analysis",
                  username = "root",
                  password = "root",
                  host = "127.0.0.1", 
                  port = 3306)
dbSendQuery(conn,'SET NAMES gbk')
claim_data <- dbReadTable(conn, "claim_data")
claim_data <- claim_data[,c(1,2,3,23)]
claim_data <- unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
claim_data <- claim_data[claim_data$生产厂 == '合肥',]
claim_data$理赔日期 <- as.yearmon(as.character(claim_data$理赔日期), "%Y%m")


sales_data <- dbReadTable(conn, "sales_data")
sales_data <- sales_data[,c(1,2,3,11)]
sales_data <- unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
sales_data$销售日期 <- as.yearmon(as.character(sales_data$销售日期), "%Y%m")

sum_claim <- as.data.frame(table(unlist(claim_data$理赔日期)))
sum_sales <- as.data.frame(table(unlist(sales_data$销售日期)))
sum_claim <- rename(sum_claim, c(Var1 = "理赔日期", Freq = "数量"))
sum_sales <- rename(sum_sales, c(Var1 = "销售日期", Freq = "数量"))

bb = 0
for (j in seq(sum_sales$数量)) {
      bb = bb + sum_sales$数量[j]
       j = j + 1 }
a <- cumsum(sum_sales$数量)
newsales_data <- data.frame(sum_sales$销售日期, a)
newsales_data <- rename(newsales_data, c(sum_sales.销售日期 = "日期", 
                                         a = "销售数量"))

bb = 0
for (j in seq(sum_claim$数量)) {
  bb = bb + sum_claim$数量[j]
  j = j + 1 }
b <- cumsum(sum_claim$数量)
newclaim_data <- data.frame(sum_claim$理赔日期, b)
newclaim_data <- rename(newclaim_data, c(sum_claim.理赔日期 = "日期", 
                                         b = "理赔数量"))

data <- merge(x = newclaim_data, y = newsales_data, by = "日期", 
              all.x = TRUE)
data <- data[order(data$日期), ]
data <- transform(data, 理赔率 = 理赔数量 / 销售数量*100)
data$理赔率 <- round(data$理赔率, 5)
par(mar = c(5, 5, 3, 1))
par(las = 2)
barplot(data$理赔率, names.arg = data$日期,
        main = "合肥产品不同月份理赔率",
        horiz = TRUE,
        cex.names = 0.8,
        col = rainbow(length(data$日期)),
        xlab = "理赔率/%",
        xlim = c(0, max(data$理赔率) + 0.1), xaxt = "n")
axis(1, seq(0,max(data$理赔率) + 0.1,by = 0.03))
abline(v = data$理赔率, lty = 2, col = "black")