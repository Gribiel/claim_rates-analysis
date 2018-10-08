pdf("无内胎理赔率.pdf", family="GB1")
#安装包
if (!require(RMySQL)) { install.packages("RMySQL") }
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(plyr)) { install.packages("plyr") }
if (!require(zoo)) { install.packages("zoo") }
if (!require(lubridate)) { install.packages("lubridate") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(ggplot2)) { install.packages("ggplot2") }
#加载包
library(RMySQL)
library(tidyr)
library(plyr)
library(zoo)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
#连接数据库
conn <- dbConnect(MySQL(), 
                  dbname = "claim_tires_analysis",
                  username = "root",
                  password = "root",
                  host = "127.0.0.1",
                  port = 3306)
dbSendQuery(conn,'SET NAMES gbk')
#读取数据
claim_data <- dbReadTable(conn, "claim_data")
claim_data <- claim_data[,c('id', '年', '月', '轮胎品牌', '轮胎规格', '轮胎花纹',
                            '轮胎层级', '生产月', 'DOT年号', '生产厂')]
claim_data <- unite(claim_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
claim_data <- unite(claim_data, 理赔日期, 年, 月, sep = "", remove = TRUE)
claim_data$生产日期 <- paste("20", claim_data$生产日期, sep = "")
claim_data <- unite(claim_data, 
                    轮胎规格,
                    轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                    sep = " ",remove = TRUE)
claim_data <- claim_data[claim_data$生产厂 == '合肥',] 
claim_data$轮胎规格 <- paste(claim_data$轮胎规格, "PR", sep = "")
claim_data$理赔日期 <- as.yearmon(as.character(claim_data$理赔日期), "%Y%m")
claim_data$生产日期 <- as.yearmon(as.character(claim_data$生产日期), "%Y%m")
claim_data$理赔日期 <- as.Date(claim_data$理赔日期)
claim_data$生产日期 <- as.Date(claim_data$生产日期)
int <- interval(ymd(claim_data$生产日期), ymd(claim_data$理赔日期))
claim_data$经过月 <- time_length(int, "month")

sales_data <- dbReadTable(conn, "sales_data")
sales_data <- sales_data[,c('id', '销售年', '销售月', '轮胎品牌', '轮胎规格', 
                            '轮胎花纹', '轮胎层级', '生产厂', '生产月', 'DOT年号')]
sales_data <- unite(sales_data, 销售日期, 销售年, 销售月, sep = "",remove = TRUE)
sales_data <- unite(sales_data, 生产日期, DOT年号, 生产月, sep = "", remove = TRUE)
sales_data$生产日期 <- paste("20", sales_data$生产日期, sep = "")
sales_data <- unite(sales_data, 
                    轮胎规格, 
                    轮胎品牌, 轮胎规格, 轮胎花纹, 轮胎层级,
                    sep = " ",remove = TRUE)
sales_data$轮胎规格 <- paste(sales_data$轮胎规格, "PR", sep = "")
sales_data$销售日期 <- as.yearmon(as.character(sales_data$销售日期), "%Y%m")
sales_data$生产日期 <- as.yearmon(as.character(sales_data$生产日期), "%Y%m")
sales_data$销售日期 <- as.Date(sales_data$销售日期)
sales_data$生产日期 <- as.Date(sales_data$生产日期)
int <- interval(ymd(sales_data$生产日期), ymd(sales_data$销售日期))
sales_data$经过月 <- time_length(int, "month")

m <- c("155|165|175|185|195|205|215|225|235|245|255|265|275|285|295|31|385|445|
       425|17.5|19.5|22.5|24.5")
x <- grep(pattern = m, x = claim_data$轮胎规格, value = TRUE)
y <- grep(pattern = m, x = sales_data$轮胎规格, value = TRUE)
myvars1 <- claim_data$轮胎规格 %in% x
myvars2 <- sales_data$轮胎规格 %in% y
dat1 <- claim_data[myvars1, ]
dat2 <- sales_data[myvars2, ]

dat1_1 <- dat1[, c("生产日期","经过月")]
dat1_2 <- plyr::count(dat1_1, names(dat1_1))
names(dat1_2)[3] <- "理赔数量"
dat1_3 <- dat1_2[order(dat1_2$生产日期, dat1_2$经过月),]

dat2_1 <- dat2[, c("生产日期","经过月")]
dat2_2 <- plyr::count(dat2_1, names(dat2_1))
names(dat2_2)[3] <- "销售数量"
dat2_3 <- dat2_2[order(dat2_2$生产日期, dat2_2$经过月),]

merge_data <- merge(dat1_3, dat2_3 ,by = c("经过月", "生产日期"), all = TRUE)
merge_data$理赔数量[is.na(merge_data$理赔数量)] <- 0
merge_data$销售数量[is.na(merge_data$销售数量)] <- 0
new_merge_data <- merge_data[merge_data$经过月 >= 0,]
new_merge_data <- new_merge_data[order(new_merge_data$生产日期,new_merge_data$经过月),]
t = data.table(new_merge_data)
new_merge_data1 <- t[,cumsum(销售数量), by = 生产日期]
new_merge_data2 <- t[,cumsum(理赔数量), by = 生产日期]
names(new_merge_data1)[2] <- "销售数量"
names(new_merge_data2)[2] <- "理赔数量"
newdata <- cbind(new_merge_data1, new_merge_data2, new_merge_data$经过月)
names(newdata)[5] <- "经过月"
newdata <- newdata[,c("生产日期", "经过月", "销售数量", "理赔数量")]

newdata <- transform(newdata, 理赔率 = 理赔数量 / 销售数量 * 100)
newdata$理赔率 <- round(newdata$理赔率, 2)
newdata$生产日期 <- format(newdata$生产日期, format = "%Y%m")
newdata$生产日期 <- as.character(newdata$生产日期)
newdata <- newdata[newdata$理赔率 <= 10,]
ggplot(newdata, aes(经过月, 理赔率, group = 生产日期, color = 生产日期)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.8) +
  geom_text(aes(label = 理赔率, 
                vjust = -0.2, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,max(newdata$理赔率) + 0.1,0.1), 
                     limits = c(0,max(newdata$理赔率) + 0.1)) +
  scale_x_continuous(breaks = seq(0,max(newdata$经过月),1),
                     limits = c(0,max(newdata$经过月))) +
  labs(title = "截止2017年9月无内胎理赔率（合肥）",
       x = "经过月", y = "理赔率/%") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off() 