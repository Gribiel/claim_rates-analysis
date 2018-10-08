if (!require(RMySQL)) { install.packages("RMySQL") }
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(plyr)) { install.packages("plyr") }
if (!require(zoo)) { install.packages("zoo") }
if (!require(lubridate)) { install.packages("lubridate") }
if (!require(dplyr)) { install.packages("dplyr") }
if (!require(ggplot2)) { install.packages("ggplot2") }
library(RMySQL)
library(tidyr)
library(plyr)
library(zoo)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggthemes)
conn <- dbConnect(MySQL(), 
                  dbname = "claim_tires_analysis",
                  username = "root",
                  password = "root",
                  host = "127.0.0.1",
                  port = 3306)
dbSendQuery(conn,'SET NAMES gbk')
claim_data <- dbReadTable(conn, "claim_data")
claim_data <- claim_data[,c(1,2,3,9,10,11,12,14,16,23)]
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
sales_data <- sales_data[,c(1,2,3,7,8,9,10,11,12,14)]
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

dat1 <- claim_data[claim_data$轮胎规格 == "富力通 12.00R20 S-3015 20PR",]
dat2 <- sales_data[sales_data$轮胎规格 == "富力通 12.00R20 S-3015 20PR",]
dat1 <- dat1[, c(2,3,4,6)]
dat1_1 <- plyr::count(dat1, names(dat1))
names(dat1_1)[5] <- "理赔数量"
dat1_1 <- dat1_1[,c("轮胎规格", "生产日期", "经过月", "理赔数量")]
dat1_1 <- dat1_1[order(dat1_1$生产日期,dat1_1$经过月),]

dat2 <- dat2[, c(2,3,5,6)]
dat2_1 <- plyr::count(dat2, names(dat2))
names(dat2_1)[5] <- "销售数量"
dat2_1 <- dat2_1[,c("轮胎规格", "生产日期", "经过月", "销售数量")]
dat2_1 <- dat2_1[order(dat2_1$生产日期, dat2_1$经过月),]

a <- merge(dat1_1, dat2_1 ,by = c("经过月", "生产日期"), all = TRUE)
a$理赔数量[is.na(a$理赔数量)] <- 0
a$销售数量[is.na(a$销售数量)] <- 0
a$轮胎规格.x[is.na(a$轮胎规格.x)] <- "富力通 12.00R20 S-3015 20PR"
a$轮胎规格.y[is.na(a$轮胎规格.y)] <- "富力通 12.00R20 S-3015 20PR"
data <- a[, c(1,2,3,4,6)]
data <- data[order(data$生产日期,data$经过月),]
t = data.table(data)
data1 <- t[,cumsum(销售数量), by = 生产日期]
data2 <- t[,cumsum(理赔数量), by = 生产日期]
names(data1)[2] <- "销售数量"
names(data2)[2] <- "理赔数量"
newdata <- cbind(data1, data2, data$经过月)
names(newdata)[5] <- "经过月"
newdata <- newdata[,c(1,5,4,2)]

newdata <- transform(newdata, 理赔率 = 理赔数量 / 销售数量*100)
newdata$理赔率 <- round(newdata$理赔率, 2)
newdata$生产日期 <- format(newdata$生产日期, format = "%Y%m")
newdata$生产日期 <- as.character(newdata$生产日期)
ggplot(newdata, aes(经过月, 理赔率, group = 生产日期, color = 生产日期)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = 理赔率, 
                vjust = -0.2, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,max(newdata$理赔率) + 0.1,0.1), 
                     limits = c(0,max(newdata$理赔率) + 0.1)) +
  scale_x_continuous(breaks = seq(0,max(newdata$经过月),1),
                     limits = c(0,max(newdata$经过月))) +
  labs(title = "截止2017年9月富力通 12.00R20 S-3015 20PR理赔率（合肥）",
       x = "经过月", y = "理赔率/%") 
  