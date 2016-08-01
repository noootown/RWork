library(dplyr)
library(ggplot2)
data <- read.table(file="./Downloads/airbox-20160501-20160531.txt", header=FALSE, sep="\t");
colnames(data) <- c("Time","Id","PM25","Temp","Humidity","Longtitude","Latitude");
data$f_time <-  as.POSIXct(strptime(data$Time, "%Y-%m-%dT%H:%M:%SZ",tz="Etc/GMT+8"))

data2 <- tbl_df(data = data)
#count number of data by Id
counts_data <- data2 %>% group_by(Id) %>% summarise(data_count = n())
#count number of data by Id & Date 
counts_date <- data2 %>% group_by(Id,as.Date(Time)) %>% summarise(count = n())
colnames(counts_date)[2] <- "Date"
#count Date of data by Id
counts_date_by_Id <- counts_date %>% group_by(Id) %>% summarise(date_count=n())

counts_data_high <- counts_data %>% arrange(desc(data_count)) %>% head(n=40)
counts_data_high$Id <- factor(counts_data_high$Id,levels = unique(counts_data_high$Id))
ggplot(counts_data_high, aes(x=Id,y=data_count)) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + ggtitle("資料筆數 vs 測站 (高到低)")

counts_data_low <- counts_data %>% arrange(data_count) %>% head(n=40)
counts_data_low$Id <- factor(counts_data_low$Id,levels = unique(counts_data_low$Id))
ggplot(counts_data_low, aes(x=Id,y=data_count)) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + ggtitle("資料筆數 vs 測站 (低到高)")

counts_date_by_Id_high <- counts_date_by_Id %>% arrange(desc(date_count))
counts_date_by_Id_high$Id <- factor(counts_date_by_Id_high$Id,levels = unique(counts_date_by_Id_high$Id))
ggplot(counts_date_by_Id_high, aes(x=Id,y=date_count)) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + ggtitle("收集天數 vs 測站 (高到低)")

counts_date_by_Id_low <- counts_date_by_Id %>% arrange(date_count) %>% head(n=40)
counts_date_by_Id_low$Id <- factor(counts_date_by_Id_low$Id,levels = unique(counts_date_by_Id_low$Id))
ggplot(counts_date_by_Id_low, aes(x=Id,y=date_count)) + geom_point() + theme(axis.text.x = element_text(angle = 90)) + ggtitle("收集天數 vs 測站 (低到高)")

counts_date_low <- data2 %>% filter(Id %in% counts_date_by_Id_low$Id) %>% mutate(Date=as.Date(Time)) %>% select(Id,Date) %>% inner_join(counts_date_by_Id,by="Id") %>% arrange(date_count) %>% distinct() %>% mutate(Id=factor(.$Id,levels = unique(.$Id)))
ggplot(counts_date_low,aes(Id,Date)) + geom_point() + theme(axis.text.x = element_text(angle = 90))

counts_date_high <- data2 %>% mutate(Date=as.Date(Time)) %>% select(Id,Date) %>% inner_join(counts_date_by_Id,by="Id") %>% arrange(desc(date_count)) %>% distinct() %>% mutate(Id=factor(.$Id,levels = unique(.$Id)))
ggplot(counts_date_high,aes(Id,Date)) + geom_point() + theme(axis.text.x = element_text(angle = 90))

