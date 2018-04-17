


library(readr)

dt_int_buy<-read_csv(file="FX/H1_PF_1,5_TR_10_2001-2016/H1_buy.csv")
dt_int_sell<-read_csv(file="FX/H1_PF_1,5_TR_10_2001-2016/H1_sell.csv")


nms<-names(dt_int_buy)
names(dt_int_buy)<-c(setdiff(nms,"DPO"),"Time")

nms<-names(dt_int_sell)
names(dt_int_sell)<-c(setdiff(nms,"DPO"),"Time")

write.csv(dt_int_buy,file="FX/H1_PF_1,5_TR_10_2001-2016/H1_buy.csv",row.names = FALSE)
write.csv(dt_int_sell,file="FX/H1_PF_1,5_TR_10_2001-2016/H1_sell.csv",row.names = FALSE)
