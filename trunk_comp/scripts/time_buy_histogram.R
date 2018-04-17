library(ggplot2)

df_all<-read_csv(file="C:/Users/m00760171/Desktop/Templates/trunk/FX/H1_PF_2_TR_15_2001-2016/H1_all_targets.csv")
df<-as.data.frame(df_all$buy_time)
names(df)<-"buy_time"
#df<-as.data.frame(TP_time[1:2000])
df_prof=as.data.frame(df[df$buy_time>0 &  df$buy_time<2000,])
names(df_prof)<-"TP_time"
ggplot(data=df_prof, aes(df_prof$TP_time)) + geom_histogram(bins = 100)
summary(df_prof)
#qplot(df_prof$TP_time, geom="histogram") 

df_loss=as.data.frame(df[df$buy_time>-2000 &  df$buy_time<0,])
names(df_loss)<-"TP_time"
ggplot(data=df_loss, aes(df_loss$TP_time)) + geom_histogram(bins = 50)
summary(df_loss)

#qplot(df_loss$TP_time, geom="histogram") 

#ggplot(data=df, aes(df$TP_time)) + geom_histogram(bins = 1)
