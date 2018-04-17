

library(readr)
library(tsne)

#dataset<-read_csv("C:/Users/m00760171/Desktop/Templates/trunk/data/normal/data_OFF_OFF_OFF.csv")
dataset<-read_csv("C:/Users/m00760171/Desktop/Templates/trunk/data/smote/data_OFF_OFF_OFF_SMOTE.csv")
#dataset<-read_csv("C:/Users/m00760171/Desktop/Templates/trunk/data/smote/data_CORR_CORR_CORR_SMOTE.csv")
#dataset<-read_csv("C:/Users/m00760171/Desktop/Templates/trunk/data/smote/data_PCA_PCA_PCA_SMOTE.csv")
#dataset<-read_csv("C:/Users/m00760171/Desktop/Templates/trunk/data/under/data_OFF_OFF_OFF_UNDER.csv")
#dataset<-read_csv("C:/Users/m00760171/Desktop/Templates/trunk/data/under/data_CORR_CORR_CORR_UNDER.csv")


Npoints<-700

colors = rainbow(length(unique(dataset$churn)))
names(colors) = unique(dataset$churn)

colors = c("blue","red")
count <-0
ecb = function(x,y)
{
  plot(x,pch=".", col=colors[1+dataset$churn])
  #print(count)
  #count<<-count+100

  }


#tsne_dataset = tsne(dataset[sample(1:nrow(dataset)-10,500),1:ncol(dataset)-1], epoch_callback = ecb, perplexity=50)
inds = sample(1:nrow(dataset),Npoints)
tsne_dataset = tsne(dataset[inds,1:ncol(dataset)-1], epoch_callback = ecb, perplexity=50)

#plot(tsne_dataset,pch="*", col=colors[1+dataset$churn])

df <-as.data.frame(tsne_dataset)
df_dataset <- cbind(df,dataset[inds,])

df <-cbind(df,dataset[inds,"churn"])


write_csv(df_dataset,"tsne_dataset_orig.csv")
write_csv(df,"tsne.csv")
write_csv(dataset,"tsne_orig.csv")




