library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(plotly)
library(gganimate)
 
read.table(file = "C:/Users/jabba/Desktop/Sandbox/plate1_20230420_Mock_60min_wave2_ van.txt",skipNul = FALSE,skip = 18,fileEncoding="UCS-2LE", sep = "\t")
read.table(file = FILES[1],skipNul = TRUE,skip = 18,nrows = 8,fileEncoding="UCS-2LE", sep = "\t")
df<-read.table(file = FILES[23],skipNul = TRUE,skip = 18,nrows = 8,fileEncoding="UCS-2LE", sep = "\t")
df<-df[,3:14]
colnames(df)<-c(1:12)
as.numeric(which.max(colMeans(df)))   ####position of max column
df[-as.numeric(which.max(colMeans(df)))]<-NA
df$Row<- LETTERS[1:8]
df$File<-FILES[1]
df<-separate(df, col = File, into = c("Plate","Date", "Cell", "Read_time"), sep = "_")
df<-separate(df,col = Plate, into = c("a","b","c","d","e","f","g","Plate"), sep = "/")
df[14:20]<-NULL
df$Read_time<-extract_numeric(df$Read_time)
extract_numeric(df$Read_time)







 

#Read in our Data
FILES<-list.files(path = "C:/Users/Jabbar/OneDrive - rheostat-tx.com/Desktop/Sandbox/PKD Kinetics", full.names = TRUE)


df<-lapply(FILES, function(i){
  df<-read.table(file = i,skipNul = TRUE,skip = 18,nrows = 8,fileEncoding="UCS-2LE", sep = "\t")
  df<-df[,3:14]
  colnames(df)<-c(1:12)
  #as.numeric(which.max(colMeans(df[[34]][1:12]))) position of max column
  df[-as.numeric(which.max(colMeans(df)))]<-NA
  df$Row<- LETTERS[1:8]
  df$File<-i
  df<-separate(df, col = File, into = c("Plate","Date", "Cell", "Read_time"), sep = "_")
  df<-separate(df,col = Plate, into = c("a","b","c","d","e","f","g","Plate"), sep = "/")
  df[14:20]<-NULL
  df$Read_time<-extract_numeric(df$Read_time)
  df
  
})



#Bind together
df1<-df %>% 
  bind_rows() %>% 
  group_by(Cell, Read_time)

 
###removes NA
junk<- df[[1]] %>%select_if(~all(!is.na(.)))
 junk$Column<-colnames(junk)[1]
 colnames(junk)[1]<-"RFU"
 junk
 
###maybe lapply this to each data frame!!!!make a new column called column with same value as colname[1]
 df2<-df
 
df2<-lapply(df2, function(i){
     df2<- i %>%select_if(~all(!is.na(.)))
  df2$Column<-colnames(df2)[1]
  colnames(df2)[1]<-"RFU"
  df2
}) 

df2<-bind_rows(df2)


df3<-pivot_wider(df2[order(df2$Cell,df2$Read_time),], id_cols = c(Date,Row), names_from = c(Cell,Read_time),values_from = RFU)



df
view(df2)
view(df3)


geom

p<-ggplot(df2, aes(x=Read_time,y=RFU), color = Cell)+
  geom_point(aes(color = Cell), size=2)+
  facet_wrap(~Cell)+
  theme_bw()
ggplotly(p)

#ANIMATION
ggplot(df2, aes(x=Read_time,y=RFU), color = Cell, size = Row)+
  geom_jitter(aes(color = Cell), size=10, alpha = .8)+
  #facet_wrap(~Cell)+
  theme_bw()+
  labs(title = 'Read_time: {round(frame_time, 1)}', x= 'Read_time', y= 'RFU')+
  transition_time(Read_time)+
  ease_aes('linear')







df5<-lapply(FILES, function(i){
  df5<-read.table(file = i,skipNul = TRUE,skip = 18,nrows = 8,fileEncoding="UCS-2LE", sep = "\t")
  df5<-df5[,3:14]
  colnames(df5)<-c(1:12)
  #as.numeric(which.max(colMeans(df[[34]][1:12]))) position of max column
  #df[-as.numeric(which.max(colMeans(df)))]<-NA
  df5$Row<- LETTERS[1:8]
  df5$File<-i
  df5<-separate(df5, col = File, into = c("Plate","Date", "Cell", "Read_time"), sep = "_")
  df5<-separate(df5,col = Plate, into = c("a","b","c","d","e","f","g","Plate"), sep = "/")
  df5[14:20]<-NULL
  df5$Read_time<-extract_numeric(df5$Read_time)
  df5
  
})


df5<-bind_rows(df5) %>% 
  gather(.,key = "Column", value= "RFU", 0:12)




p<-ggplot(df5, aes(x=as.numeric(Column),y=RFU), color = Cell, size = Row)+
  geom_jitter(aes(color = Cell), size=5, alpha = .8)+
  theme_bw()+
  labs(title = 'Read_time: {round(frame_time, 1)}', x= 'Read_time', y= 'RFU')+
  transition_time(Read_time)+
  ease_aes('linear')










####Make us a Map
bind_rows(df)
time<-list(unique(bind_rows(df)[17]))
time<-unlist(time)
sort(time)
seq(1:12)
map<-cbind(seq(1:12),sort(time))
colnames(map)<-c("Column","Time")
rownames(map)<-c(1:12)

