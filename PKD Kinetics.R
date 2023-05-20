library(tidyverse)


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
df<-separate(df,col = Plate, into = c("a","b","c","d","e","Plate"), sep = "/")
df[14:18]<-NULL
df$Read_time<-extract_numeric(df$Read_time)
extract_numeric(df$Read_time)
















#Read in our Data
FILES<-list.files(path = "C:/Users/jabba/Desktop/Sandbox", full.names = TRUE)


df<-lapply(FILES, function(i){
  df<-read.table(file = i,skipNul = TRUE,skip = 18,nrows = 8,fileEncoding="UCS-2LE", sep = "\t")
  df
  df<-df[,3:14]
  colnames(df)<-c(1:12)
  #as.numeric(which.max(colMeans(df[[34]][1:12]))) position of max column
  df[-as.numeric(which.max(colMeans(df)))]<-NA
  df$Row<- LETTERS[1:8]
  df$File<-i
  df<-separate(df, col = File, into = c("Plate","Date", "Cell", "Read_time"), sep = "_")
  df<-separate(df,col = Plate, into = c("a","b","c","d","e","Plate"), sep = "/")
  df[14:18]<-NULL
  df$Read_time<-extract_numeric(df$Read_time)
  df
  
})


####Make us a Map
bind_rows(df)
time<-list(unique(bind_rows(df)[17]))
time<-unlist(time)
sort(time)
seq(1:12)
map<-cbind(seq(1:12),sort(time))
colnames(map)<-c("Column","Time")
rownames(map)<-c(1:12)

#Bind together
df1<-df %>% 
  bind_rows() %>% 
  group_by(Cell, Read_time)
