
Bioinformatic
install.packages("ape")
install.packages("kmer")
library(ape)
library(kmer)









#find the number of instances of a known K-mer
kmer_count<- function(file, kmer, seq_length){
  seq<-readChar(con  = file , nchars = seq_length)##READ IN THE FILE
  seq<-strsplit(seq, split = "")#MAKE A LIST OF THE DATA
  seq<-as.DNAbin(seq)# CONVERT DATA TO STRUCTURE TYPE DNA
  locations<-where(seq,pattern = kmer)#LOOK FOR OUR KMER OF INTEREST IN THE DATA
  locations<-strsplit(as.character(locations), split = " ")#AS A CHARACTER MUST BE CONVERT TO A LIST FOR REFERENCING
  length(locations[[1]])#GIVES NUMBER OF TIMES ITS OBSERVED
  #locations[[1]]#gives where they are
}

#example
kmer_count(file = "C:/Users/jabbarc/Desktop/quiz1.txt", kmer = "tgt" , seq_length  = 24 )










##what is the highest obseravable sequence of a given length 
seq<-readChar(nchars = 1000 ,con= "C:/Users/jabbarc/Desktop/quiz1.txt")###read in file
seq<-strsplit(seq, split = "") ##make a list of the data
seq<-as.DNAbin(seq)##convert to object of strucutre DNA
x<-kcount(seq, k= 11)#kcount gives a dataframe/matrix fo all possible sequences of length K and how many times they occur
seq<-x
colnames(x)[which.max(x)]#give me a subset of the colname that has a maximum value. This is the kkmer that occurs most often





library(ape)
####find a certain sequence within a target sequence
file<-"C:/Users/jabba/Desktop/Bioinformatic/exercise 1 find kmer.txt"
seq<-readChar(nchars = 1000 ,con = file)
seq<-strsplit(seq, split = "")
seq<-as.DNAbin(seq)
where(seq, "ccc")












#hamming.distance looks at each element if true assigne the value to a counter,
#made it a function so it can be applied iteravily in a loop
library(ape)
library(dplyr)
library(tidyr)
seq1<-read.csv(file = "clipboard")#read in sequence one AS A DATAFRAME
seq2<-read.csv(file = "clipboard")#readin sequence two AS A DATAFRAME
#colnames(seq1)#reassign the colnames to something 
hamm<-function(x,y){
x<-strsplit(colnames(seq1), split = "")#split sequence1  into a list since it exist as a colname
y<-strsplit(colnames(seq2), split = "")#split sequence2  into a list since it exist as a colname
df<-cbind(as.data.frame(x),as.data.frame(y))#combine both list into a structured dataframe
colnames(df)<-c("seq1","seq2")#rename the collnames to someting intelligible
df$hamm<-c(x[[1]] == y[[1]])#go thru each list and test if TRUE or FALSE and assign as new column
sum(df$hamm)#sum up all the TRUE instances
sum(!df$hamm)#summ up all the False instances
}


hamm(seq1,seq2)







3^1+2^4



#look for skew within a sequence
seq<-read.csv(file = "clipboard")#read in the clipbaord
seq<-strsplit(colnames(seq),split = "")#split out the clip board into a string
df<-as.data.frame(seq)#convert to a dataframe
colnames(df)<-"seq"#rename column to soething that makes sense
#making my own for loop i need to do this by hand calculating the skew for each element and assigning to new column position
for(i in 1:c(length(df$seq))){
  df$skew[i] <- (sum(df$seq[c(1:i)] == "G")    -    sum(df$seq[c(1:i)] == "C") )    / (sum(df$seq[c(1:i)] == "G") +   sum(df$seq[c(1:i)] == "C") )
}
#plot the skew
library(ggplot2)
ggplot(data = df, mapping = aes(x=c(1:length(df$seq)), y=skew))+
  geom_point(stat = "identity")+
  scale_x_continuous(name = "seq", breaks = c(1:length(df$seq)), labels = df$seq)
  





####compute hamming distance in itteration moving the target seqeuence along to each position
query<-"ATGC"###what we seek to match
query<-strsplit(query, split = "")##split this out for counting purposes
seq1<-read.csv(file = "clipboard")#read in sequence to match against
  x<-strsplit(colnames(seq1), split = "")#split sequence1  into a list since it exist as a colname
  df<-as.data.frame(x)###convert our sequence to a dataframe
  colnames(df)<-c("seq1")#rename the collnames to someting intelligible
 
for(i in c(1:length(df$seq))){##loop thru creating a new  seq2 each time 
  df$seq2<-c(rep("N",i),query[[1]],  rep("N", length(df$seq1) - (i  + length(query[[1]] ))   ))##query must advance in postiion and fill in the rest
  df$hamm<-c(df$seq1 == df$seq2)##test for true false  
  df$hamm_pos[i]<-sum(!df$hamm)###count up the falses and assign in new column as the hamming distance at that postion
}
 