install.packages("wordcloud")
install.packages("tm")
install.packages("RColorBrewer")
library(wordcloud)
library(RColorBrewer)


subject<-c("Assay Development", "Biochemistry", "Molecular Biology", 
            "DNA" , "Protein", "High Content Imaging", "R studio","Resourceful", "Creativity",
            "Python", "Data Science", "Max Kuhn", "Organization", "Streamlining", 
            "Effiecency", "Documentation", "MSD", "Electrophysiology", "Liquid Handling", 
           "Echo", "Target Engagement", "Automation", "Tempest", "syntax", "Quantification", "Sensitivity ")


freq<- seq(from = 1000, to = 1 , by = ((1- 1000)/(26- 1)))





set.seed =3
wordcloud(words = subject, freq = freq, scale = c(2,.5), max.words = Inf, color = brewer.pal(8,name = "Dark2"))



