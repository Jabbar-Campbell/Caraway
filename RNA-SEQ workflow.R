#THERE ARE QUITE A FEW PACKAGES BUT GENERALLY THE ARE MEANT TO CONVERT SEQ DATA TO A 
#DATA STRUCTURE THAT R CAN WORK WITH 





##since airway is a package not asssociated with github there some extra
#instructing to get packages from Bioconducter
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
##from biomanger we can retrieve the package airway
BiocManager::install("airway")
library(airway)


#The package airway has some data sets  much like cars from tidy package
#system.file allows us to see dir associated to a particular package
dir<- system.file("extdata", package = 'airway', mustWork=TRUE)
#we can list them an save them as FILES
FILES<-list.files(dir, full.names = TRUE)




#file one has web info and project related information
metadata<-read.csv(file = FILES[1])
#file 4 has cell and sample ID information
metadata<-read.csv(file = FILES[4])
#file 5 has data realted to the run on illumina
metadata<- read.csv(file = FILES[5])


#we can take the first column of file 5 and attach text denoting it as bam 
#or just parse the folder for bam files i choose to just filter the directory
mybam<-list.files(dir, pattern = ".bam", full.names = TRUE)
#the full path to the first file is...
mybam[1]



#again install.packages wont work so we need to get the package from Biooconductor
BiocManager::install("Rsamtools")
library("Rsamtools")
#if we use the BamFile list function this saves the bam files as a true
#Formalized "BAM-data" structure
#as opposed to a list of files that happen to be BAMs
mybam<- BamFileList(mybam, yieldSize=2000000)




#we can now call seq info on each one
seqinfo(mybam[1])


#since we need a model/organism to align these too let pull one in from Ensemble
#using the GenomicFeatures package. 
BiocManager::install("GenomicFeatures")
library("GenomicFeatures")
makeTxDbFromEnsembl()#You can pull them in from Ensemble 
makeTxDbFromGFF()#save them in various formats such as GFF
mygtf<-makeTxDbFromGFF(file = FILES[2], format = "gtf")#since our own gene feature file lets use that


#we can inspect the exons using exonBy() and other features from this package
#introns we wouldnt want since these arent coded but we could look
gtf#it appears to be human
exonsBy(mygtf)#give me all the exons
myexons<-exonsBy(mygtf, by= "gene")#we can group them by gene
 


#next we can use  summarizeOvelaps() from the GenomicAlignments package
#which reads map to which exons
#CAREFUL EACH BAM FILE CAN TAKE 30 MINUTES OR MORE!!!!!
#ONE SHOULD DEFINETLY USE MULTIPLE CORES using  MulticoreParam() or SnowParam() 
#from the BiocParallel package
BiocManager::install("GenomicAlignments")
library(GenomicAlignments)
summarizeOverlaps()
summarizeOverlaps_(features=myexons,#exons from gene feature struct 
                  reads=mybam,#bam struct
                  mode="Union,#type of overlaps to count
			            singleEnd=FALSE,#there are paired-end reads, we want to count as one
			            ignore.strand=TRUE,#strand specific?
			            fragments=TRUE )






 RPKM (Reads Per Kilobase Million) or 
 FPKM (Fragments Per Kilobase Million). 
 TPM (Transcripts Per Kilobase Million) is now becoming quite popular.
 
 
Here's how you do it for RPKM:
Count up the total reads in a sample and divide that number by 1,000,000 - this is our "per million" scaling factor.
Divide the read counts by the "per million" scaling factor. This normalizes for sequencing depth, giving you reads per million (RPM)
Divide the RPM values by the length of the gene, in kilobases. This gives you RPKM.


FPKM is very similar to RPKM. RPKM was made for single-end RNA-seq, FPKM was made for paired-end RNA-seq.

To calculate TPM:
  Divide the read counts by the length of each gene in kilobases. This gives you reads per kilobase (RPK).
  Count up all the RPK values in a sample and divide this number by 1,000,000. This is your "per million" scaling factor.
  Divide the RPK values by the "per million" scaling factor. This gives you TPM.
