http://ec2-54-84-12-219.compute-1.amazonaws.com/
  login rstudio
passwd rstudio


#normal package installation wont work....these packages come from Bioconductor hence require some extra code for installing  they 
#come with depensendies but other packages you might need are
library(BSgenome)
library(Rsamtools)
library(rtracklayer)
library(GenomicFeatures)
library(Gviz)


update.packages (ask = FALSE)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager", dependencies = TRUE)
BiocManager::install("QuasR")
library (QuasR)




#https://stat.ethz.ch/pipermail/bioconductor/2013-November/055792.html
#The sample file is a list ofåΩ txt file that list the location of your sequence  files. it doesnt contain and sequence itself
#I've created smaller verions of the fastq files and have called them test_reads.fq
# in linux i took the first 4 sequences or 16 lines of each compressed.gz and made new fq files 
#the command in linux is "zcat 1-1_R1_001.fastq.gz | head -16  > test_reads.fq" 
#these are the paths. 
/home/ubuntu/Genewiz/test_reads.fq
/home/ubuntu/Genewiz/test_reads2.fq

/home/ubuntu/Genewiz/sftp


#to make a sample file change path to the path with the path to your fastq file

tab <- data.frame(FileName=c("C:/path/to/seqs/SRR653521.fastq"),
                  SampleName=c("Sample1"))
write.table(tab, "mysamples.txt", quote=FALSE, row.names=FALSE, col.names=TRUE, sep="\t")



#using the above example we get 
list.files("/home/ubuntu/Genewiz")
FILES<-list.files("/home/ubuntu/Genewiz/sftp", full.names = TRUE)
tab<- data.frame(FileName=FILES, 
                 SampleName=c("Sample1", "Sample2", "Sample3", "Sample4"))
write.table(tab, "mysamples.txt", quote=FALSE, row.names=FALSE,col.names=TRUE, sep="\t")


#the path to this new sample file mysamples.txt is 
/home/rstudio/mysamples.txt
sampleFile<-"/home/rstudio/mysamples.txt"


#Genomle files
#if you want to download genome file versions go here https://www.gencodegenes.org/human/release_19.html since file needs to be 
#fasta for qAlign() pick the fasta and asign the path once you get it
genomeFile<- "/home/ubuntu/Genewiz/GRCh37.p13.genome.fa"


#qAlign takes a the paths to the seq files and to the alignment file and places the result  in the 
#directory of choice but careful this package makes use of samtools which will choke if the genomefile is to big
qAlign(sampleFile, genomeFile, alignmentsDir = "/home/ubuntu/Genewiz/seqs")



#indeed the genome file is giving qalign problems....maybe packages "seqinr" and "CHNOSZ" can help inspect it. 
install.packages("CHNOSZ")
library(CHNOSZ)
mygenomeFile<-read.fasta(file = "/home/ubuntu/Genewiz/GRCh37.p13.genome.fa", ret = "FASTA",  iseq = 1)



#YEP its indeed a FASTA file looks like  there are 297  very long sequences  which i asssume corresponds to chromosomes 
#for a total of 594 lines, thats 2 lines for every sequences/chromosome maybe just pick one....
#in linux zcat GRCh37.p13.genome.fa | head -5  > testgenome.fa creates a truncated version
#the path to which is /home/ubuntu/Genewiz/testgenome.fa
genomeFile<-"/home/ubuntu/Genewiz/testgenome.fa"
qAlign(sampleFile, genomeFile= "BSgenome", alignmentsDir = "/home/ubuntu/Genewiz/seqs")


##HMMM still not working perhaps its not write permission or even the size thats the problem 
#lets try getting a genomefile from somehere else....
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("BSgenome.Hsapiens.UCSC.hg19")
library("BSgenome.Hsapiens.UCSC.hg19")



#FINALLY!!!!!!
myproj_full<-QuasR::qAlign(sampleFile, genome = "BSgenome.Hsapiens.UCSC.hg19", alignmentsDir = "/home/ubuntu/Genewiz/seqs")

#read.delim(file = "/home/rstudio/QuasR_log_bba57673391b.txt") #to see what going on



#QuasR will treat these genomes as if they would be seperate chromosomes
#of a single genome, which may not be what you want. For example, reads
#mapping to regions that are identical in the separate genomes will be
#randomly assigned. It is probably preferrable to select one of the
#genomes as a reference, save it to a fasta file, and use that instead
#for your genome argument in qAlign:

BSgenome.Hsapiens.UCSC.hg19
help(package = "BSgenome.Hsapiens.UCSC.hg19")
seqlengths(BSgenome.Hsapiens.UCSC.hg19)
BSgenome.Hsapiens.UCSC.hg19$chr1 
BSgenome.Hsapiens.UCSC.hg19[[1]]

##since the entire genome is quite large perhaps we should only take chromosome 1 for now  
singleGenome <- as(BSgenome.Hsapiens.UCSC.hg19[["chr1"]], "DNAStringSet")##turn chr1 into a string
names(singleGenome) <- "chr1"#label it as chromosome 1
writeXStringSet(singleGenome, "Hsapien_genome_chr1.fa")##write this to a file
genomeName <- "Hsapien_genome_chr1.fa"##use this name instead of long as path location


##we could also convert the entire genome into a file and use that
#singleGenome <- as(BSgenome.Hsapiens.UCSC.hg19, "DNAStringSet")##turn chr1 into a string
#names(singleGenome) <- "full_genenome"#label it as full genome
#writeXStringSet(singleGenome, "Hsapien_genome_full.fa")##write this to a file
#genomeName <- "Hsapien_genome_full.fa"##use this name instead of long as path location




#bam files will be generated and stored in object myproj for test_reads 1 and 2 against chr1 of hg19
#be sure to give destination folder read and write folders
#/home/ubuntu/Genewiz/test_reads.fq
#/home/ubuntu/Genewiz/test_reads2.fq
myproj<-QuasR::qAlign(sampleFile, genome = genomeName, alignmentsDir = "/home/ubuntu/Genewiz/seqs")





# we can count reads using a "GRanges" query but in order to count the alingnments we need turn our genome 
#into a stringset object that will  give us chromosme regions that map ontop
#chr1 of "BSgenome.Hsapiens.UCSC.hg19" in this case.

#turns genome file of chr 1  into object we can extract ranges from  in the next step
gseq <- readDNAStringSet(genomeName)


#areas of overlap to look for and what to call them hopefully from a gene map. this key will be called chrRegions
#so we not which chromosomes the overlaps come from
chrRegions <- GRanges(names(gseq), IRanges(start=1,width=width(gseq),names=names(gseq)))#areas of overlap to look within

library(Rsamtools)

####################or###################

genomeRegion <- Rsamtools::scanFaIndex("/home/ubuntu/Genewiz/gencode-hr19.gtf")#reads seq names and the widths that they span
chrominfo <- data.frame(chrom=as.character(seqnames(genomeRegion)),
                        length=end(genomeRegion),
                        is_circular=rep(FALSE, length(genomeRegion)))#makes a dataframe from the genenames in this object



#we can no count points of overlap with these set ranges for each chromosome
#these genomic ranges objects work
mycountchr1<-QuasR::qCount(proj=myproj_full, query=chrRegions, reportLevel="gene")#other report levels can be exon etc.
mycount<-QuasR::qCount(proj=myproj_full, query=genomeRegion, reportLevel="gene")



#path to genome file from stephen
/home/ubuntu/Genewiz/gencode-hr19.gtf




#instead of counting overlap using Granges we can us "Txdb" to count areas that correspond to a gene 
#from a Txdb object that has meta data like gene names, regions/locations and report levels ie
#if its a gene or not...
#makeTxDbFromGFF() for some reason can't process this file due to chrominfo lets try and use a different 
#annotation file from UCSC which requires the RMariaDB rtracklayer and GenomicFeatures package 
BiocManager::install("RMariaDB")
install.packages("RMariaDB")
library(RMariaDB)
library('GenomicFeatures')
library(rtracklayer)
ucscGenomes()[ , "db"] #list of all available db at UCSC
ucscGenomes("hg19")



txdb<-makeTxDbFromUCSC(genome="hg19",tablename="knownGene",transcript_ids=NULL,circ_seqs=DEFAULT_CIRC_SEQS,
                       url="http://genome.ucsc.edu/cgi-bin/", goldenPath_url="http://hgdownload.cse.ucsc.edu/goldenPath",
                       taxonomyId=NA, miRBaseBuild=NA)
supportedUCSCtables()

txdb<-asGFF(txdb)###convert to genomic ranges object has chromosome information


txdb2<- makeTxDbFromGFF(file = "/home/ubuntu/Genewiz/gencode-hr19.gtf", format = "gtf")##txdb from stephens file
txdb2<-asGFF(txdb2)#has GeneID numbers


#early we used the ranges of chromosome 1
#now we can count with the using the txdb object, the alignments in myproj_full which are to the whole genome can then be counted
#against a txdb object (the enitre genome) thats been converted to ranges. 

mycount<-QuasR::qCount(proj=myproj_full, query=txdb, reportLevel="gene")#using genome from ucsc as a txdb object 
mycount2<- QuasR::qCount(proj=myproj_full, query=txdb2, reportLevel="gene")#using stephens file as a txdb object
head(txdb2)

BiocManager::install("Gviz")
library(Gviz)
library(rtracklayer)
library(GenomicRanges)
data("cpgIslands")
class(cpgIslands)
## [1] "GRanges"
## attr(,"package")
## [1] "GenomicRanges"
chr <- as.character(unique(seqnames(cpgIslands)))
gen <- genome(cpgIslands)
atrack <- AnnotationTrack(cpgIslands, name = "CpG")

seqlevels(txdb)# all the chromosomes in our txdb obect
chr <- as.character(unique(seqnames(txdb)))#all chromosomes of txdb object
gen <- genome(myproj_full)#????
atrack <- AnnotationTrack(gen, name = "myproj_full")

#Genome axis track works ok but annotation track seem to give
#memory issues with txdb or even a range but multiple chromosomes only 
#get an error "cannot allocate vector of size 3007.0 Gb" maybe to many ranges/genes
#however it seems to be able to handle  a specific chromsome at a certain spot
#data track takes the data from mycount
atrack <- AnnotationTrack(range = txdb ,name = "myproj_full")
GeneRegionTrack(range = txdb )
IdeogramTrack(range = txdb, genome = "hg19", chromosome = 14)


#not evene a different source file helps...still memory issues         
annotationFile<-"/home/ubuntu/Genewiz/gencode-hr19.gtf"
regions<-import.gff(annotationFile, format = "gtf")
atrack<-GeneRegionTrack(range = regions)
GeneRegionTrack(regions, name = "genes", start = 58858172, end =  58874214, showId = TRUE) #specifying cuts down on memory load :)

#however
##we can set an axis using the entire range
axisTrack<-GenomeAxisTrack(range =  txdb, name = "hg19")

##if we wanted to look at a specific chromosome we could add an  IdeogramTrack        
itrack <- IdeogramTrack(genome = "hg19" ,chromosome = "chr14")

#to plot data tracks we first need Bigwig files. These ar generated off the alignments for each sample
#named in the sample file
qExportWig(myproj_full, binsize=100L, scaling=TRUE, collapseBySample=TRUE)
gr1 <- import("Sample1.wig.gz")
gr2 <- import("Sample2.wig.gz")
gr3 <- import("Sample3.wig.gz")
gr4 <- import("Sample4.wig.gz")
dTrack1 <- DataTrack(range=gr1, name="Sample 1", type="h")
dTrack2 <- DataTrack(range=gr2, name="Sample 2", type="h")
dTrack3 <- DataTrack(range=gr3, name="Sample 2", type="h")
dTrack4 <- DataTrack(range=gr4, name="Sample 2", type="h")
plotTracks(list(axisTrack, dTrack1, dTrack2, dTrack3, dTrack4, itrack), extened.left= 1000)








#DESeq2 has a whole new learning curve :(....coldata needs to be and transposition of your counts such that cols from 
#your qcount() are now rows.both count data and transposed coldata also need to have columns "design" and "batch" tacked on)
#  Please read the vignette section 'Model matrix not full rank':
BiocManager::install("DESeq2")
library(DESeq2)

#mycount_dseq<-mycount
#colnames(mycount_dseq)

#bind a column for batch onto matrix
#mycount_dseq<- cbind(mycount_dseq, rep(0, dim(mycount_dseq)[1]))#
#colnames(mycount_dseq)[4]<-"batch"

##bind a column for condition onto matrix
#mycount_dseq<- cbind(mycount_dseq, rep(0, dim(mycount_dseq)[1]))
#colnames(mycount_dseq)[5]<-"condition"


colnames(mycount_dseq)<-mycount_dseq[1,]

rownames(x)
x<-mycount

rownames(x)<-x[,1]###switch numbers to widths for row names
x<-x[,-1]###get rid of first column or widths

mycount_dseq<-as.data.frame(x) #turn it into a dataframe

#mycount_dseq<-mycount_dseq[,-1]
mycount_dseq<-t(mycount_dseq)

colnames(mycount_dseq)<-mycount[,1]
rownames(mycount_dseq)
colnames(mycount_dseq)[635288]
#dim(mycount_dseq)

#colnames(mycount_dseq)[,635290]<-"batch"
#colnames(mycount_dseq)[,635289]<-"condition"

mycount_dseq<-cbind(mycount_dseq,  factor(c("A", "A","B","B")))#as a matrix columns are added with cbind 
colnames(mycount_dseq)[dim(mycount_dseq)[2]]<-"condition"#and then given a column name


mycount_dseq<-cbind(mycount_dseq, factor(c(1,1,1,1)))
colnames(mycount_dseq)[dim(mycount_dseq)[2]]<-"batch"

my_deseq <- DESeqDataSetFromMatrix(countData = x,
                                   colData = mycount_dseq,
                                   design=   ~  condition)

as.factor(c("a","b"))

vignette("DESeq2")

dds<-DESeq(my_deseq)#

res<-results(dds)#calculates results on dds object
summary(res)
res
mcols(res, use.names=TRUE)


#which of res is significant
sum( res$padj < 0.1, na.rm=TRUE )
resSig <- res[ which(res$padj < 0.1 ), ]
head(resSig)

#strongest upregulation
tail( resSig[ order( resSig$log2FoldChange ), ] )


#over view of the experiment
plotMA( res, ylim = c(-1, 1) )

#plots dispersion elements
plotDispEsts( dds, ylim = c(1e-6, 1e1) )

hist( res$pvalue, breaks=20, col="grey" )


#this library get the ensemble id and swaps in gene names
library( "biomaRt" )


#r log transforms the data
rld <- rlog( dds )


#these help mark lows signal genes
par( mfrow = c( 1, 2 ) )
plot( log2( 1+counts(dds, normalized=TRUE)[, 1:2] ), col="#00000020", pch=20, cex=0.3 )
plot( assay(rld)[, 1:2], col="#00000020", pch=20, cex=0.3 )

#Heatmapping and distance matrices
sampleDists <- dist( t( assay(rld) ) )
sampleDists
sampleDistMatrix <- as.matrix( sampleDists )

install.packages("gplots")
library( "gplots" )
library( "RColorBrewer" )
rownames(sampleDistMatrix) <- paste( rld$treatment,rld$patient, sep="-" )
colnames(sampleDistMatrix) <- NULL
colours = colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
heatmap.2( sampleDistMatrix, trace="none", col=colours)



library( "genefilter" )
topVarGenes <- head( order( rowVars( assay(rld) ), decreasing=TRUE ), 35 )
heatmap.2( assay(rld)[ topVarGenes, ], scale="row",trace="none", dendrogram="column",col = colorRampPalette( rev(brewer.pal(9, "RdBu")) )(255))





