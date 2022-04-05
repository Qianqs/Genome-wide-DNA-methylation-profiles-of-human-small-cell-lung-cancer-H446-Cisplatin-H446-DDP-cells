args<-commandArgs(TRUE)

datafile<-args[1]
outfile<-args[2]
library(ggplot2)
data<-read.delim(datafile,header = TRUE,sep="\t")
subset(data,Qvalue != '-' )->data
if (length.POSIXlt(data)>30){ 
  data<-data[1:30,]
 }
Gene_number<-data$target_gene_in_this_pathway
q_value<-as.character(data$Qvalue)
q_value<-as.numeric(q_value)
dd<-(data$target_gene_in_this_pathway/data$all_gene_in_this_pathway)/(data$target_gene_in_all_pathway/data$all_gene_in_all_pathway)
g<-ggplot(data,aes(x=dd,y=PATHWAY_DES,colour=q_value,size=Gene_number))+theme(title=element_text(face="bold",size=16,colour="black"),axis.title=element_text(face="bold",size=15,colour="black"),axis.text.x=element_text(face="bold",size=13,colour="black"),axis.text.y=element_text(face="bold",size=15,colour="black"),legend.text=element_text(face="bold",size=13,colour="black"),legend.title=element_text(face="bold",size=13,colour="black"))
g<-g+geom_point(stat="identity")+scale_colour_continuous(low="red",high="green",space="rgb")
g<-g+labs(x = "Rich Factor", y = "", title = "Top 30 of Pathway Enrichment")
g
ggsave(paste(outfile,".pdf",sep=""),  width=10, height=8)
ggsave(paste(outfile,".png",sep=""),  width=10, height=8)
