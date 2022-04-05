args<-commandArgs(TRUE)

datafile<-args[1]
outfile<-args[2]
library(ggplot2)
data<-read.delim(datafile,header = TRUE,sep="\t",quote="")
subset(data,Qvalue != '-' )->data
 if (length.POSIXlt(data)>30){ 
  data<-data[1:30,]
 }
data[,2]<-as.character(data[,2])
for (i in 1:dim(data)[1]){
  if(nchar(data[i,2])>60){
    data[i,2]<-paste(substr(data[i,2],1,60),' etc...',sep='') 
  }
}
od=as.numeric(data$TYPE)
class1 <- data$GO_term;class1<-reorder(class1,od,median)
data$class1 <- class1 
GO_domain<-data$TYPE
Gene_number<-data$target_gene_in_this_GO
q_value<-as.character(data$Qvalue)
q_value<-as.numeric(q_value)
dd<-(data$target_gene_in_this_GO/data$all_gene_in_this_GO)/(data$all_target_gene_in_all_GO/data$all_gene_in_all_GO)
g<-ggplot(data,aes(x=dd,y=class1,colour=q_value,size=Gene_number,shape=GO_domain))+theme(title=element_text(face="bold",size=16,colour="black"),axis.title=element_text(face="bold",size=15,colour="black"),axis.text.x=element_text(face="bold",size=13,colour="black"),axis.text.y=element_text(face="bold",size=15,colour="black"),legend.text=element_text(face="bold",size=13,colour="black"),legend.title=element_text(face="bold",size=13,colour="black"))
g<-g+geom_point(stat="identity")+scale_colour_continuous(low="red",high="green",space="rgb")
g<-g+labs(x = "Rich Factor", y = "", title = "Top 30 of GO Enrichment")
ggsave(paste(outfile,".pdf",sep=""),  width=11, height=8)
ggsave(paste(outfile,".png",sep=""),  width=11, height=8)
