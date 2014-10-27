#' Simple R Script to determine classification accuracy of depth

plosDepth<-read.csv(url("http://www.plosone.org/article/fetchSingleRepresentation.action?uri=info:doi/10.1371/journal.pone.0025764.s003"))
nlaPlosDepth<-plosDepth[!is.na(plosDepth$NLAMeasMaxDepth_m),]
webPlosDepth<-plosDepth[!is.na(plosDepth$WebMaxDepth_m),]

#1. Shallow, Med, Deep (0-20ft,20-100ft, >100ft) 
depthclassWeb<-cut(webPlosDepth$WebMaxDepth_m,breaks=c(0,6.1,30.5,500),labels=c("Shallow","Medium","Deep"))
depthclassPred<-cut(webPlosDepth$PredMaxDepth_m_correct,breaks=c(0,6.1,30.5,500),labels=c("Shallow","Medium","Deep"))
classTable1<-table(data.frame(depthclassWeb,depthclassPred))
(classTable1[1,1]+classTable1[2,2]+classTable1[3,3])/sum(classTable1)

#2. Shallow, Med, Deep (0-1t,20-100ft, >100ft) 
depthclassWeb<-cut(webPlosDepth$WebMaxDepth_m,breaks=c(0,6.1,30.5,500),labels=c("Shallow","Medium","Deep"))
depthclassPred<-cut(webPlosDepth$PredMaxDepth_m_correct,breaks=c(0,6.1,30.5,500),labels=c("Shallow","Medium","Deep"))
classTable1<-table(data.frame(depthclassWeb,depthclassPred))
(classTable1[1,1]+classTable1[2,2]+classTable1[3,3])/sum(classTable1)