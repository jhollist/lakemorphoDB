#script to compare NHD and NLA lake locations
#Initial method missed ~280 lakes

setwd("L:\\Priv\\LakeMorphometry")
direct1<-list.dirs(".",recursive=F)
direct1<-direct1[grep("[0-9LNSWU]$", direct1)]
myNLA<-read.csv("lakeinformation_sampled.csv")
nlaPoints<-SpatialPointsDataFrame(data.frame(myNLA$LON_DD,myNLA$LAT_DD),myNLA,
                                  proj4string=CRS("+proj=longlat +datum=NAD83"))
#nlaPoints<-spTransform(nlaPoints,CRS(proj4string(myLakes)))
#nlaPoints<-nlaPoints[nlaPoints$VISIT_NO==1,]
nlaCOMID<-data.frame()
for(i in direct1)
{
  setwd(paste("L:\\Priv\\LakeMorphometry",i,sep="\\"))
  shp<-gsub(".shp","",list.files(pattern="ed1.shp")[1])       
  myLakes<-readOGR(".",shp)
  nlaPoints<-spTransform(nlaPoints,CRS(proj4string(myLakes)))
  nlaPoints<-nlaPoints[nlaPoints$VISIT_NO==1,]
  if("ComID" %in% names(myLakes)){
    names(myLakes)[names(myLakes)=="ComID"]<-"COMID"
  }
  
  myLakes2<-myLakes[is.na(as(myLakes,"SpatialPolygons") %over%
                          as(nlaPoints,"SpatialPoints"))==F,]
  
  nlaOverLakesInd<-is.na(as(nlaPoints,"SpatialPoints") %over% 
         as(myLakes,"SpatialPolygons"))==F
  nlaPointsOver<-nlaPoints[nlaOverLakesInd,]
  
  for(j in 1:sum(nlaOverLakesInd)){
      nlapt<-nlaPointsOver[j,]
      sid<-as.character(nlapt$SITE_ID)
      huc<-as.character(nlapt$HUC_2)
      cid<-myLakes2[!is.na(as(myLakes2,"SpatialPolygons") %over% as(nlapt,"SpatialPoints")),]$COMID
      nlaCOMID<-rbind(nlaCOMID,data.frame(SITE_ID=sid,COMID=cid,HUC_2=huc))
    }
}

sidout<-as.character(nlaPoints$SITE_ID[!nlaPoints$SITE_ID %in% nlaCOMID$SITE_ID])
nlaCOMID<-rbind(nlaCOMID,data.frame(SITE_ID=sidout,COMID=rep(0,length(sidout)),HUC_2=rep(NA,length(sidout))))


# One point occurs twice, with two separate COMIDs
nlaCOMID[nlaCOMID$SITE_ID=="NLA06608-2477",]

#And occurs in these hucs 
myNLA[myNLA$SITE_ID=="NLA06608-2477",]$HUC_8
  
#The full list of ID's without a mathcing lake are
nomatch<-nlaCOMID[nlaCOMID$COMID==0,]
setwd("L:\\Priv\\LakeMorphometry\\UpperMissouri10U")
shp<-gsub(".shp","",list.files(pattern="ed1.shp")[1])       
huc10Lakes<-readOGR(".",shp)
for(i in 1:nrow(nomatch)){
  nomatch[i,]$HUC_2<-nlaPoints[nlaPoints$SITE_ID==nomatch[i,]$SITE_ID,]$HUC_2
  mypt<-nlaPoints[nlaPoints$SITE_ID==nomatch[i,]$SITE_ID,]
  print(sum(as(huc10Lakes,"SpatialPolygons") %over% as(mypt,"SpatialPoints"),na.rm=T))
}



