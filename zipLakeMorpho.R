#'  Function to pull together and zip up lake morphometry data
#' 
#'  This fucntion takes a list of HUC regions, grabs the appropriate shape files 
#'  and .csv's and creates a zip file from them
#' 
#'  @param hucVec this is a vector listing the HUCS you want to pull together into an
#'  output zip file
#'  @param outFile this is a string with the name of the output zip file.  Default value
#'  is outputLakeMorpho.zip
#'
#'  @examples
#'  x<-c("ArkRedWhite11","California18") 
#'  zipLakeMorpho(x) 
#'  #Example for Eastern TNC Lakes
#'  eTNCRegions <- c("NorthEast01","MidAtlantic02","SouthAtlanticNorth03N",
#'                   "SouthAtlanticWest03W","SouthAtlanticSouth03S","GreatLakes04",
#'                   "Ohio05","Tennessee06")
#'  zipLakeMorpho(eTNCRegions,"epaLakeMorphoForTNC.zip")                  
zipLakeMorpho<-function(hucVec,outFile="outputLakeMorpho.zip"){
  # Assumes location is AED Network drive
  basePath<-"L:\\Priv\\LakeMorphometry"
  dirs<-list.files(basePath)[list.files(basePath)%in%hucVec]
  myFiles<-NULL
  for(dir in dirs){
    myPath<-paste(basePath,dir,"lakemorphodata\\FinalLakeMorpho",sep="\\")
    myFiles<-c(myFiles,list.files(path=myPath,pattern=dir,full.names=T))
  }
  myFiles<-c(myFiles,list.files(path=basePath,pattern="DataDictionary",full.names=T))
  zip(outFile,myFiles,flags="-j")
}  
