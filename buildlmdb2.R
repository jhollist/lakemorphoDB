#'  Create sqlite dbase for lakemorpho
#'  
#'  This function is likely a one-off for using the writeSQLite function to create a
#'  single sqlite dbase (and table for the time being).  The point of this is to 
#'  facilitate the validation of the predictions
#'  
#'  @param homeDir character vector of the home directory in which to start looking for
#'  the csv files. 
#'  
#'  @examples
#'  source("writeSQLite.R")
#'  hd<-"L:\\Priv\\LakeMorphometry"
#'  buildlmdb(hd)
buildlmdb<-function(homeDir){
  folders<-list.dirs(homeDir,full.names=TRUE,recursive=FALSE)
  folders<-folders[grep("[0-9LNSWU]$", folders)]
  myCsv<-list()
  for(i in 1:length(folders)){
    load(list.files(folders[i],"*.RData",full.names=T))
    myCsv[[i]]<-data.frame(morphoDF,region=substr(folders[i],regexpr("/",folders[i])+1,nchar(folders[i]))) 
  }
  writeSQLite(myCsv,"lakemorpho.sqlite3")
} 