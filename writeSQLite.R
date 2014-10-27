#'  Create a flat database from csv files 
#'  
#'  This function is likely a one-off.  Designed to pull in csv files and output
#'  to a sqlite3 database.  This is first step in building up the Lake Morphometry
#'  database.
#'  
#'  @param csvfile A character list pointing to the csv file
#'  @param sqlitedbase name of sqlite3 database to write csv file to
#'  
writeSQLite<-function(csvfile, sqlitedbase, ...){
  require(RSQLite)
  csvfile<-as.list(csvfile)
  con<-dbConnect(dbDriver("SQLite"),dbname=sqlitedbase)
  for(i in 1:length(csvfile)){
    if(dbExistsTable(con,"lakemorpho")){
      dbWriteTable(con,"lakemorpho",csvfile[[i]],row.names=FALSE,append=TRUE,header=T)
    } else {
      dbWriteTable(con,"lakemorpho",csvfile[[i]],row.names=FALSE,header=T)
    }
  }
  dbDisconnect(con)
} 

