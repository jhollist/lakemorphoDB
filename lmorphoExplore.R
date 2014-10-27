#' Explore lakemorpho dat
#' 
#' Simple script to begin exploring lakemorpho data and start validation
#' 

# Pull in using ReadTable
#con<-dbConnect(dbDriver("SQLite"),"lakemorpho.sqlite3")
#lmorphoAll<-dbReadTable(con,"lakemorpho")
#dbDisconnect(con)
#str(lmorphoAll)
#system.time(lmorphoAll<-lmorphoAll[!is.na(lmorphoAll$nlaSITE_ID),])

# Connect with dplyr
library(dplyr)
lmorphoAll_src<-src_sqlite("lakemorpho.sqlite3",create=FALSE)
lmorphoAll_sqlite<-tbl(lmorphoAll_src,"lakemorpho")
#system.time(collect(filter(lmorphoAll_sqlite,!is.na(nlaSITE_ID))))

lmorphoNLA<-filter(lmorphoAll_sqlite,!is.na(nlaSITE_ID)) %.%
                  collect()%.%data.frame()


# Function to return adjr2 of two variables
getAdjr2<-function(x,y){
  return(summary(lm(y~x))$adj.r.squared)
}



# Run correctDepth
source("correctDepth.R")
lmNLA_maxdepth<-data.frame(lmorphoNLA$MaxDepthRaw,lmorphoNLA$MaxDepthNLA,lmorphoNLA$region)
by(lmorphoNLA,lmorphoNLA$region,function(x) mean(x[,1]))
by(lmNLA_maxdepth,lmNLA_maxdepth$region,function(x,y) correctDepth(x[,1:2],"influence.measures"))


lmdCorrectionStats_im<-group_by(lmorphoNLA,region)%.%
  summarise(cf=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                            "influence.measures",200)[[1]],
            corv=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                             "influence.measures",200)[[2]],
            RMSEv=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                              "influence.measures",200)[[3]],
            cora=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                              "influence.measures",200)[[4]],
            RMSEa=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                               "influence.measures",200)[[5]],
            n=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                           "influence.measures",200)[[6]])


lmdCorrectionStats_cooks<-group_by(lmorphoNLA,region)%.%
  summarise(cf=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                            "cooks",1000)[[1]],
            cor=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                             "cooks",1000)[[2]],
            RMSE=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                              "cooks",1000)[[3]],
            nTrain=round(length(MaxDepthRaw)*0.8),
            nValid=length(MaxDepthRaw)-round(length(MaxDepthRaw)*0.8))

lmdCorrectionStats_none<-group_by(lmorphoNLA,region)%.%
  summarise(cf=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                            "none",1000)[[1]],
            cor=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                             "none",1000)[[2]],
            RMSE=correctDepth(data.frame(MaxDepthRaw,MaxDepthNLA),
                              "none",1000)[[3]],
            nTrain=round(length(MaxDepthRaw)*0.8),
            nValid=length(MaxDepthRaw)-round(length(MaxDepthRaw)*0.8))

# Group Correlation Results
group_by(lmorphoNLA,region)%.%
  summarise(r=cor(MaxDepthCorrect,MaxDepthNLA,use="pairwise.complete.obs"))

# Plot All
gglmorpho<-ggplot(lmorphoNLA,aes(x=MaxDepthCorrect,y=MaxDepthNLA)) +
  geom_point(aes(color=factor(region))) +
  geom_abline(intercept=0,slope=1)

gglmorpho

# Cooks Distance not always getting it right
# Perhaps broadedn definition of influential points
# use influence.measures() throw out any starred

# Filter out MA02 as comparison
ma02<-filter(lmorphoNLA, region=="MidAtlantic02")
ma02_lm<-lm(MaxDepthNLA~0+MaxDepthRaw,data=ma02)

# Filter out problem children
lowMiss08<-filter(lmorphoNLA, region=="LowerMississippi08") %.%
  data.frame()
lowMiss08<-data.frame(lowMiss08["MaxDepthRaw"],lowMiss08["MaxDepthNLA"])
lowMiss08<-lowMiss08[!is.na(lowMiss08["MaxDepthNLA"]),]
lowMiss08LinMod<-lm(MaxDepthNLA~0+MaxDepthRaw,data=lowMiss08)
lowMiss08Infl<-as.logical(apply(influence.measures(lowMiss08LinMod)[[2]],1,sum)) 
lowMiss08NoInfl<-lowMiss08[!lowMiss08Infl,]
lowMiss08LinModNoInfl<-lm(MaxDepthNLA~0+MaxDepthRaw,data=lowMiss08NoInfl)
summary(lowMiss08LinModNoInfl)

ggLowMiss08<-ggplot(lowMiss08,aes(x=MaxDepthRaw,y=MaxDepthNLA)) +
  geom_point() +
  geom_abline(intercept=0,slope=1)

ggLowMiss08

ggLowMiss08NoInfl<-ggplot(lowMiss08NoInfl,aes(x=MaxDepthRaw,y=MaxDepthNLA)) +
  geom_point() +
  geom_abline(intercept=0,slope=1)

ggLowMiss08NoInfl

# Group Correlation Results
group_by(lowMiss08NoInfl,region)%.%
  summarise(r=cor(MaxDepthCorrect,MaxDepthNLA,use="pairwise.complete.obs"))


# Notes on analysis that needs to happen

1. calcualte new correction factors per region
2. use new factors to calculate corrected depths
3. use new factors to calculate corrected volumes

# Test correctDepth
lowMiss08<-filter(lmorphoNLA, region=="LowerMississippi08") %.%
  data.frame()
lowMiss08<-data.frame(lowMiss08["MaxDepthRaw"],lowMiss08["MaxDepthNLA"])
correctDepth(lowMiss08,"cooks")


