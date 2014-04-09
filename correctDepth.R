#' Calulate Correction Factors for Lake Depth
#' 
#' This function calculates correction factors, based off of Hollister et al. 2011,
#' for maximum lake depth estimates.  These correction factors are based off a 
#' comparison of maximum depth estimates based on median topology to measured maximum
#' depth estimates. A regression line through the origin is fit to these data.  The
#' slope of the resulting line is used to correct the original estimates.  
#' 
#' Outliers may be found that skew the correction factor and hinder prediction 
#' accuracies. Two methods are available, one that removes observations with a Cook's
#' distance greater than or equal to one.  This is the method as described in 
#' Hollister et al. 2011. An additional method is to remove observations that are 
#' identified as being an outlier with `influence.measures()`.  This second method is
#' a more liberal definition of outlier and will remove a larger number of points.
#' Alternatively, all observations may be left in.
#' 
#' Lastly, the correction factor may be based of the original data or off of the
#' median value from a set of cross-validated samples,as in Hollister et al. 2011.  
#' 
#' @param maxDepthDF a data frame containing two columns.  First column contains the 
#'        uncorrected maximum depth predictions. These are usually acquired from 
#'        lakemorpho::lakeMaxDepth. The second columns contain measured maximum depth
#'        for the same lengths.  In Hollister et al. 2011, the US EPA National Lake
#'        Assessment were used for this. NA values are removed pairwise.
#' @param outMethod a character indicating which method to use to remove outliers.
#'        Acceptable values are "none", "cooks","influence.measures".
#' @param iter the number of cross-valiterations 
#' @param crossValSize the size of the cross-validation sample.  Defaults to 80% for 
#'        training and 20% for validation
#'        
#' @return correctDepth returns a list with 3 elements.  The first element is the 
#'         correction factor (median if cross-validation is used), the second 
#'         element is the correlation (mean if cross-validated) between the 
#'         corrected depths and the measured depths, the last element is the RMSE 
#'         (mean if cross-validated) between the corrected and measured maximum depth
#'         values.  
#Function to correct lake depths as identified in Hollister et al 2011
correctDepth<-function(maxDepthDF,outMethod=c("cooks","influence.meausres","none"),
                       iter=100, crossValSize=0.8,seed=91920110){
 
  # Fit inital regression model on all input data
  xdf<-na.omit(data.frame(maxDepthDF[,1],maxDepthDF[,2]))
  names(xdf)<-c("predicted","observed")
  xdflm<-lm(xdf$observed~0+xdf$predicted)
  xdf2<-xdf
  
  # Check for and remove outliers based on user input method
  if(outMethod == "cooks"){
    xdf2<-xdf[cooks.distance(xdflm)<1,]
    xdflm<-lm(xdf2$observed~0+xdf2$predicted)
  } else if (outMethod == "influence.measures"){
    xdf2<-xdf[!as.logical(apply(influence.measures(xdflm)[[2]],1,sum)),] 
    xdflm<-lm(xdf2$observed~0+xdf2$predicted)
  } else if (outMethod == "none") {
    xdflm<-lm(xdf2$observed~0+xdf2$predicted)
  } else {
    stop(paste(outMethod, "Not a valid outMethod",sep=" "))
  }
  
  # Run 
  xSlope<-vector("numeric",iter)
  xCorv<-vector("numeric",iter)
  xRMSEv<-vector("numeric",iter)
  xCora<-vector("numeric",iter)
  xRMSEa<-vector("numeric",iter)
  set.seed(seed)
  for(i in 1:iter)
  {
    samp<-sample(nrow(xdf2),nrow(xdf2)*crossValSize)
    xdfTrain<-xdf2[samp,]
    xdfValidate<-xdf2[-samp,]
    xS<-lm(xdfTrain$observed~0+xdfTrain$predicted)$coefficients
    xSlope[i]<-xS
    xCorv[i]<-cor(xdfValidate$predicted*xS,xdfValidate$observed)
    xRMSEv[i]<-sqrt(mean(((xdfValidate$predicted*xS)-xdfValidate$observed)^2))
    xCora[i]<-cor(xdf2$predicted*xS,xdf2$observed)
    xRMSEa[i]<-sqrt(mean(((xdf2$predicted*xS)-xdf2$observed)^2))
  }
  
  return(list(cf=median(xSlope),corv=mean(xCorv),RMSEv=mean(xRMSEv),cora=mean(xCora),
              RMSEa=mean(xRMSEa),n=nrow(xdf2)))
}