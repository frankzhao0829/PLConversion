lead_format_change <- function (lead, predictionDate) {

    lead[,c('LeadID', 'RespID', 'dwContactPK')] <- lapply(lead[,c('LeadID','RespID', 'dwContactPK')], as.factor) 
    
    lead[, c('RespDttm', 'Email_Validity_Dttm')] <- lapply(lead[, c('RespDttm', 'Email_Validity_Dttm')], as.POSIXlt) 
    
    lead[,c('CampaignPK', 'CampaignBusinessGroup', 'CampaignBusinessDivision', 'CampaignBusinessUnit', 'CampaignChannel', 'CampaignTactic', 'CampaignProductLine1', 'CampaignProductLine2', 'CampaignProductLine3', 'CampaignProductLine4', 'CampaignProductLine5', 'ProductLineInterest1', 'ProductLineInterest2', 'ProductLineInterest3', 'ProductLineInterest4', 'ProductLineInterest5')] <- lapply(lead[,c('CampaignPK', 'CampaignBusinessGroup', 'CampaignBusinessDivision', 'CampaignBusinessUnit', 'CampaignChannel', 'CampaignTactic', 'CampaignProductLine1', 'CampaignProductLine2', 'CampaignProductLine3', 'CampaignProductLine4', 'CampaignProductLine5', 'ProductLineInterest1', 'ProductLineInterest2', 'ProductLineInterest3', 'ProductLineInterest4', 'ProductLineInterest5')], as.factor) 
    
    
    lead[, c('OppDateCreated', 'OppCloseDate')] <- lapply(lead[, c('OppDateCreated', 'OppCloseDate')], as.POSIXlt)
    
    lead[,c('OppStageName', 'OppStatus', 'OppOwnerGBU', 'OppCountry', 'OppCurrency')] <- lapply(lead[,c('OppStageName', 'OppStatus', 'OppOwnerGBU', 'OppCountry', 'OppCurrency')], as.factor) 
    
    lead[, c('GeneratedToEloquaSFDCDttm', 'PreviousRespDate')] <- lapply(lead[, c('GeneratedToEloquaSFDCDttm', 'PreviousRespDate')], as.POSIXlt)
    
    lead[, 'ContProdQuant1'] <- as.numeric(lead[, 'ContProdQuant1'])
    
    
    lead[, c('CompCity', 'CompStPv','CompCountry', 'ContProd1', 'RespRespType_s',  'RespRespMethod_s', 'RespPriority', 'RespShipMeth', 'RespTriggeringCd', 'RespTimeFrame', 'RespWillingToBuy',  'RespPartnerLeadShareFlg', 'RespContactReq', 'BusinessUnit_s', 'ContactedByTMFlag', 'BANTValidatedYN', 'RespTouchPoint', 'department_name', 'job_level_category', 'job_function_category', 'countryName', 'BusinessFuction', 'JobRole', 'ContRespAreaTx', 'MasterLeadFlg', 'Email_validity', 'Language', 'JobSeniority', 'JobResponsibilities', 'PurchasingRole', 'Suppress_from_Campaigns', 'SFDCAccountID', 'PreviousREspShipMeth')] <- lapply (lead[, c('CompCity','CompStPv',  'CompCountry', 'ContProd1', 'RespRespType_s',  'RespRespMethod_s', 'RespPriority', 'RespShipMeth', 'RespTriggeringCd', 'RespTimeFrame', 'RespWillingToBuy',  'RespPartnerLeadShareFlg', 'RespContactReq', 'BusinessUnit_s', 'ContactedByTMFlag', 'BANTValidatedYN', 'RespTouchPoint', 'department_name', 'job_level_category', 'job_function_category', 'countryName', 'BusinessFuction', 'JobRole', 'ContRespAreaTx', 'MasterLeadFlg', 'Email_validity', 'Language', 'JobSeniority', 'JobResponsibilities', 'PurchasingRole', 'Suppress_from_Campaigns', 'SFDCAccountID',  'PreviousREspShipMeth')], as.factor)
    
    lead[, c('Tactic', 'PreviousTactic')] <- lapply(lead[, c('Tactic', 'PreviousTactic')], as.factor)
     
    
    
    ##### add additional attributes
    
    
    a <- as.numeric(difftime(predictionDate, lead[,'RespDttm'], units = 'days')) 
    lead <- cbind(lead, recency = a)
    
    #summary(testsetmarch11[, 'recency'])
    
    lead[lead[,'Converted'] == 1,'recency']  <- as.numeric(difftime(lead[lead[,'Converted'] == 1, 'OppDateCreated'], lead[lead[,'Converted'] ==1, 'RespDttm'], units = 'days'))
     
    a <- is.na(lead[, 'ProductLineInterest1'])+is.na(lead[,'ProductLineInterest2'])+is.na(lead[,'ProductLineInterest3']) +is.na(lead[, 'ProductLineInterest4'])+ is.na(lead[, 'ProductLineInterest5'])
    lead <- cbind(lead, numOfProdInterests = a)
     
    a <- as.character(lead[, 'CampaignProductLine1']) == as.character (lead[, 'ProductLineInterest1'])
    lead <- cbind(lead, firstInterestMatchYN = a)
    
    return (lead)
}


lead_acc_format_change <- function (lead) {

    lead[, c('Country', 'Continent', 'SalesVolumeType', 'TotalEmployeeCountType', 'LocalEmployeeCountType', 'SmallBusinessFlag', 'PubPrivateFlag', 'LineOfBusiness', 'Recent3YearSalesTrend', 'Recent3YearEmpTrend', 'Recent5YearSalesTrend', 'Recent5YearEmpTrend', 'industry_vertical_cd', 'industry_vertical_nm', 'industry_segment_cd', 'industry_segment_nm', 'sitesubgrp', 'sitegrp', 'partner', 'sweetspot_3yrs', 'sweetspot_5yrs')] <- lapply (lead[, c('Country', 'Continent', 'SalesVolumeType', 'TotalEmployeeCountType', 'LocalEmployeeCountType', 'SmallBusinessFlag', 'PubPrivateFlag', 'LineOfBusiness', 'Recent3YearSalesTrend', 'Recent3YearEmpTrend', 'Recent5YearSalesTrend', 'Recent5YearEmpTrend', 'industry_vertical_cd', 'industry_vertical_nm', 'industry_segment_cd', 'industry_segment_nm', 'sitesubgrp', 'sitegrp', 'partner', 'sweetspot_3yrs', 'sweetspot_5yrs')], as.factor)
    
    lead[, c('first_purchase_dt', 'last_purchase_dt')] <- lapply(lead[, c('first_purchase_dt', 'last_purchase_dt')], as.POSIXlt)    #ok, let YearStart as numbers
    
    return (lead)
}


rpart.prune.0SE <- function(rpartobject) {
  cp <- with(data.frame(rpartobject$cptable),
             CP[xerror <= min(xerror)]
            )
  objectpruned <- prune(rpartobject, cp = max(cp))
  a <- rpart_nsplit(rpartobject) - rpart_nsplit(objectpruned)
  cat("rpart.prune.0SE:", a, "splits or leaf nodes removed.\n")
  return(objectpruned)
}

################################################################################
# number of splits in the rpart() tree
################################################################################
rpart_nsplit <- function(rpart_object) {
  #the last row of rpart_object$cptable, column "nsplit" is the answer:
  return(rpart_object$cptable[nrow(rpart_object$cptable), "nsplit"])
}


################################################################################
# number of leaf nodes in the rpart() tree
################################################################################
tree_nleafnodes <- function(rpart_object) {
    return(rpart_nsplit(rpart_object)+1)
}


################################################################################
# The largest leaf node and the smallest leaf node in the rpart() tree
################################################################################
tree_largest_smallest_leaf <- function(rpart_object) {
  
  a <- subset(rpart_object$frame, var=="<leaf>", select=n)
  return(list(largest=max(a), smallest=min(a)) )
}


################################################################################
# tree depth is the maximum number of levels in the tree (level 1 = root).
#################################################################################
tree_depth <- function(rpart_object) {
  largest_node_number <- max(as.integer(rownames(rpart_object$frame)))
  if (largest_node_number > 1) {
    return(ceiling(log2(largest_node_number-1)))
  } else {
    return(1)
  }#if (largest_node_number > 1)
}#tree_depth

xerror.plot <- function(rpartobject) {
  objectname <- deparse(substitute(rpartobject)) 
  with(data.frame(rpartobject$cptable), {
    plot(nsplit, xerror, type='b',
       xlab=paste(objectname,"$cptable[,'nsplit']", sep=""),
       ylab=paste(objectname,"$cptable[,'error']", sep="") )
    iminxerror <- which.min(xerror)
    points(nsplit[iminxerror],xerror[iminxerror],pch=19,col='red')
    i1se <- nsplit[xerror <= (xerror+xstd)[iminxerror]] #i1se is a range centered around iminxerror
    abline(v=c(min(i1se),max(i1se)), lty='dashed',col='grey')
  })#with
}


variables_used_in_tree <- function(rpart_object) {
  #we extract the variables from the rpart_object$frame: The first column is "var"
  a <- levels(rpart_object$frame$var)[-1] #the first level is <leaf>; so we take that out
  return(a)
}

################################################################################
# variable.importance.used.in.tree(rpart_object) returns a vector of variable
# names that are used in the construction of the tree -- sorted by importance.
################################################################################
variable.importance.used.in.tree <- function(rpart_object) {
  #we extract the variables from the rpart_object$frame: The first column is "var"
  a <- levels(rpart_object$frame$var)[-1] #the first level is <leaf>; so we take that out
  b <- rpart_object$variable.importance[a]
  return(sort(b, decreasing=TRUE))
}




ResetUnseenLevels <- function(traindatafr, testdatafr, columnnames) {
  y <- testdatafr # the output dataframe: initial value
  for (x in columnnames){
    if ((x %in% names(traindatafr)) & (x %in% names(testdatafr)) & is.factor(testdatafr[,x]) ) {
      #testlevels <- levels(testdatafr[,x])
      testactuallevels <- as.character(unique(testdatafr[,x]))
      #trainlevels <- levels(traindatafr[,x])
      trainactuallevels <- as.character(unique(traindatafr[,x]))
      #lowesttrainlevel <- (trainlevels[trainlevels %in% trainactuallevels])[1] #this gives the lowest level in traindatafr
      n <- sum(!(testactuallevels %in% trainactuallevels)) # number of levels in test but not in train
      if (n>0) {
        nn <- sum(!(as.character(testdatafr[,x]) %in% trainactuallevels)) # number of rows in test but not in train
        #next: find the most popular level
        a <- table(testdatafr[,x]) #table(as.character(testdatafr[,x]))
        imax <- which.max(a)
        mostpopular <- names(a)[imax] #as.character(testdatafr[,x][imax])
        y[,x][!(as.character(y[,x]) %in% trainactuallevels)] <- mostpopular #lowesttrainlevel #replace with the base level in the traindatafr
        #levels(y[,x])[!(testactuallevels %in% trainactuallevels)] <- lowesttrainlevel #replace with the base level in the traindatafr
        y[,x] <- factor(as.character(y[,x])) #refactor

        cat(x, "in", deparse(substitute(testdatafr)), ": ", display.units(n, "level", "levels"), "reset to", paste('"', mostpopular, '"', sep=""),
          ": ", paste('"', setdiff(testactuallevels, trainactuallevels), '"', sep="", collapse="; "), "; ", display.units(nn, "data row", "data rows"), "\n")
      }#if (n>0)
		}#if
  }#for
  return(y)
}#ResetUnseenLevels


display.units <- function(n, unit.string, units.string) {
  return(ifelse(n==1, paste("1 ", unit.string, sep=""), paste(n, " ", units.string, sep="")))
}



myPartialSum <- function(SumVec, TopNVec, TopPercentage=FALSE) {
  if (TopPercentage) {a <- TopNVec*length(SumVec)} else {a <- TopNVec}
  return(sapply(a, function(x) sum(SumVec[1:x]) ) )
}


TopSum <- function(BasisVec, SumVec, TopNVec, TopPercentage=FALSE) {
  idx <- order(BasisVec, runif(length(BasisVec)), decreasing=TRUE)
  # in the above, runif() is used to randomly break ties
  SumVecRearranged <- SumVec[idx]
  return(myPartialSum(SumVecRearranged, TopNVec, TopPercentage))
}

 
TopSum_TieShare_TopNSingle <- function(BasisVec, SumVec, TopNSingle) {
  rankvec <- rank(-BasisVec, ties.method='min')
  rankvec_decr <- sort(rankvec) # 1 is the largest
  rank_at_TopNSingle <- rankvec_decr[TopNSingle]
  a <- which(rankvec_decr==rank_at_TopNSingle)
  rank_start <- min(a) # lowest rank
  fraction_to_include <- (TopNSingle- rank_start+1)/length(a)
  stopifnot(fraction_to_include <=1 && fraction_to_include >= 0)
  # summing in 2 parts:
  sum_lower <- sum(SumVec[rankvec < rank_at_TopNSingle])
  sum_atrank <- sum(SumVec[rankvec == rank_at_TopNSingle])
  return(sum_lower + sum_atrank*fraction_to_include)
}



TopSum_TieShare_TopNVec <- function(BasisVec, SumVec, TopNVec, TopPercentage=FALSE) {
  n <- length(TopNVec)
  myTopNVec <- TopNVec
  if (TopPercentage) {
    nSumVec <- length(SumVec)
    myTopNVec <- floor(TopNVec*nSumVec)
    myTopNVec[myTopNVec < 1] <- 1 # added 2007-10-29: to guard against TopNSingle=0
  }#if
  a <- rep(0, times=n)  # initial values to 0
  for (i in 1:n)
    a[i] <- TopSum_TieShare_TopNSingle(BasisVec, SumVec, TopNSingle=myTopNVec[i])
  return(a)
}


mySimpsonArea <- function(f) {
  input.length <- length(f)

  # test if vector f contains an odd number of points
  stopifnot(input.length>2)
  stopifnot(input.length %% 2 ==1)
  stopifnot(is.numeric(f))

  # Apply Simpson's rule
  area <- f[1]+f[input.length]
  area <- area + 4*sum(f[seq(2,input.length-1,by=2)])
  area <- area + 2*sum(f[seq(3,input.length-2,by=2)])
  return(area/3)
} 


addXYlines <- function(x, y, xstart=-0.1, ystart=-0.1) {
    # vertical line:
    lines(x=c(x,x),y=c(ystart,y), lty=2, col='grey')
    # horizontal line:
    lines(x=c(xstart,x),y=c(y,y), lty=2, col='grey')
    # text on y-axis
    #text(x=0,y=y, labels=y,cex=0.8)
} 



myCaptureCurve <- function(ActualAmount, RankVectors, 
  MarkPoints = NULL, RankVectorsToMark = NULL, LegendTexts = NULL, showAUC=FALSE, ...) {

  # internal parameter: 
  ngrid <- 100
  a <- (1:ngrid)/ngrid

  sumActual <- sum(ActualAmount)

  oldpar <- par(no.readonly = TRUE) # remember the current par() setting
  par(mfrow=c(1,1))
  plot(x=c(0,a), y=c(0,TopSum(BasisVec=ActualAmount, SumVec=ActualAmount,
                     TopNVec=a, TopPercentage=TRUE)/sumActual),
       type='l', col='red', ylim=c(0,1), ...)
  abline(a=0,b=1,lty='dashed',col='grey')

  # make RankVectors a dataframe
  rankdf <- data.frame(RankVectors)
  nRankVectors <- ncol(rankdf)
  AUC <- rep(0, times=nRankVectors)

  # add lines to mark the curves
  myRankVectorsToMark <- rep(FALSE, times=nRankVectors) 
  if (!is.null(MarkPoints)) {
    # deal with the default value of RankVectorsToMark
    myRankVectorsToMark <- RankVectorsToMark
    if (is.null(RankVectorsToMark)) myRankVectorsToMark <- rep(TRUE, times=nRankVectors) # default: Mark all curves

    # Initialize YatMarkPoints
    YatMarkPoints <- matrix(0, nrow=length(MarkPoints), ncol=nRankVectors+1)

  } #if (!is.null(MarkPoints)) 


  # loop through all columns in RankVectors
  for (i in 1:nRankVectors ) {
    b <- TopSum_TieShare_TopNVec(BasisVec=rankdf[,i], SumVec=ActualAmount, TopNVec=a, TopPercentage=TRUE)
    b <- b/sumActual
    lines(x=c(0,a), y=c(0,b), col=i+2)
    AUC[i] <- mySimpsonArea(c(0,b))/ngrid

    # register YatMarkPoints
    if (!is.null(MarkPoints))
      for (j in 1:length(MarkPoints)) {
        YatMarkPoints[j,i+1] <- b[MarkPoints[j]*ngrid]
      } #for (j in 1:length(MarkPoints))

    # add XY lines
    if (myRankVectorsToMark[i]) 
      for (j in 1:length(MarkPoints)) {
        addXYlines(MarkPoints[j], b[MarkPoints[j]*ngrid]) 
        text(x=0,y=b[MarkPoints[j]*ngrid]+0.02,labels=round(b[MarkPoints[j]*ngrid],d=3),cex=0.7)  
      } #for (j in 1:length(MarkPoints))

  } #for (i in 1:nRankVectors ) 

  # AUC actual
  b <- TopSum_TieShare_TopNVec(BasisVec=ActualAmount, SumVec=ActualAmount, TopNVec=a, TopPercentage=TRUE)
  b <- b/sumActual
  AUC.actual <- mySimpsonArea(c(0,b))/ngrid
  if (!is.null(MarkPoints)) {
    for (j in 1:length(MarkPoints)) {
        # register YatMarkPoints
        YatMarkPoints[j,1] <- b[MarkPoints[j]*ngrid]

        #addXYlines(MarkPoints[j], b[MarkPoints[j]*ngrid]) 
        #text(x=0,y=b[MarkPoints[j]*ngrid]+0.02,labels=round(b[MarkPoints[j]*ngrid],d=3),cex=0.7)  
      } #for (j in 1:length(MarkPoints))
  } #if (!is.null(MarkPoints)) 


  # add legend
  myLegendTexts <- LegendTexts
  if (is.null(LegendTexts)) myLegendTexts <- names(rankdf)
  #if (is.null(LegendTexts)) 
  #  myLegendTexts <- rep("", times=nRankVectors)

  # add "Perfect hindsight", "Random pick"
  myLegendTexts <- c("Perfect hindsight", myLegendTexts, "Random pick")

  # show AUC
  if (showAUC) {
    myLegendTextsShow <- paste(myLegendTexts, " auc=", format(c(AUC.actual,AUC,0.5),digits=3))
  } else {
    myLegendTextsShow <- myLegendTexts
  } #if (showAUC)

  legend("bottomright", myLegendTextsShow,
    col=c("red",3:(nRankVectors+2),"grey"), lty=c('solid',rep('solid',times=nRankVectors),'dashed'))

  # print out the data points
  if (!is.null(MarkPoints)) {
     mk <- data.frame(MarkPoints, YatMarkPoints, MarkPoints)
     names(mk) <- c("MarkPoint", myLegendTexts)
     # show
     print(format(mk, digits=3))
  } #if (!is.null(MarkPoints)) 

  # print out the AUC
  AUCs <- c(AUC.actual,AUC,0.5)
  names(AUCs) <- myLegendTexts
  #print(round(AUCs,digits=3))

  par(oldpar) # restore the par() setting
  
  return(AUCs)
} #myCaptureCurve 