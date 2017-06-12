########################################
## PREDICT 422
## Charity Project - Part 4 (The Mailing List Problem)
##
## RankedDonorOutput.R
########################################

########################################
## Function to Generate Output by Ranked Donors
# 
# Inputs
#   numBins: number of bins to use, e.g. deciles = 10 bins
#   rankVar: variable name to use for ranking, e.g. "PDONR" or "EXAMT"
#   dataToRank: data frame that contains data to be ranked, e.g. valData
########################################
outputForRankedDonors = function(numBins,rankVar,dataToRank)
{
  rankedDonors = dataToRank[order(dataToRank[[rankVar]],decreasing=TRUE),]
  qVals = quantile(rankedDonors[[rankVar]],probs=c(0:numBins)/numBins)
  breakVals = unique(qVals)
  numBins = min(numBins,length(breakVals)-1)
  rankedDonors$Bin = rev(cut(rankedDonors[[rankVar]],breaks=breakVals,
                             include.lowest=TRUE,right=FALSE,
                             labels=as.character(1:numBins))) # Bin 1 is top decile, bin 10 is bottom decile
  donorTable = data.frame(
    Num.Mailed=unlist(by(rankedDonors,rankedDonors$Bin,nrow,simplify=FALSE)),
    Donors=unlist(by(rankedDonors$DONR == "1",rankedDonors$Bin,sum,simplify=FALSE)),
    Donations=unlist(by(rankedDonors$DAMT,rankedDonors$Bin,sum,simplify=FALSE))
  )
  donorTable$Cum.Mailed = cumsum(donorTable$Num.Mailed)
  donorTable$Cum.Donors = cumsum(donorTable$Donors)
  donorTable$Cum.Donations = cumsum(donorTable$Donations)
  
  mailingTable = data.frame(Bins.Mailed=character(length=numBins))
  mailingTable$Bins.Mailed = paste("1 thru",1:numBins)
  mailingTable$Num.Mailed = donorTable$Cum.Mailed
  mailingTable$Num.Donors = donorTable$Cum.Donors
  mailingTable$Success.Rate = mailingTable$Num.Donors / mailingTable$Num.Mailed * 100
  mailingTable$Total.Cost = 0.99 * mailingTable$Num.Mailed
  mailingTable$Total.Donations = donorTable$Cum.Donations
  mailingTable$Total.Profit = mailingTable$Total.Donations - mailingTable$Total.Cost
  mailingTable$Average.Donation = mailingTable$Total.Donations / mailingTable$Num.Donors
  
  return(list(breakVals=breakVals,Donor.Table=donorTable,Mailing.Table=mailingTable))
}
