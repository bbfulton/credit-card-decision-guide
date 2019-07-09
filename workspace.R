require(tidyverse)

wd <- "C:/Users/Bryan/Google Drive/Credit Card Comparison/"
fileName <- "creditcardrewards.csv"

# yearlyGroceries <- 3500
# yearlyFuel <- 2500
# yearlyFlight <- 3000
# yearlyOtherTravel <- 1500
# yearlyDining <- 3500
# yearlyEntertainment <- 1200
# yearlyMajorRetail <- 1200
# yearlyMiscellaneous <- 5000
# numYears <- 4
# yearlyInternational <- 500
# maximizePointsSpent <- 1
# maximizePointsEarned <- 1
# GlobalEntry <- 1

data <- read.csv(paste(wd, fileName, sep = ""), header = TRUE, stringsAsFactors = FALSE)
data$CardType <- as.factor(data$CardType)

data$BonusEarned <- ifelse(sum(yearlyGroceries,
                               yearlyFuel,
                               yearlyFlight,
                               yearlyOtherTravel,
                               yearlyDining,
                               yearlyEntertainment,
                               yearlyMajorRetail,
                               yearlyMiscellaneous)/4 > data$BonusExpenditure, 1, 0)

data$SignupBonusCash[data$CardName == "Discover IT Cash"] <- 0.0225*(sum(yearlyGroceries, yearlyDining, yearlyFuel, yearlyMajorRetail)) +
                                                             0.0100*(sum(yearlyFlight, yearlyEntertainment, yearlyMiscellaneous, yearlyOtherTravel))
data$SignupBonusCash[data$CardName == "Alliant Visa Signature"] <- 0.0050*(sum(yearlyGroceries, yearlyFlight,
                                                                               yearlyDining, yearlyEntertainment,
                                                                               yearlyFuel, yearlyMiscellaneous,
                                                                               yearlyMajorRetail, yearlyOtherTravel))
data$SignupBonusCash[data$CardName == "Chase Freedom Unlimited"] <- min(0.0150*(sum(yearlyGroceries, yearlyFlight,
                                                                                    yearlyDining, yearlyEntertainment,
                                                                                    yearlyFuel, yearlyMiscellaneous,
                                                                                    yearlyMajorRetail, yearlyOtherTravel)), 300)
data$SignupBonusCash[data$CardName == "HSBC Cash Rewards Mastercard"] <- min(0.0150*(sum(yearlyGroceries, yearlyFlight,
                                                                                         yearlyDining, yearlyEntertainment,
                                                                                         yearlyFuel, yearlyMiscellaneous,
                                                                                         yearlyMajorRetail, yearlyOtherTravel)), 300)
data$SignupBonusPoints[data$CardName == "HSBC Platinum Mastercard"] <- min(2*sum(yearlyGroceries, yearlyFlight,
                                                                                 yearlyDining, yearlyEntertainment,
                                                                                 yearlyFuel, yearlyMiscellaneous,
                                                                                 yearlyMajorRetail, yearlyOtherTravel), 30000)

waived <- c("Capital One Venture Rewards", "Capital One Savor Cash Rewards", "Capital One Spark Business Cash",
            "Capital One Spark Business Miles", "Citi Premier Card", "Alliant Visa Signature", 
            "Barclay Arrival Plus")
data$MiscCreditYear[data$CardName %in% waived] <- data$YearlyFee[data$CardName %in% waived]/numYears

# End of primary data; save image file 

results <- as.data.frame(matrix(data = NA, nrow = nrow(data), ncol = 7))
names(results) <- c("CardName", "Costs", "ExpectedBonus", "PointsEarned", "Credits", "GlobalEntry", "OverallValue")
results$CardName <- data$CardName

totalCosts <- function(fee, intlrate, intlexpense) {
      fee + (intlrate/100*intlexpense)
}
results$Costs <- totalCosts(data$YearlyFee, data$ForeignTransFee, yearlyInternational)


expectedBonusValue <- function(BonusEarned, BonusCash, BonusPoints, Years, CardType) {
        case_when(CardType == "Cashback" ~ BonusEarned*BonusCash/Years,
                  CardType == "Travel" ~ BonusEarned*BonusPoints/100/Years)
}
results$ExpectedBonus <- expectedBonusValue(data$BonusEarned, 
                                            data$SignupBonusCash, 
                                            data$SignupBonusPoints,
                                            numYears, data$CardType)

earnedPoints <- function(CBGroceries, CBFuel, CBFlight, CBTravel, CBDining, CBEntertainment, CBRetail, CBGeneral,
                         BPGroceries, BPFuel, BPFlight, BPTravel, BPDining, BPEntertainment, BPRetail, BPGeneral,
                         Groceries, Fuel, Flight, Travel, Dining, Entertainment, Retail,
                         General, CardType, TravelCredit, PortalSpending, Multiplier) {
        if(CardType == "Travel") {
                ep <- (Groceries*BPGroceries + Fuel*BPFuel + max(Flight-TravelCredit, 0)*BPFlight + Travel*BPTravel + Dining*BPDining + 
                       Entertainment*BPEntertainment + Retail*BPRetail + General*BPGeneral)/100
                ep <- ifelse(PortalSpending == 1, ep*Multiplier, ep)
        }
        if(CardType == "Cashback") {
                ep <- (Groceries*CBGroceries + Fuel*CBFuel + max(Flight-TravelCredit, 0)*CBFlight + Travel*CBTravel + Dining*CBDining + 
                       Entertainment*CBEntertainment + Retail*CBRetail + General*CBGeneral)/100
        }
      return(ep)
}


for (i in 1:nrow(data)) {
        results$PointsEarned[i] <- if(maximizePointsEarned == 1) {
                                                earnedPoints(data$MaxCBGroceries[i],data$MaxCBFuel[i],data$MaxCBFlight[i], 
                                                             data$MaxCBOtherTravel[i],data$MaxCBDining[i],data$MaxCBEntertainment[i],
                                                             data$MaxCBMajor[i],data$MaxCBGeneral[i],data$MaxBPGroceries[i],
                                                             data$MaxBPFuel[i],data$MaxBPFlight[i],data$MaxBPOtherTravel[i],
                                                             data$MaxBPDining[i],data$MaxBPEntertainment[i],data$MaxBPMajor[i],
                                                             data$MaxBPGeneral[i],yearlyGroceries,yearlyFuel,yearlyFlight,
                                                             yearlyOtherTravel,yearlyDining,yearlyEntertainment,
                                                             yearlyMajorRetail,yearlyMiscellaneous,data$CardType[i],
                                                             data$TravelCreditYear, maximizePointsSpent, data$RewardMultiplier[i])
                                                } else {
                                                earnedPoints(data$StdCBGroceries[i],data$StdCBFuel[i],data$StdCBFlight[i], 
                                                             data$StdCBOtherTravel[i],data$StdCBDining[i],data$StdCBEntertainment[i],
                                                             data$StdCBMajor[i],data$StdCBGeneral[i],data$StdBPGroceries[i],
                                                             data$StdBPFuel[i],data$StdBPFlight[i],data$StdBPOtherTravel[i],
                                                             data$StdBPDining[i],data$StdBPEntertainment[i],data$StdBPMajor[i],
                                                             data$StdBPGeneral[i],yearlyGroceries,yearlyFuel,yearlyFlight,
                                                             yearlyOtherTravel,yearlyDining,yearlyEntertainment,
                                                             yearlyMajorRetail,yearlyMiscellaneous,data$CardType[i],
                                                             data$TravelCreditYear, MaximizePointsSpent, data$RewardMultiplier[i])
                                   }        
}

credits <- function(travelcredit, travelspent, misccredit) {
      case_when(travelcredit >= travelspent ~ travelspent + misccredit,
                travelcredit <= travelspent ~ travelcredit + misccredit)
}
results$Credits <- credits(data$TravelCreditYear, yearlyFlight, data$MiscCreditYear)

ge <- function(gecredit, ge, years) {
      gecredit*ge/min(5,years)
}
results$GlobalEntry <- ge(data$GlobalTSA, GlobalEntry, numYears)

yearlyValue <- function(points, bonus, global, credit, cost, numYears) {
      round((numYears*points + bonus + numYears*global + numYears*credit - numYears*cost)/numYears, 0)
}

results$OverallValue <- yearlyValue(results$PointsEarned, results$ExpectedBonus, results$GlobalEntry, results$Credits, results$Costs, numYears)

results <- results[order(results$OverallValue, decreasing = TRUE),]
results

