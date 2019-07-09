require(tidyverse)

wd <- "C:/Users/Bryan/Google Drive/Credit Card Comparison/"
fileName <- "ccinfo.csv"
data <- read.csv(paste(wd, fileName, sep = ""), header = TRUE, stringsAsFactors = FALSE)
data$CardType <- as.factor(data$CardType)

yearlyGroceries <- 3500
yearlyFuel <- 2500
yearlyFlight <- 6000
yearlyUber <- 500
yearlyOtherTravel <- 3000
yearlyDining <- 3500
yearlyEntertainment <- 1200
yearlyMajorRetail <- 1200
yearlyMiscellaneous <- 5000
numYears <- 4
yearlyInternational <- 500
maximizePointsSpent <- 1
maximizePointsEarned <- 1
globalEntry <- 1

data$BonusEarned <- ifelse(sum(yearlyGroceries,
                               yearlyFuel,
                               yearlyFlight,
                               yearlyOtherTravel,
                               yearlyDining,
                               yearlyEntertainment,
                               yearlyMajorRetail,
                               yearlyUber,
                               yearlyMiscellaneous)/4 > data$BonusExpenditure, 1, 0)

data$RewardMultiplier <- ifelse(maximizePointsSpent == 1, data$RewardMultiplier, 1)


data$SignupBonusCash[data$CardName == "Discover IT Cash"] <- 0.0225*(sum(yearlyGroceries, yearlyDining, yearlyFuel, yearlyMajorRetail)) +
      0.0100*(sum(yearlyFlight, yearlyEntertainment, yearlyMiscellaneous, yearlyOtherTravel, yearlyUber))

data$SignupBonusCash[data$CardName == "Alliant Visa Signature"] <- 0.0050*(sum(yearlyGroceries, yearlyFlight,
                                                                               yearlyDining, yearlyEntertainment,
                                                                               yearlyFuel, yearlyMiscellaneous,
                                                                               yearlyMajorRetail, yearlyOtherTravel,
                                                                               yearlyUber))
data$SignupBonusCash[data$CardName == "Chase Freedom Unlimited"] <- min(0.0150*(sum(yearlyGroceries, yearlyFlight,
                                                                                    yearlyDining, yearlyEntertainment,
                                                                                    yearlyFuel, yearlyMiscellaneous,
                                                                                    yearlyMajorRetail, yearlyOtherTravel,
                                                                                    yearlyUber)), 300)
data$SignupBonusCash[data$CardName == "HSBC Cash Rewards Mastercard"] <- min(0.0150*(sum(yearlyGroceries, yearlyFlight,
                                                                                         yearlyDining, yearlyEntertainment,
                                                                                         yearlyFuel, yearlyMiscellaneous,
                                                                                         yearlyMajorRetail, yearlyOtherTravel,
                                                                                         yearlyUber)), 300)
data$SignupBonusPoints[data$CardName == "HSBC Platinum Mastercard"] <- min(2*sum(yearlyGroceries, yearlyFlight,
                                                                                 yearlyDining, yearlyEntertainment,
                                                                                 yearlyFuel, yearlyMiscellaneous,
                                                                                 yearlyMajorRetail, yearlyOtherTravel,
                                                                                 yearlyUber), 30000)

waived <- c("Capital One Venture Rewards", "Capital One Savor Cash Rewards", "Capital One Spark Business Cash",
            "Capital One Spark Business Miles", "Citi Premier Card", "Alliant Visa Signature", 
            "Barclay Arrival Plus")
data$MiscCreditYear[data$CardName %in% waived] <- data$YearlyFee[data$CardName %in% waived]/numYears
data$MiscCreditYear[data$CardName == "American Express Platinum"] <- min(yearlyUber/12, 15)*12 +
                                                                     data$MiscCreditYear[data$CardName == "American Express Platinum"]


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

earnedPoints <- function(PGroceries, PFuel, PFlight, PTravel, PDining, PEntertainment, PRetail, PGeneral,
                         Groceries, Fuel, Flight, Uber, Travel, Dining, Entertainment, Retail, General,
                         TravelCredit, PortalSpending, Multiplier) {
            ep <- (Groceries*PGroceries + Fuel*PFuel + max(Flight-TravelCredit, 0)*PFlight + Travel*PTravel + Dining*PDining + 
                         Entertainment*PEntertainment + Retail*PRetail + General*PGeneral + Uber*PTravel)/100
            ep <- ifelse(PortalSpending == 1, ep*Multiplier, ep)
            return(ep)
}


for (i in 1:nrow(data)) {
      results$PointsEarned[i] <- if(maximizePointsEarned == 1) {
            earnedPoints(data$MaxGroceries[i],data$MaxFuel[i],data$MaxFlight[i], 
                         data$MaxTravel[i],data$MaxDining[i],data$MaxEntertainment[i],
                         data$MaxMajor[i],data$MaxOther[i],yearlyGroceries,yearlyFuel,
                         yearlyFlight,yearlyUber,yearlyOtherTravel,yearlyDining,
                         yearlyEntertainment,yearlyMajorRetail,yearlyMiscellaneous,
                         data$TravelCreditYear[i], maximizePointsSpent, data$RewardMultiplier[i])
      } else {
            earnedPoints(data$StdGroceries[i],data$StdFuel[i],data$StdFlight[i], 
                         data$StdTravel[i],data$StdDining[i],data$StdEntertainment[i],
                         data$StdMajor[i],data$StdOther[i],yearlyGroceries,yearlyFuel,
                         yearlyFlight,yearlyUber,yearlyOtherTravel,yearlyDining,
                         yearlyEntertainment,yearlyMajorRetail,yearlyMiscellaneous,
                         data$TravelCreditYear[i], maximizePointsSpent, data$RewardMultiplier[i])
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
results$GlobalEntry <- ge(data$GlobalTSA, globalEntry, numYears)

yearlyValue <- function(points, bonus, global, credit, cost, numYears) {
      round((numYears*points + bonus + numYears*global + numYears*credit - numYears*cost)/numYears, 0)
}

results$OverallValue <- yearlyValue(results$PointsEarned, results$ExpectedBonus, results$GlobalEntry, results$Credits, results$Costs, numYears)

results <- results[order(results$OverallValue, decreasing = TRUE),]
results

