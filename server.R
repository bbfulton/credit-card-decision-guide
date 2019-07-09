require(shiny)
require(ggplot2)
require(tidyverse)
require(ggthemes)

# wd <- "C:/Users/Bryan/Google Drive/Credit Card Comparison/"
fileName <- "ccinfo.csv"
# ccdata <- read.csv(paste(wd, fileName, sep = ""))
ccdata <- read.csv(fileName)

shinyServer(
   function(input, output) {
      cc <- eventReactive(input$button, {
            expenseMethod = input$ExpenseMethod
            yearlyGroceries = input$Groceries
            yearlyFuel = input$Fuel
            yearlyFlight = input$Flights
            yearlyUber = input$Uber
            yearlyOtherTravel = input$OtherTravel
            yearlyEntertainment = input$Entertainment
            yearlyInternational = input$International
            yearlyMiscellaneous = input$Miscellaneous
            yearlyMajorRetail = input$MajorRetailers
            yearlyDining = input$Dining
            numYears = input$Years
            globalEntry = input$Global
            maximizePointsSpent = input$MaxSpent
            maximizePointsEarned = input$MaxEarned
            globalEntry <- ifelse(globalEntry == TRUE, 1, 0)
            maximizePointsEarned <- ifelse(maximizePointsEarned == TRUE, 1, 0)
            maximizePointsSpent <- ifelse(maximizePointsSpent == TRUE, 1, 0)
            
            cm <- case_when(expenseMethod == "Yearly" ~ 1,
                            expenseMethod == "Monthly" ~ 12,
                            expenseMethod == "Weekly" ~ 52)
            
            yearlyGroceries <- yearlyGroceries * cm
            yearlyFuel <- yearlyFuel * cm
            yearlyFlight <- yearlyFlight * cm
            yearlyUber <- yearlyUber * cm
            yearlyOtherTravel <- yearlyOtherTravel * cm
            yearlyEntertainment <- yearlyEntertainment * cm
            yearlyInternational <- yearlyInternational * cm
            yearlyMajorRetail <- yearlyMajorRetail * cm
            yearlyMiscellaneous <- yearlyMiscellaneous * cm
            yearlyDining <- yearlyDining * cm
            
            ccdata$BonusEarned <- ifelse(sum(yearlyGroceries,
                                             yearlyFuel,
                                             yearlyFlight,
                                             yearlyOtherTravel,
                                             yearlyDining,
                                             yearlyEntertainment,
                                             yearlyMajorRetail,
                                             yearlyUber,
                                             yearlyMiscellaneous)/4 > ccdata$BonusExpenditure, 1, 0)
            
            ccdata <- ccdata %>% mutate(RewardMultiplier = case_when(maximizePointsSpent == 1 ~ RewardMultiplier,
                                                                     maximizePointsSpent == 0 ~ 1))
            
            ccdata$SignupBonusCash[ccdata$CardName == "Discover IT Cash"] <- 0.0225*(sum(yearlyGroceries, yearlyDining, yearlyFuel, yearlyMajorRetail)) +
                  0.0100*(sum(yearlyFlight, yearlyEntertainment, yearlyMiscellaneous, yearlyOtherTravel, yearlyUber))
            
            ccdata$SignupBonusCash[ccdata$CardName == "Alliant Visa Signature"] <- 0.0050*(sum(yearlyGroceries, yearlyFlight,
                                                                                           yearlyDining, yearlyEntertainment,
                                                                                           yearlyFuel, yearlyMiscellaneous,
                                                                                           yearlyMajorRetail, yearlyOtherTravel,
                                                                                           yearlyUber))
            ccdata$SignupBonusCash[ccdata$CardName == "Chase Freedom Unlimited"] <- min(0.0150*(sum(yearlyGroceries, yearlyFlight,
                                                                                                yearlyDining, yearlyEntertainment,
                                                                                                yearlyFuel, yearlyMiscellaneous,
                                                                                                yearlyMajorRetail, yearlyOtherTravel,
                                                                                                yearlyUber)), 300)
            ccdata$SignupBonusCash[ccdata$CardName == "HSBC Cash Rewards Mastercard"] <- min(0.0150*(sum(yearlyGroceries, yearlyFlight,
                                                                                                     yearlyDining, yearlyEntertainment,
                                                                                                     yearlyFuel, yearlyMiscellaneous,
                                                                                                     yearlyMajorRetail, yearlyOtherTravel,
                                                                                                     yearlyUber)), 300)
            ccdata$SignupBonusPoints[ccdata$CardName == "HSBC Platinum Mastercard"] <- min(2*sum(yearlyGroceries, yearlyFlight,
                                                                                             yearlyDining, yearlyEntertainment,
                                                                                             yearlyFuel, yearlyMiscellaneous,
                                                                                             yearlyMajorRetail, yearlyOtherTravel,
                                                                                             yearlyUber), 30000)
            
            waived <- c("Capital One Venture Rewards", "Capital One Savor Cash Rewards", "Capital One Spark Business Cash",
                        "Capital One Spark Business Miles", "Citi Premier Card", "Alliant Visa Signature", 
                        "Barclay Arrival Plus")
            ccdata$MiscCreditYear[ccdata$CardName %in% waived] <- ccdata$YearlyFee[ccdata$CardName %in% waived]/numYears
            ccdata$MiscCreditYear[ccdata$CardName == "American Express Platinum"] <- min(yearlyUber/12, 15)*12 +
                  ccdata$MiscCreditYear[ccdata$CardName == "American Express Platinum"]
            ccdata$MiscCreditYear[ccdata$CardName == "HSBC Premier World Elite Mastercard"] <- min(yearlyUber, 100) +
                    ccdata$MiscCreditYear[ccdata$CardName == "HSBC Premier World Elite Mastercard"]
            ccdata$MiscCreditYear[ccdata$CardName == "HSBC Premier World Mastercard"] <- min(yearlyUber, 50) +
                    ccdata$MiscCreditYear[ccdata$CardName == "HSBC Premier World Mastercard"]
            ccdata$MiscCreditYear[ccdata$CardName == "Uber Visa"] <- min(yearlyUber, 50) +
                    ccdata$MiscCreditYear[ccdata$CardName == "Uber Visa"]
            
            ###
            
            results <- as.data.frame(matrix(data = NA, nrow = nrow(ccdata), ncol = 8))
            names(results) <- c("CardName", "PointsEarned", "ExpectedBonus", "GlobalEntry", "Credits", "Costs", "BonusQualify", "OverallValue")
            
            results$CardName <- ccdata$CardName
            results$BonusQualify <- as.factor(ccdata$BonusEarned)
            results$RewardMultiplier <- ccdata$RewardMultiplier
            
            totalCosts <- function(fee, intlrate, intlexpense) {
                  fee + (intlrate/100*intlexpense)
            }
            results$Costs <- totalCosts(ccdata$YearlyFee, ccdata$ForeignTransFee, yearlyInternational)

                        expectedBonusValue <- function(BonusEarned, BonusCash, BonusPoints, Years, CardType) {
                  case_when(CardType == "Cashback" ~ BonusEarned*BonusCash/Years,
                            CardType == "Travel" ~ BonusEarned*BonusPoints/100/Years)
            }
            results$ExpectedBonus <- expectedBonusValue(ccdata$BonusEarned,
                                                        ccdata$SignupBonusCash,
                                                        ccdata$SignupBonusPoints,
                                                        numYears, ccdata$CardType)

            earnedPoints <- function(PGroceries, PFuel, PFlight, PTravel, PDining, PEntertainment, PRetail, PGeneral,
                                     Groceries, Fuel, Flight, Uber, Travel, Dining, Entertainment, Retail, General,
                                     TravelCredit, Multiplier) {
                         Multiplier*(Groceries*PGroceries + Fuel*PFuel + max(Flight-TravelCredit, 0)*PFlight + Travel*PTravel +
                         Dining*PDining + Entertainment*PEntertainment + Retail*PRetail + General*PGeneral + Uber*PTravel)/100
            }
            if (maximizePointsEarned == 0) {
                results$PointsEarned <- earnedPoints(ccdata$StdGroceries, ccdata$StdFuel, ccdata$StdFlight, ccdata$StdTravel, ccdata$StdDining,
                                                     ccdata$StdEntertainment, ccdata$StdMajor, ccdata$StdOther, yearlyGroceries, yearlyFuel,
                                                     yearlyFlight, yearlyUber, yearlyOtherTravel, yearlyDining, yearlyEntertainment, yearlyMajorRetail,
                                                     yearlyMiscellaneous, ccdata$TravelCreditYear, ccdata$RewardMultiplier)
            }
            if (maximizePointsEarned == 1) {
                results$PointsEarned <- earnedPoints(ccdata$MaxGroceries, ccdata$MaxFuel, ccdata$MaxFlight, ccdata$MaxTravel, ccdata$MaxDining,
                                                     ccdata$MaxEntertainment, ccdata$MaxMajor, ccdata$MaxOther, yearlyGroceries, yearlyFuel,
                                                     yearlyFlight, yearlyUber, yearlyOtherTravel, yearlyDining, yearlyEntertainment, yearlyMajorRetail,
                                                     yearlyMiscellaneous, ccdata$TravelCreditYear, ccdata$RewardMultiplier)                    
            }
            results$PointsEarned[results$CardName == "Uber Visa"] <- results$PointsEarned[results$CardName == "Uber Visa"] - 
                    ccdata$RewardMultiplier[ccdata$CardName == "Uber Visa"]*yearlyUber*ccdata$StdTravel[ccdata$CardName == "Uber Visa"]/100/3

            credits <- function(travelcredit, travelspent, misccredit) {
                  case_when(travelcredit >= travelspent ~ travelspent + misccredit,
                            travelcredit <= travelspent ~ travelcredit + misccredit)
            }
            results$Credits <- credits(ccdata$TravelCreditYear, yearlyFlight, ccdata$MiscCreditYear)

            ge <- function(gecredit, ge, years) {
                  gecredit*ge/min(5,years)
            }
            results$GlobalEntry <- ge(ccdata$GlobalTSA, globalEntry, numYears)

            yearlyValue <- function(points, bonus, global, credit, cost, numYears) {
                  round((numYears*points + bonus + numYears*global + numYears*credit - numYears*cost)/numYears, 0)
            }
            
            results$OverallValue <- yearlyValue(results$PointsEarned, results$ExpectedBonus, results$GlobalEntry, results$Credits, results$Costs, numYears)
            results <- results[order(results$OverallValue, decreasing = TRUE),]
            numResults <- 10
            results <- head(results, numResults)
            p <- ggplot(data = results, aes(x = reorder(CardName, OverallValue), y = OverallValue, fill = OverallValue, alpha = as.numeric(BonusQualify)))
            p <- p + scale_alpha_continuous(range = c(0.25,1))
            p <- p + geom_bar(stat = "identity", width = 0.75)
            p <- p + scale_fill_gradient(low="turquoise3", high ="green3")
            p <- p + theme(axis.text.y = element_text(size = min(15,300/numResults)),
                           axis.text.x = element_text(size = 15),
                           axis.title.y = element_text(size = 15))
            p <- p + labs(x = "", 
                          y = "Value Per Year",
                          caption = "Translucent bars represent cards for which your indicated expenditures do NOT qualify you for their bonus")
            p <- p + scale_y_continuous(labels = function(x) paste0("$", x))
            p <- p + theme(plot.caption = element_text(size = 12, face = "italic"), axis.title = element_text(size = 15))
            p <- p + theme(legend.title = element_blank(), legend.position = "none")
            p <- p + coord_flip()
            return(list(ccdata, results, p))

      })
            
            output$resultsplot = renderPlot(cc()[[3]])
            output$ccdatatable = renderTable(cc()[[1]])

})


