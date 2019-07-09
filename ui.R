require(shiny)
require(shinythemes)
require(shinydashboard)

shinyUI(fluidPage(
      theme = shinytheme("sandstone"),
      titlePanel("Credit Card Decision Guide"),
      sidebarLayout(
            sidebarPanel(width = 4,
                         div(style="display: inline-block;vertical-align:moddle; width: 145px;",
                             h4("Please enter your")),
                         div(style="display: inline-block;vertical-align:middle; width: 100px;",
                             selectInput("ExpenseMethod","",c("Weekly","Monthly", "Yearly"))),
                         div(style="display: inline-block;vertical-align:moddle; width: 160px;",
                             h4("expenditures below:")),
                         # h4("Please enter your MONTHLY expenditures below:"),
                         # h5("(Initial Values Represent Average Household Estimates)"),
                     box(width = 15, title = "", 
                         splitLayout(
                                 numericInput("Groceries", label = "Grocery Stores", value = "0", min = 0, max = 25000),
                                 numericInput("Fuel", label = "Gas Stations", value = "0", min = 0, max = 25000)
                         )
                     ),
                     box(width = 15, title = "", 
                         splitLayout(
                                 numericInput("Flights", label = "Airline Tickets", value = "0", min = 0, max = 25000),
                                 numericInput("Uber", label = "Uber", value = "0", min = 0, max = 25000)
                         )
                     ),
                     box(width = 15, title = "", 
                         splitLayout(
                                 numericInput("OtherTravel", label = "All Other Travel", value = "0", min = 0, max = 25000),
                                 numericInput("Dining", label = "Restaurants", value = "0", min = 0, max = 25000)
                         )
                     ),
                     box(width = 15, title = "", 
                         splitLayout(
                                 numericInput("MajorRetailers", label = "Amazon", value = "0", min = 0, max = 25000),
                                 numericInput("Entertainment", label = "Entertainment", value = "0", min = 0, max = 25000)
                         )
                     ),
                     box(width = 15, title = "", 
                         splitLayout(
                                 numericInput("International", label = "International", value = "0", min = 0, max = 25000),
                                 numericInput("Miscellaneous", label = "Everything Else", value = "0", min = 0, max = 25000)
                         )
                     ),
                checkboxInput("Global", label = "Do you value Global Entry credits?", value = FALSE),
                checkboxInput("MaxEarned", label = "Maximize points EARNED by purchasing thru specific portals?", value = FALSE),
                checkboxInput("MaxSpent", label = "Maximize points SPENT by redeeming thru specific portals?", value = FALSE),
                numericInput("Years", label = "Number of years you plan on using this card", value = "5", min = 0, max = 25000),

                actionButton("button", "GO")
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Results", plotOutput("resultsplot")),
                    tabPanel("General Info",
                             h3("Who is this application designed for?"),
                             p("This application is designed with a specific user in mind.  The credit cards recommended here are for people with
                               either good or excellent credit who are looking for credit cards that provide either cashback or travel point rewards
                               and are in the position to quickly pay off the balance that they accrue.  These are not optimized for people who are 
                               looking to build their credit, to transfer balances, or to make purchases they won't be able to immediately pay off.  
                               If you fall into any of these categories, then there are other credit cards that will suit you better."),
                             h3("What are the limitations of this application?"),
                             p("There are probably hundreds of different types of rewards cards available today, and there are a vast number 
                               of different types of bonuses and incentives that these cards provide (some of which are very specific or nuanced).
                               This application is designed to be of general use and does not try to capture every possible combination of user type
                               and reward.  As such, most (though not all) brand/reward specific cards are not included in this application.  That
                               includes such cards as the Southwest Airline Rapid Rewards, American Express Delta or Hilton cards, and so on.  If
                               your credit card expenditures are heavily weighted to specific companies, then those cards should be considered.
                               Uber and Amazon have been included as they constitute a notable part of yearly expenditures for a very large number
                               of people."),
                             p("This application also makes no attempt to determine the ease or rate of rewards transfer."),
                             h3("Are luxury cards included in this application?"),
                             p("While a few of the cards give exclusive benefits to cardholders, all of them provide excellent, quantifiable rewards
                               that they provide that they offer.  Aspects such as luxury, exclusivity, level of service, and so on, are difficult
                               to quantify in a general way, so cards that specialize in those features are not included.")),
                    tabPanel("Assumptions",
                             h3("Travel is for travel; cashback is for cashback"),
                             p("The calculations made by this application are made with the assumption that rewards from travel cards will be used
                               exclusively for travel redemptions.  While many travel rewards cards do allow users to redeem points for credit 
                               statements, the redemption rate is generally not as favorable as it is when redeeming for travel related purchases.
                               If you believe that there is a reasonable chance that you will be in the position where you will need to use
                               your travel points for a credit statement, then it's likely that a cashback card will be of greater value."),
                             h3("The Values you enter are not how much you spend..."),
                             p("...But rather, how much you CHARGE to your credit card.  This might seem obvious, but it's also easy to overlook.
                               Also....YOU DON'T EARN REWARDS POINTS WHEN YOU PAY WITH REWARDS POINTS.  The calculations do not deduct the points 
                               that it projects that you will earn from the amount you indicate that you spend."),
                             h3("That you actually have good credit"),
                             p("All of the cards on this list require at least a good credit score to obtain.  Some require an excellent credit score.
                               In no way, shape, or form does this app imply that you can actually get the cards that it's recommending.  That's for 
                               you to determine."),
                             h3("You live in the United States"),
                             p("While I'm sure that many of the credit cards in this comparison are available all over the world with similar bonuses
                               and incentives, the calculations are all based off of US card data.")),
                    # tabPanel("Estimating Expenditures",
                    #          h3("How to accurately estimate your expenditures"),
                    #          h4("Grocery Stores"),
                    #          p("As you might guess, this is how much you spend per year at grocery stores.  Generally speaking, this does NOT include
                    #            groceries purchased at places like Walmart or local convenience stores.  An easy way to estimate your yearly expenditures
                    #            is to think about what you spend on a weekly basis (which is generally easier when it comes to groceries) and then multiply
                    #            that amount by 52 (or even 50, if that's easier)."),
                    #          h4("Gas Stations"),
                    #          p("Just like with groceries, estimate what you spend in gas per week and mulitply it by 52.  If you're lucky and don't have
                    #            to drive very often, then take your monthly cost and multiply by 12."),
                    #          h4("Restaurants"),
                    #          p("How many times do you go out to eat per week?  And what is your average tab?  Multiply those numbers together and multiply 
                    #            the result by 52.  How many times per year do you splurge for special occasions?  And what do those evenings usually cost?  
                    #            Be sure to add those splurges in to your total."),
                    #          h4("Airline Tickets/Other Travel"),
                    #          p("This should be fairly straightforward.  'Other Travel' includes purchases such as hotels, buses, cruises, and even some
                    #            local transportation costs.  It technically also includes Uber and Lyft, but do not include Uber expenditures in this tally
                    #            since Uber has its own category."),
                    #          h4("Uber/Amazon"),
                    #          p("Both of these companies make yearly spending reports readily available to their clients.  A couple of Google searches and
                    #            a few minutes of your time and you'll have the exact numbers."),
                    #          h4("Entertainment"),
                    #          p("This includes anything from movies, sporting events, concerts, etc.  Again, it may help to break this down into a weekly
                    #            or monthly figure and multiply that by 52 or 12 to get a yearly estimate."),
                    #          h4("International"),
                    #          p("Pretty straightforward. It's fairly easy for most people to calculate international expenditures, unless of course you
                    #            happen to travel internationally often."),
                    #          h4("Everything Else"),
                    #          p("This is a catch-all category that includes everything not listed above.  It ranges from monthly bills to car repairs to
                    #            online shopping.  It might be easier to determine how much you charge to your credit card in total per year and then 
                    #            deduct the figures listed above.  Think of how much money you earn in one month after taxes and savings?  
                    #            Deduct your mortgage/rent from that figure and then multiply it by 12.  Deduct your grocery, fuel, travel, dining, etc.,
                    #            expenses from that amount, and you'll have a reasonable estimate of how much you're spending on everything else.")
                    #          ),
                    tabPanel("Lingo",
                            h3("What are Global Entry credits?"),
                            p("Have you ever seen those folks that are able to walk right past the security lines in the airport and go through their 
                              own security checkpoint?  Or the people who don't have to wait in line at US Customs when arriving home from an 
                              international flight?  Those people are enrolled in the Global Entry or TSA precheck programs.  Enrolling in these 
                              programs cost $100/$85 every 5 years.  Some rewards cards will reimburse you for this cost if you charge it to your
                              credit card.  If this is something that you already pay for or if it's a convenience that you value, then check the box."),
                            h3("What does it mean to buy or redeem through certain portals?"),
                            p("Certain cards provide only modest point value earnings on most purchases, but when you buy from websites or apps that are 
                              affiliated with that card, the rewards multipliers substantially increase.  The American Express Platinum card is one 
                              such example.  While it normally only earns 1 point per dollar spent on flights on Expedia.com, Travelocity, etc., 
                              it will earn 5 points per dollar when you either buy from the airline directly or from American Express's own purchasing
                              site."),
                            p("Likewise, some cards provided bonuses when you REDEEM your points through certain platforms.  The Chase Sapphire Reserve 
                              card provides a 50% points bonus when you redeem your points through their affiliated redemption network.")
                            ),
                    # tabPanel("Table", tableOutput("resultstable")),
                    tabPanel("Credit Card Data", tableOutput("ccdatatable")))
      )      
)
)
)
#


