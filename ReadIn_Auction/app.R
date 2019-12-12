# Shiny Web App for resolving the Day Ahead Drayage Auction

# Load Libraries
library(shiny)
library(DT)
library(lpSolveAPI)
library(tidyverse)
library(readxl)

# "/home/sean/pCloudDrive/Data/MergedDrayExample.xlsx" -- Data
# "/home/sean/Downloads/Optimizer test bed 111819.xlsx"
Starting_Data <- read_excel("/home/sean/pCloudDrive/Data/MergedDrayExample.xlsx") %>%
    mutate(Ceiling_Price = as.numeric(Ceiling_Price),
           Bid_Price = as.numeric(Bid_Price)) %>% 
    mutate(CP_Delta = Ceiling_Price - Bid_Price) %>% 
    na.omit()

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Day-Ahead Auction"),

    # Sidebar with options and Run the Auction button to start the optimization 
    sidebarLayout(
        sidebarPanel(
            actionButton("AuctionRun", "Run the Auction"),
            numericInput("weight", "Weight Parameter",
                         value = 0.1,
                         min = 0,
                         max = 1,
                         step = 0.15,
                         width = '50%'),
            checkboxInput("CarrierRes", "Show Results by Carrier", value = F, width = '50%'),
            width = 2
        ),
        
        # Show tables
        mainPanel(
           h3('Input Data'),
           dataTableOutput("BidTable"),
           h3('Results Summary'),
           dataTableOutput("SummaryTable"),
           h3('Results Data'),
           dataTableOutput("ResultsTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Stops the app when the browser window closes
    session$onSessionEnded(stopApp)
    
    # Prepare the Input Data for viewing
    output$BidTable <- renderDataTable({
        Starting_Data
    })
    
    # Run the LPsolve optimization algorithm and collect results
    Ending_Data <- eventReactive(input$AuctionRun, {
        
        numberLanes <- n_distinct(Starting_Data$Lane)
        numCarriers <- n_distinct(Starting_Data$Carrier)
        
        numBid_LoadCombinations <-  nrow(Starting_Data)
        numLoads <- n_distinct(Starting_Data$LoadID) # Need to change these in cases of multiple lanes.  Look at Drayage_SingleLane_DayAheadAuction.RMD
        numBids <- n_distinct(Starting_Data$BidID)
        
        lprec <- NULL
        
        lprec <- make.lp(0, numBid_LoadCombinations+numBids)
        
        
        # Objective function
        set.objfn(lprec, 
                  obj = c(input$weight*Starting_Data$CP_Delta,
                          distinct(Starting_Data, Carrier, numLoadsBid)$numLoadsBid))
        
        # Constraints
        
        # loads
        # Each Load can only be used 1 time
        for (load in seq(1, numLoads)) {
            add.constraint(lprec,
                           xt = rep(x = 1,
                                    times = sum(Starting_Data$LoadID == load)),
                           indices = which(Starting_Data$LoadID == load),
                           "<=", rhs = 1)
        }
        
        # global sum of loads used must be <= total number of loads available in the lane (check this twice, once from each load x bid combo and once from the numLoadsBid sum)
        add.constraint(lprec,
                       xt = rep(x = 1,
                                times = numBid_LoadCombinations),
                       indices = 1:numBid_LoadCombinations,
                       "<=", rhs = numLoads)
        
        add.constraint(lprec,
                       xt = distinct(Starting_Data, Carrier, numLoadsBid)$numLoadsBid,
                       indices = (numBid_LoadCombinations+1):(numBid_LoadCombinations+numBids),
                       "<=", rhs = numLoads)
        
        # carriers
        # only 1 bid per carrier
        for (carrier in seq(1, numCarriers)) {
            add.constraint(lprec,
                           xt = rep(x = 1,
                                    times = sum(distinct(Starting_Data, Carrier, BidID, numLoadsBid)$Carrier == unique(Starting_Data$Carrier)[[carrier]])),
                           indices = numBid_LoadCombinations + which(distinct(Starting_Data, Carrier, numLoadsBid)$Carrier == unique(distinct(Starting_Data, Carrier, numLoadsBid)$Carrier)[[carrier]]),
                           "<=", rhs = 1)
        }
        
        # bid amounts == loads carried
        for (bid in seq(1, numBids)) {
            add.constraint(lprec,
                           xt = 1,
                           indices = numBid_LoadCombinations+bid,
                           "<=", rhs = 1)
            
            add.constraint(lprec,
                           xt = c(rep(x=(1/distinct(Starting_Data, Carrier, BidID, numLoadsBid)$numLoadsBid[[bid]]), times = sum(Starting_Data$BidID == bid)),
                                  -1),
                           indices = c(which(Starting_Data$BidID == bid), numBid_LoadCombinations+bid),
                           "=", rhs = 0)
            
            add.constraint(lprec,
                           xt = c(rep(x=1, times = sum(Starting_Data$BidID == bid)),
                                  -distinct(Starting_Data, Carrier, BidID, numLoadsBid)$numLoadsBid[[bid]]),
                           indices = c(which(Starting_Data$BidID == bid),numBid_LoadCombinations+bid),
                           "=", rhs = 0)
        }
        
        
        #Make decision variables binary and integer semi-continuous
        set.type(lprec, columns = 1:(numBid_LoadCombinations+numBids), type = "binary") # set all decision vars to binary
        
        # Set to maximize or minimize depending on maxNumLoads_setting
        lp.control(lprec, sense='max') 
        
        # Solve the model
        solve(lprec)
        
        Starting_Data$UseBid <- get.variables(lprec)[1:numBid_LoadCombinations]
        
        Starting_Data
    })
    
    # Prepare the Results Data for viewing
    observeEvent(input$AuctionRun, {
    output$ResultsTable <- renderDataTable({
        Ending_Data() %>% 
            filter(UseBid == 1)
    })
    })
    
    # Prepare summary statistics for viewing
    observeEvent(input$AuctionRun, {
        output$SummaryTable <- renderDataTable({
            
            if(input$CarrierRes == T){
                Ending_Data() %>%
                    filter(UseBid == 1) %>%
                    group_by(Carrier) %>% 
                    summarise(
                        `Loads Assigned` = n(),
                        `Sum Ceiling Price` = sum(Ceiling_Price),
                        `Total Cost` = sum(Bid_Price),
                        `Total Savings` = sum(CP_Delta))
                
            } else {
                
            Ending_Data() %>%
                filter(UseBid == 1) %>%
                summarise(
                    `Loads Assigned` = n(),
                    `Sum Ceiling Price` = sum(Ceiling_Price),
                    `Total Cost` = sum(Bid_Price),
                    `Total Savings` = sum(CP_Delta))
            }
        })
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
