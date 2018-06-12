#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(fBasics)
library(ROI)
library(dygraphs)
library(ROI.plugin.quadprog)
library(PortfolioAnalytics)
library(igraph)
library(foreach)
library(DEoptim)
library(fGarch)
library(Rglpk)
library(quadprog)
library(dplyr)
library(ROI.plugin.glpk)
library(ROI.plugin.symphony)
library(pso)
library(GenSA)
library(corpcor)
library(testthat)
library(nloptr)
library(MASS)
library(robustbase)
library(shinythemes)
library(ggplot2)
library(TTR)
library(rvest)
library(tidyverse)
library(tibble)
library(lubridate)
library(plotly)
library(corrplot)

ui <- fluidPage(theme = shinytheme("lumen"),
                # Application title
                titlePanel(
                  fluidRow(
                    column(9, h1("Application for Risk and Portfolio Analytics")),
                    column(12, img(height = 105, width = 300, src='data:image/png;base64,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', align = "Right")
                    ))),
                
                # br(),
                # br(),
                # br(),
                # br(),
                # br(),
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    textInput("symbol","Enter the stock symbol",value = toupper("CL MSFT MMM WBAI AAPL WFC")),
                    dateInput("startDate","Enter the starting Date",value = "2017-01-01",format = "yyyy-mm-dd"),
                    dateInput("lastDate","Enter the Ending Date",value = "2018-01-01",format = "yyyy-mm-dd"),
                    # checkboxInput("network","Network Analysis",value = FALSE),
                    # checkboxInput("Garch","GARCH Analysis",value = FALSE),
                    # checkboxInput("var","VAR(Vectorized Auto Regression Model) Analysis",value = FALSE),
                    submitButton("Submit"),
                    p("NOTE: Only stock symbols separated by space are allowed ")
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel( #poistion="right",
                    tabsetPanel(
                      type="tab",
                      
                      tabPanel(
                        "Baisc Information",
                        h3("Price Plot"),
                        dygraphOutput("priceplot"),
                        # h3("Discrete Returns Plot"),
                        # dygraphOutput("returnsplot"),
                        # h3("Log Returns Plot"),
                        # dygraphOutput("logreturnsplot"),
                        br(),
                        br(),
                        br(),
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"), h3("Discrete Returns Plot"), h3("Log Returns Plot"))
                        ),
                        fluidRow(
                          splitLayout(cellWidths = c("50%", "50%"), dygraphOutput("returnsplot"), dygraphOutput("logreturnsplot"))
                        ),
                        br(),
                        br(),
                        br(),
                        h3("Basic Log Return Statistical Results"),
                        verbatimTextOutput("bstat_log"),
                        h3("Basic Discrete Return Statistical Results"),
                        verbatimTextOutput("bstat"),
                        br(),
                        br(),
                        br(),
                        br(),
                        h6("References"),
                        p("The application has solely been created for academic purposes only.")
                      ),
                      tabPanel(
                        "Portfolio Analytics",
                        h2("Portfolio Weights"),         
                        plotOutput("plot2"), 
                        h4("Optimised weights for the given stocks in percenatges are"),
                        verbatimTextOutput("wt"),
                        h4("Portfolio Statistics"),
                        verbatimTextOutput("tab"),
                        h3("Efficient Frontier analysis"),
                        plotOutput("deg"),
                        h3("Summary of Efficient Frontier"),
                        verbatimTextOutput("deg_s"),
                        br(),
                        br(),
                        h3("Sensititvity of Portfolio Weights in Charts"),
                        plotOutput("deg_w"),
                        h2("Correlations in the Stocks (Matrix)"),         
                        verbatimTextOutput("cor"),
                        plotOutput("sp_plt2"),
                        h2("Interaction Network of the stocks"),
                        plotOutput("sna"),
                        h3("How to Evaluate this Graph?"),
                        h5("This graph is based on the Legend that is provided with this. The color coding of the graph explains the kind of relationship is present in the stock securities (See the Legend Table). The thickness of the edges are defined on the bases of the correlation of the securities also this is further divided into tranches i.e. the high thickness with green indicates that is more close to the 40% but less than 40 %."),
                        h5("The color RED indicates that the stocks have a negative correlations"),
                        h5("Here the Stock Numbers represent the order in which they are indicated in the Correlation Table"),
                        h3("Risk versus Rewards plot of Stocks"),
                        plotlyOutput("sp_plt1"),
                        br(),
                        br(),
                        br(),
                        h6("References"),
                        p("The application has solely been created for academic purposes only.")
                      ),
                      tabPanel(
                        "Risk Analytics",
                        h2("Value at Risk at 95% Probability"),
                        verbatimTextOutput("Value95"),
                        br(),
                        br(),
                        h2("Value at Risk at 99% Probability"),
                        verbatimTextOutput("Value99"),
                        br(),
                        br(),
                        h2("Conditional VaR at 95% Probability (Expected Shortfall)"),
                        verbatimTextOutput("CValue95"),
                        br(),
                        br(),
                        h2("Conditional VaR at 99% Probability (Expected Shortfall)"),
                        verbatimTextOutput("CValue99"),
                        br(),
                        br(),
                        h2("Incremental VaR at 95%"),         
                        verbatimTextOutput("iValue95"),
                        br(),
                        br(),
                        h2("Incremental VaR at 99%"),         
                        verbatimTextOutput("iValue99"),
                        br(),
                        br(),
                        br(),
                        br(),
                        h2("Overall Downside Risk of the Assets"),         
                        verbatimTextOutput("dr"),
                        br(),
                        br(),
                        br(),
                        br(),
                        h6("References"),
                        p("The application has solely been created for academic purposes only.")
                      ),
                      tabPanel(
                        "Monte Carlo",
                        plotOutput("mcplot1"),
                        plotOutput("mcplot2"),
                        plotOutput("mcplot3"),
                        plotOutput("mcplot4"),
                        plotOutput("mcplot5"),
                        plotOutput("mcplot6")
                      )
                    )
                  )
                ),
                hr(),
                br()
)



# Define server logic required to draw a histogram
server <- function(input, output) ({
  df <- reactive({
    #splitting the input
    syms <- unlist(strsplit(input$symbol, " "))
    #getting stocks with 10 year data and subsetting for close prices 
    Stocks = lapply(syms, function(sym) {
      na.omit(getSymbols(sym,auto.assign=FALSE,src="yahoo", from = input$startDate, to = input$lastDate)[,4])
    })
    
    #removing na's for stocks which dont have 10 yr data
    x<-do.call(cbind, Stocks)
    df<-x[complete.cases(x), ]
    
    #changing colnames of df
    for(name in names(df)){
      colnames(df)[colnames(df)==name] <- strsplit(name,"\\.")[[1]][1]}
    
    return(df)
    #returning df withclosing prices of stocks
    
  })
  
  returns_log<-reactive({
    return(Return.calculate(df(),method = c("log"))[-1])
  })
  returns<-reactive({
    return(Return.calculate(df(),method = c("discrete"))[-1])
  })
  
  
  bstats <- function(input, output)({
    return(basicStats(input))
  })
  n <- reactive({
    return(ncol(df()))
  })
  
  port_spec<-reactive({
    
    # Create the portfolio specification
    port_spec <- portfolio.spec(colnames(returns()))
    
    # Add a full investment constraint such that the weights sum to 1
    port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
    
    # Add a long only constraint such that the weight of an asset is between 0 and 1
    port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
    
    # Add an objective to minimize portfolio standard deviation
    port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
    
    return(port_spec)
  })
  
  # Solve the optimization problem
  
  opt<-reactive({
    
    opt <- optimize.portfolio(returns(), portfolio = port_spec(), optimize_method = "ROI")
    
    return(opt)
  })
  
  wts<-reactive({
    
    return(extractWeights(opt()))
    
  })
  
  
  wts_var <- reactive({
    x = vector("list", n())
    x[] <- lapply(extractWeights(opt()), as.numeric)
    return(x)
  })
  
  
  net <- function(input,output)({
    
    cor_mat <- cor(df())
    cor_mat[ lower.tri(cor_mat, diag=TRUE) ]<- 0
    
    graph <- graph.adjacency(cor_mat, weighted=TRUE, mode="upper" )
    # E(graph)$weight<-t(cor_mat)[abs(t(cor_mat))>0.5]
    E(graph)[ weight>0.7 ]$color<- "yellow"
    E(graph)[ weight>=0.4 & weight<0.7 ]$color <-"blue" 
    E(graph)[ weight>=0 &weight<0.4 ]$color <- "green" 
    E(graph) [ weight<0 ]$color <- "red"
    # E(graph)[ weight<0.55  ]$color <- "yellow"
    V(graph)$label<- seq(1:n())#V(graph)$name
    V(graph)$name <- colnames(df())
    graph$layout <- layout.fruchterman.reingold
    factor<-as.factor(cut(E(graph)$weight*10,c(-10,-7,-4,0,4,7,10)))
    plotit <- function(input,output){plot(decompose.graph(graph)[[which.max(sapply(decompose.graph(graph), vcount))]], edge.width =as.numeric(factor)*1.5,frame=T)
      legend("bottomleft", title="Colors", cex=0.75, pch=16, col=c("yellow", "blue","green", "red"),
             legend=c(">70%", "40-70%","0-40%","0-(100)%"), ncol=2)}
    return(plotit())
  })
  
  eff <- function(input, output){
    meanvar.ef <- create.EfficientFrontier(R=df(), portfolio=port_spec(), type="mean-StdDev")
    return(meanvar.ef)
  }
  
  sp <- function(inpu,output){
    # Web-scrape SP500 stock list
    sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
      html_node("table.wikitable") %>%
      html_table() %>%
      select(`Ticker symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>%
      as_tibble()
    
    # Format names
    names(sp_500) <- sp_500 %>% 
      names() %>% 
      str_to_lower() %>% 
      make.names()
    
    sp_5 <- subset(sp_500, ticker.symbol %in% unlist(strsplit(input$symbol, " ")))
    
    get_stock_prices <- function(ticker, return_format = "tibble", ...) {
      # Get stock prices
      stock_prices_xts <- getSymbols(Symbols = ticker, src = "yahoo",auto.assign = FALSE, ...)
      # Rename
      names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
      # Return in xts format if tibble is not specified
      if (return_format == "tibble") {
        stock_prices <- stock_prices_xts %>%
          as_tibble() %>%
          rownames_to_column(var = "Date") %>%
          mutate(Date = ymd(Date))
      } else {
        stock_prices <- stock_prices_xts
      }
      stock_prices
    }
    
    get_log_returns <- function(x, return_format = "tibble", period = 'daily', ...) {
      # Convert tibble to xts
      if (!is.xts(x)) {
        x <- xts(x[,-1], order.by = x$Date)
      }
      # Get log returns
      log_returns_xts <- periodReturn(x = x$Adjusted, type = 'log', period = period, ...)
      # Rename
      names(log_returns_xts) <- "Log.Returns"
      # Return in xts format if tibble is not specified
      if (return_format == "tibble") {
        log_returns <- log_returns_xts %>%
          as_tibble() %>%
          rownames_to_column(var = "Date") %>%
          mutate(Date = ymd(Date))
      } else {
        log_returns <- log_returns_xts
      }
      log_returns
    }
    sp_5 <- sp_5 %>%
      mutate(
        stock.prices = map(ticker.symbol, 
                           function(.x) get_stock_prices(.x, 
                                                         return_format = "tibble", 
                                                         from = input$startDate,
                                                         to = input$lastDate)
        ),
        log.returns  = map(stock.prices, 
                           function(.x) get_log_returns(.x, return_format = "tibble")),
        mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
        sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
        n.trade.days = map_dbl(stock.prices, nrow)
      )
    
    return(sp_5)
  }
  
  sp_plot <- function(input,output){
    plot_ly(data   = sp(),
            type   = "scatter",
            mode   = "markers",
            x      = ~ sd.log.returns,
            y      = ~ mean.log.returns,
            color  = ~ n.trade.days,
            colors = "Blues",
            size   = ~ n.trade.days,
            text   = ~ str_c("<em>", security, "</em><br>",
                             "Ticker: ", ticker.symbol, "<br>",
                             "Sector: ", gics.sector, "<br>",
                             "Sub Sector: ", gics.sub.industry, "<br>",
                             "No. of Trading Days: ", n.trade.days),
            marker = list(opacity = 0.8,
                          symbol = 'circle',
                          sizemode = 'diameter',
                          sizeref = 4.0,
                          line = list(width = 2, color = '#FFFFFF'))
    ) %>%
      layout(title   = 'Analysis: Stock Risk vs Reward',
             xaxis   = list(title = 'Risk/Variability (StDev Log Returns)',
                            gridcolor = 'rgb(255, 255, 255)',
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwidth = 2),
             yaxis   = list(title = 'Reward/Growth (Mean Log Returns)',
                            gridcolor = 'rgb(255, 255, 255)',
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwith = 2),
             margin = list(l = 100,
                           t = 100,
                           b = 100),
             font   = list(color = '#FFFFFF'),
             paper_bgcolor = 'rgb(0, 0, 0)',
             plot_bgcolor = 'rgb(0, 0, 0)')
  }
  
  
  sp_plot2 <- function(input,output){
    
    sp_5_hp <- sp() %>%
      mutate(rank = mean.log.returns %>% desc() %>% min_rank()) %>%
      arrange(rank) %>%
      select(ticker.symbol, rank, mean.log.returns, sd.log.returns, log.returns)
    
    sp_5_hp_unnest <- sp_5_hp %>%
      select(ticker.symbol, log.returns) %>%
      unnest()
    
    sp_5_hp_spread <- sp_5_hp_unnest %>%
      spread(key = ticker.symbol, value = Log.Returns) %>%
      na.omit()
    
    sp_5_hp_cor <- sp_5_hp_spread %>%
      select(-Date) %>%
      cor() 
    
    sp_5_hp_cor %>%
      corrplot()
  }
  
  
  
  mcsimr <- function(ine,output){
    
    stats <- function(x){
      mat <- matrix(nrow=0,ncol=2)
      stat <- as.data.frame(mat)
      names <- names(x)
      for (i in 1:n()){
        er <- x[,i]
        mu <- mean(er)
        sd <- sd(er)
        delta_t <- 1/12
        z = rnorm(10000)
        month_returns <- mu*delta_t + sd*z*sqrt(delta_t)
        stat[i,] <- c(mean(month_returns)*12,sd(month_returns)*sqrt(12))
        names(stat) <- c("mean","volatility")
      }
      return(stat)
    }
    
    a <- stats(returns())
    b <- as.data.frame(lapply(wts(),as.numeric))
    mean <- sum(a[,1]*b)
    sd <- sum(a[,2]*b)
    stats_p <- data.frame(mean,sd)
    
    # 5000 scenarios, for 30 years. $1000 initial investment, type is 1 if stock, 2 if bond
    
    bond_value <- getSymbols(Symbols = "DGS3", src="FRED", auto.assign = FALSE)
    # days <- c(input$startDate,input$lastDate)
    # bond_value <- bond_value[days]
    bond_return_disc <- Return.calculate(bond_value, method = c("discrete"))[-1]
    bond_return_disc <- bond_return_disc[-1]
    
    mean_brd <- mean(bond_return_disc, na.rm = TRUE)
    sd_brd <- sd(bond_return_disc, na.rm = TRUE)
    corr_sb <- (mean_brd*stats_p$mean)/(sd_brd*stats_p$sd)
    
    initialcapital<-1000;
    
    years<-30;
    
    scenarios<-5000;
    
    types<-2;
    
    sigma<-matrix(c(stats_p$sd,corr_sb,corr_sb,sd_brd),c(2,2));
    
    mu<-matrix(c(stats_p$mean,mean_brd),2,1);
    
    stockreturn<-matrix(1, scenarios,1);
    
    bondreturn<-matrix(1,scenarios,1);
    
    
    for (ayear in 1:years)
      
    {
      
      marketreturnrate <- mvrnorm(n = 1000,mu = mu,Sigma = sigma);
      
      stockreturn<-stockreturn*(1+marketreturnrate[,1]); # for $1 investment
      
      bondreturn<-bondreturn*(1+marketreturnrate[,2]); # for $1 investment
      
    }
    
    counter<-1;
    
    weightstrategies<-seq(from=0, to=1,by=0.2); # try several weights
    
    endof30yearcapital<-matrix(0,scenarios,length(weightstrategies));
    
    for (weight in weightstrategies)
      
    {
      
      endof30yearcapital[,counter]<-(weight)*initialcapital*stockreturn+(1-weight)*initialcapital*bondreturn    
      counter<-counter+1;
      
    }
    return(endof30yearcapital)
  }
  
  
  
  
  
  
  # then plot histogram without default titles
  mcsim <- function(weights,output){
    counter<-weights*5+1;
    initialcapital<-1000;
    endof30yearcapital <- mcsimr();
    
    # now plot histogram, first get a list containing the structure
    
    histlist<-hist(endof30yearcapital[,counter],plot=FALSE);
    
    hist(endof30yearcapital[,counter],breaks=histlist$breaks,col=heat.colors(length(histlist$breaks)),ann=FALSE)
    
    title(main=paste("End of 30 year capital histogram with stock's weight =",weights), 
          sub = "5000 scenarios, $1000 initial investment in stocks and bonds",
          col.main="red", font.main=4);
    
  }
  
  
  
  #First Tab Outputs
  output$priceplot <- renderDygraph(dygraph(df()[,1:n()]))
  output$logreturnsplot <- renderDygraph(dygraph(returns_log()[,1:n()]))
  output$bstat_log <- renderPrint(bstats(returns_log()[,1:n()]))
  output$bstat <- renderPrint(bstats(returns()[,1:n()]))  
  output$returnsplot <- renderDygraph(dygraph(returns()[,1:n()]))
  
  
  #Second Tab Outputs
  output$wt<-renderPrint(round(wts()*100,2))
  output$plot2<-renderPlot(barplot(wts(), col="wheat",ylab="Weights", main ="Optimised weights" ))
  output$tab<-renderPrint(table.AnnualizedReturns(returns()))
  output$sna <- renderPlot(net())
  output$cor <- renderPrint(cor(df()))
  output$deg <- renderPlot(chart.EfficientFrontier(eff(), match.col="StdDev", type="l", pch=4, labels.assets = TRUE))
  output$deg_s <- renderPrint(summary(eff(), digits=2))
  output$deg_w <- renderPlot(chart.EF.Weights(eff(), colorset=bluemono, match.col="StdDev"))
  output$sp_plt1 <- renderPlotly(sp_plot())
  output$sp_plt2 <- renderPlot(sp_plot2())
  
  
  # Third Tab Outputs
  output$Value95 <- renderPrint(round(VaR(returns(),p =0.95),digits = 3))
  output$Value99 <- renderPrint(round(VaR(returns(),p =0.99),digits = 3))
  output$CValue95 <- renderPrint(round(ETL(returns(),p =0.95),digits = 3))
  output$CValue99 <- renderPrint(round(ETL(returns(),p =0.99),digits = 3))
  output$iValue99 <- renderPrint(round(VaR(returns(),p =0.99,portfolio_method = "marginal"),digits = 3))
  output$iValue95 <- renderPrint(round(VaR(returns(),p =0.95,portfolio_method = "marginal"),digits = 3))
  output$dr <- renderPrint(table.DownsideRisk(returns(),Rf=0))
  
  
  # Fourth Tab Outputs
  # output$mcplot <- renderPlot(plot(mcsimr()))
  output$mcplot1 <- renderPlot(mcsim(weights = 0))
  output$mcplot2 <- renderPlot(mcsim(weights = 0.2))
  output$mcplot3 <- renderPlot(mcsim(weights = 0.4))
  output$mcplot4 <- renderPlot(mcsim(weights = 0.6))
  output$mcplot5 <- renderPlot(mcsim(weights = 0.8))
  output$mcplot6 <- renderPlot(mcsim(weights = 1.0))
})

# Run the application 
shinyApp(ui = ui, server = server)
