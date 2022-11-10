library("shiny")
library("dplyr")
#library("ecomAnalytics")
library(factoextra)

function(input, output, session) {
  ### Input Data ###
  inputType <- reactive({
    input$inputType
  })
  
  getExampleData <- reactive({
    #ecmdata = read.csv("ecomData.csv")
    data.frame(EcomData4clustering)
    #ecomData()
  })
  
  
  getDataUser <- reactive({
    req(input$file1)
    ecomData_ <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep)
    ecomData_
  })
  
  getRawData <- reactive({
    if (inputType() == "Sample Data") getExampleData()
    else getDataUser()
  })
  
  
  
  #table output functions
  
  #' @export
  #' @rdname kpiFunctions
  calcTopProductsI <- function(ecomData, customerID, numProducts,datespan) {
    ecomData <- ecomData %>%
      filter( (DateComplete > datespan[1]) & (DateComplete < datespan[2]))
    top <- ecomData %>% filter(CustomerID == customerID) %>%
      group_by(StockCode, Description) %>% summarize(count = n(),.groups = "keep") %>%
      arrange(desc(count)) %>% head(n = as.numeric(numProducts))
  }
  
  
  #' @export
  #' @rdname kpiFunctions
  calcLowProductsI <- function(ecomData, customerID, numProducts,datespan) {
    ecomData <- ecomData %>%
      filter((DateComplete > datespan[1]) & (DateComplete < datespan[2]))
    low <- ecomData %>% filter(CustomerID == customerID) %>%
      group_by(StockCode, Description) %>% summarize(count = n(),.groups = "keep") %>%
      arrange(count) %>% head(n = as.numeric(numProducts))
  }
  
  getData <- reactive({
    edata = getRawData()
    edata <- na.omit(edata)
    edata$StockCode <- as.character(edata$StockCode)
    edata$Description <- as.character(edata$Description)
    edata$DateComplete <- as.Date(edata$Date)
    data.frame(edata)
  })
  getotherdata = reactive({
    x = getData()
    int_data = separate(x, 
                        col = time, 
                        into = c("hour", "minuteOfDay"), 
                        sep = ':',
                        remove = TRUE)
    int_data$hour = as.numeric(int_data$hour)
    
    dataExtract = data.frame(int_data[,c("InvoiceNo","StockCode", "Description","Quantity"  ,
                                         "UnitPrice","CustomerID","Country","totalCost",
                                         "Date","year","month","dayOfWeek","hour")])
    
    colnames(dataExtract) = c("InvoiceNo" ,"StockCode","Description","Quantity","UnitPrice",
                              "CustomerID","Country","Sales","Date","Year", 
                              "Month","Weekday","Hour")
    dataExtract$DateComplete <- as.Date(dataExtract$Date)
    data.frame(dataExtract)
  })
  
  getWssData = reactive({
    x = getData()
    df = separate(x, 
                  col = time, 
                  into = c("hourOfDay", "minuteOfDay"), 
                  sep = ':',
                  remove = TRUE)
    
    df$hourOfDay = gsub(" ", "", df$hourOfDay)
    df$hourOfDay = as.numeric(df$hourOfDay)
    df$Date = as.Date(df$Date)
    custSummary_2 <- df %>%
      group_by(InvoiceNo, CustomerID, Country, Date, month, year, hourOfDay, dayOfWeek) %>%
      summarise(orderVal = sum(totalCost),.groups = "keep") %>%
      mutate(recent = Sys.Date() - Date) %>%
      ungroup()
    
    custSummary_2$recent <- as.character(custSummary_2$recent)
    custSummary_2$recentDays <- sapply(custSummary_2$recent, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
    custSummary_2$recentDays <- as.integer(custSummary_2$recentDays)
    
    customerSummary_3 <- custSummary_2 %>%
      group_by(CustomerID, Country) %>%
      summarise(orders = n_distinct(InvoiceNo), revenue = sum(orderVal), 
                meanRevenue = round(mean(orderVal), 2), medianRevenue = median(orderVal), 
                mostDay = names(which.max(table(dayOfWeek))), 
                mostHour = names(which.max(table(hourOfDay))),
                recency = min(recentDays),.groups = "keep")%>%
      ungroup()
    customerSummary_3Sum <- customerSummary_3 %>%
      filter(orders > 1, revenue > 50)
    
    custTargets <- customerSummary_3Sum %>%
      select(recency, revenue, meanRevenue, medianRevenue, orders) %>%
      as.matrix()
    rownames(custTargets) <- customerSummary_3Sum$CustomerID
    as.matrix(custTargets)
  })
  
  getPlotClusterData = reactive(
    {
      x = getData()
      x$item.return <- grepl("C", x$InvoiceNo, fixed = TRUE)
      x$purchase.invoice <- ifelse(x$item.return=="TRUE",0,1)
      customers <- as.data.frame(unique(df$CustomerID))
      names(customers) <- "CustomerID"
      
      # Adding a recency column by substracting the InvoiceDate from the (last InvoiceDate+1)
      df$recency <- as.Date("2011-12-09")- as.Date(df$Date)
      
      # Returns will be removed to only consider the data of most recent "purchase"
      purdata <- subset(df,purchase.invoice==1)
      
      # Aggregate to obtain the number of days since most recent purchase
      recency <- aggregate(recency ~ CustomerID, data=purdata,FUN=min,na.rm=TRUE)
      
      # Add recency to customer data
      customers <- merge(customers,recency,by="CustomerID",all = TRUE, sort=TRUE)
      remove(recency)
      str(customers)
      customers$recency <- as.numeric(customers$recency)
      
      customer.invoices <- subset(df, select = c("CustomerID","InvoiceNo", "purchase.invoice"))
      customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]
      customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),]
      row.names(customer.invoices) <- NULL
      
      # Number of invoices/year (purchases only)
      annual.invoices <- aggregate(purchase.invoice ~ CustomerID, data=customer.invoices, FUN=sum,na.rm=TRUE)
      
      names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"
      
      # Add # of invoices to customers data
      customers <- merge(customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE)
      remove(customer.invoices, annual.invoices)
      customers <- subset(customers, frequency > 0)
      
      total.sales <- aggregate(totalCost ~ CustomerID, data=df, FUN=sum, na.rm=TRUE)
      names(total.sales)[names(total.sales)=="totalCost"] <- "monetary"
      
      # Add monetary value to customers dataset
      customers <- merge(customers, total.sales, by="CustomerID", all.x=TRUE, sort=TRUE)
      data.frame(customers)
      
    }
  )
  #plot output functions
  #' @export
  #' @rdname kpiFunctions
  trendDist <- function(ecomData, dateSpan, trendVar) {
    trendData <- ecomData %>% filter(
      (DateComplete > dateSpan[1]) & (DateComplete < dateSpan[2]))  %>%
      group_by(DateComplete) %>% summarise(summation = sum(get(trendVar)))
    
    ggplot(trendData, aes(x = DateComplete, y = summation)) +
      geom_line(color = "#b3d0ec") + labs(x = "Period", y = trendVar)
  }
  
  #' @export
  #' @rdname kpiFunctions
  timeDist <- function(ecomData, timeVar) {
    timeDistribution <- ecomData %>%
      group_by(get(timeVar)) %>% summarise(Orders = length(get(timeVar)))
    if (timeVar == "Weekday") {levels(timeDistribution$`get(timeVar)`) <- seq(1:7)}
    
    ggplot(timeDistribution, aes(`get(timeVar)`, Orders)) +
      geom_col(fill = "#b3d0ec") +
      labs(x = timeVar, y = paste("Orders per", timeVar)) +
      scale_x_discrete(drop = FALSE)
  }
  
  
  ### Shop Level Analytics ###
  
  
  ##Total revenue split by different choices  
  output$trend <- renderPlot({
    trendDist(getotherdata(), dateSpan = input$trendSpanVar,
              trendVar = input$trendVar)
  })
  
  output$time <- renderPlot({
    timeDist(getotherdata(), timeVar = input$timeVar)
  })
  
  ### Individual Level Analysis ###
  outVar <- reactive({
    ecomData <- getotherdata()
    sort(unique(as.character(ecomData$CustomerID)))
  })
  
  observe({
    updateSelectInput(session, "customerId", choices = outVar()) #data()$customerID #c(output$outVar)
  })
  
  getRevenueKpiI <- reactive({
    calcRevenueI(getotherdata(), customerID = input$customerId)
  })
  
  output$revenueKpiI <- renderPrint({
    revenue <- getRevenueKpiI()
    infoBox(title = "Total Revenue", revenue,
            color = "black", width = 12)
  })
  
  getQuantileKpiI <- reactive({
    calcQuantileI(getotherdata(), customerID = input$customerId)
  })
  
  output$quantileKpiI <- renderPrint({
    quantile <- getQuantileKpiI()
    infoBox("Customer's Quantile (by Revenue)", paste0("Top ", quantile, "%"),
            icon = icon("tachometer-alt"), color = "black", width = 12)
  })
  
  getNumProductsKpiI <- reactive({
    calcNumProdsI(getotherdata(), customerID = input$customerId)
  })
  
  output$numProductsKpiI <- renderPrint({
    numProducts <- getNumProductsKpiI()
    infoBox("Different Products count", numProducts, icon = icon("cube"),
            color = "black", width = 12)
  })
  
  getTopProductsI <- reactive({calcTopProductsI(getotherdata(), customerID = input$customerId,
                                                numProducts = input$numProductsI,
                                                datespan = input$productsSpanVarI)})
  
  output$topProductsI <- renderTable({getTopProductsI()})
  
  getLowProductsI <- reactive({
    calcLowProductsI(getotherdata(), customerID = input$customerId,
                     numProducts = input$numProductsI,
                     datespan = input$productsSpanVarI)
  })
  
  output$lowProductsI <- renderTable({
    getLowProductsI()
  })
  
  ### Raw Data ###
  output$rawDataOverview <- DT::renderDT({
    DT::datatable(getotherdata(),
                  options = list(scrollX = TRUE))
  })
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    getWssData()
  })
  
  clusters <- reactive(
    {
      custTargets = getClusterData()
      kmeans(scale(custTargets[,1:5]), input$clusters, nstart = 1)
    }
  )
  
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    fviz_nbclust(getWssData(), kmeans, method = "wss") +
      geom_vline(xintercept = 3, linetype = 2)
    #    plot(selectedData(),
    #        col = clusters()$cluster,
    #       pch = 21, cex = 3)
    # points(clusters()$centers, pch = 19, cex = 4, lwd = 4)
  })
  output$plot2 = renderPlot(
    {
      set.seed(415)
      x= getPlotClusterData()
      cluster <- kmeans(scale(x[,2:4]), input$clusters , nstart = 1)
      #fviz_cluster(cluster, data=as.data.frame(x)[, -5],
      #            ellipse.type = "norm")
      clusplot(x = x[, 2:4], 
               clus = cluster$cluster, 
               lines = 0, 
               shade = TRUE, 
               color = TRUE, 
               labels = 4, 
               plotchar = FALSE, 
               span = TRUE, 
               main = "Customer Clusters")
      
    }
  )
  
}





