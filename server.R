server <- function(input, output, session){
  
  options(scipen = 1000)
  
  subData <- reactive({
    dtaLA <- projDta[projDta$LA %in% input$LocalAuth, ]
    dtaLA <- dtaLA[dtaLA$Age == input$AgeGroup,]
    dtaLA <- dtaLA[dtaLA$variable %in% seq(from = min(input$yrs), to = max(input$yrs), by = 1),]
  #Calculates year on year change
  #  dtaLA$percChng <- ave(dtaLA$value, dtaLA$LA, FUN = Delt)
  })
  someotherData <- reactive({
    dat <- subData()
    PercentageChange <- round(ave(dat$value, as.factor(dat$LA), FUN = function(x) {(x/x[1] * 100)-100}), 1)
    dat <- cbind(dat, PercentageChange)
  })
   aggData <- reactive({
    dtaAgg <- projDta[projDta$LA %in% input$LocalAuthAgg,]
    dtaAgg <- dtaAgg[dtaAgg$variable %in% seq(from = min(input$yrsAgg), to = max(input$yrsAgg), by = 1),]
    if(input$AgeGroupAgg == "Dependency Ratio"){
      dtaAgg <- aggregate(value~Age+variable, FUN = sum, data = dtaAgg)
      dr <- tapply(dtaAgg$value, as.factor(dtaAgg$variable), function(x) {(x[1]+x[2])/(x[5]-(x[1]+x[2])) *100})
      dtaAgg <- data.frame("variable" = as.numeric(names(dr)), "value" = round(dr,2))
     # dtaAgg$PChange <- round(((dtaAgg$value-dtaAgg$value[1])/dtaAgg$value[1])*100,2)
      } 
    else{
      dtaAgg <- dtaAgg[dtaAgg$Age == input$AgeGroupAgg,]
      dtaAgg <- tapply(dtaAgg$value,as.factor(dtaAgg$variable), FUN = sum)
      dtaAgg <- data.frame("variable" = as.numeric(names(dtaAgg)), "value" = as.numeric(dtaAgg))
      PChange <- round(((dtaAgg$value-dtaAgg$value[1])/dtaAgg$value[1])*100,2)
      dtaAgg <- cbind(dtaAgg,PChange)}
    })
  
  linegraph <- function(){
    ifelse(input$AgeGroup == "Dependency Ratio", lab <- "Dependency Ratio (Population 0-15 + Over 65/ Population 16-64)", lab <-"Population")
    if(input$DispType == "Absolute Change"){
    dta <- subData()
    p <-ggplot(data = dta) +
      geom_line(aes(x = variable, y = value, fill = LA, colour = LA), size = 1.5) +
      guides(fill = FALSE, colour = FALSE) +
      theme_bw() +
      scale_y_continuous(limits = c(min(dta$value)-min(dta$value)/5, max(dta$value)+max(dta$value)/5)) +
      scale_x_continuous(breaks = seq(min(input$yrs), max(input$yrs),3)) +
      xlab("Year") + 
     geom_label_repel(data = dta[dta$variable == max(input$yrs),], 
            aes(x = variable, y = value, label = paste(LA, value)), 
            nudge_x = -(diff(input$yrs)/6)) +
      geom_label_repel(data = dta[dta$variable == min(input$yrs),], 
                       aes(x = variable, y = value, label = paste(LA, value))) +
    ylab(print(lab))
    return(p)} else{
      dta <- someotherData()
      p <-ggplot(data = dta) +
        geom_line(aes(x = variable, y = PercentageChange, fill = LA, colour = LA), size = 1.5) +
        guides(fill = FALSE, colour = FALSE) +
        theme_bw() +
        scale_x_continuous(breaks = seq(min(input$yrs), max(input$yrs),3)) +
        scale_y_continuous(limits = c(min(dta$PercentageChange)-5, max(dta$PercentageChange)+max(dta$PercentageChange)/4)) +
        xlab("Year") + ylab("Percentage Change") +
        geom_label_repel(data = dta[dta$variable == max(input$yrs),], 
                      aes(x = variable, y = PercentageChange, label = paste(LA,rep(": ", length(variable)),
                                            PercentageChange, rep("%", length(variable)), sep = "")), 
                          nudge_x = -(diff(input$yrs)/6))
      return(p)
    }
  }

  output$plot <- renderPlot({
    if(nrow(someotherData()) == 0){
      NA
    } else{linegraph()}
  })
  
  addGraph <- function(){
    ifelse(input$AgeGroup == "Dependency Ratio", lab <- "Dependency Ratio (Population 0-15 + Over 65/ Population 16-64)", lab <-"Population")
    dta <- aggData()
    if(input$DispTypeAgg == "Absolute Change"){
    p <-ggplot(data = dta) +
      geom_line(aes(x = variable, y = value), size = 1.5, colour = "red") +
      guides(fill = FALSE, colour = FALSE) +
      theme_bw() +
      scale_y_continuous(limits = c(min(dta$value)-min(dta$value)/15, max(dta$value)+max(dta$value)/15)) +
      scale_x_continuous(breaks = seq(min(input$yrsAgg), max(input$yrsAgg),3)) +
      xlab("Year") + 
      geom_label_repel(data = dta[dta$variable == max(input$yrsAgg),], 
                       aes(x = variable, y = value, label = value), 
                       nudge_x = -(diff(input$yrsAgg)/6)) +
      geom_label_repel(data = dta[dta$variable == min(input$yrsAgg),], 
                       aes(x = variable, y = value, label = value)) +
      ylab(print(lab))
    return(p)} else{
      p <-ggplot(data = dta) +
        geom_line(aes(x = variable, y = PChange), size = 1.5, colour = "red") +
        guides(fill = FALSE, colour = FALSE) +
        theme_bw() +
        scale_y_continuous(limits = c(min(dta$PChange)-min(dta$PChange)/15, max(dta$PChange)+max(dta$PChange)/15)) +
        scale_x_continuous(breaks = seq(min(input$yrsAgg), max(input$yrsAgg),3)) +
        xlab("Year") + 
        geom_label_repel(data = dta[dta$variable == max(input$yrsAgg),], 
                         aes(x = variable, y = PChange, label = PChange), 
                         nudge_x = -(diff(input$yrsAgg)/6)) +
        ylab(print(lab))
      return(p)
    }
  }
  
  output$AggPlot <- renderPlot({
    if(nrow(aggData()) == 0){
      NA
    } else{addGraph()}
  })
  
  observeEvent(eventExpr = input$selAll,
  handlerExpr = {
    updateCheckboxGroupInput(session = session,
                             inputId = "LocalAuth",
                             selected = unique(projDta$LA))
  } 
  )
  
  observeEvent(eventExpr = input$SelNon,
               handlerExpr = {
                 updateCheckboxGroupInput(session=session,
                                          inputId = "LocalAuth",
                                          selected = NA)
               }
               )
  observeEvent(eventExpr = input$selAllAgg,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "LocalAuthAgg",
                                          selected = unique(projDta$LA))
               } 
  )
  
  observeEvent(eventExpr = input$SelNonAgg,
               handlerExpr = {
                 updateCheckboxGroupInput(session=session,
                                          inputId = "LocalAuthAgg",
                                          selected = NA)
               }
  )
  observeEvent(eventExpr = input$lapPage_link,
               handlerExpr = {
                 updateNavbarPage(session, "mainList","Local Authority Projections")
               })
  observeEvent(eventExpr = input$regPage_link,
               handlerExpr = {
                 updateNavbarPage(session, "mainList", "Regional Aggregation")
               })
  
  
  output$dataexp <- DT::renderDataTable({
    ifelse(input$AgeGroup == "Dependency Ratio", lab2 <- "Dependency Ratio", lab2 <- "Population")
    dataset <- someotherData()
    dataset <- dataset[c(4,1,2,3,5)]
    colnames(dataset)[4] <- lab2
    tbl <-datatable(dataset, extensions = "Scroller", rownames = FALSE, 
                options = list(pageLength = 4160, scrollY = 700, dom = "t"),
                colnames = c("Local Authority" = 1, "Year" = 3))
  })
  
  output$Data85 <- DT::renderDataTable({
    data <- projDta[projDta$Age == "Aged 85 and Over" & projDta$variable %in% c(seq(from =2014, 2035,3),2039),2:4]
    data <- dcast(data, LA ~ variable)
    data$`PercentageChange` <- round(data$`2039`/data$`2014`*100,2)
    data <- datatable(data, extensions = "Scroller", rownames = FALSE,
                      options = list(pageLength = 32, dom = "t", scrollY = 700, scrollX = TRUE),
                      colnames = c("Local Authority" = 1, "Percentage Change" = 11))
  })
  output$depRatioDat <- DT::renderDataTable({
    data2 <- projDta[projDta$Age == "Dependency Ratio" & projDta$variable %in% c(seq(from =2014, 2035,3),2039),2:4]
    data2 <- dcast(data2, LA ~ variable)
    data2 <- datatable(data2, extensions = "Scroller", rownames = FALSE,
                      options = list(pageLength = 32, dom = "t", scrollY = 700, scrollX = TRUE),
                      colnames = c("Local Authority" = 1))
  })
  
  output$dlAll <- downloadHandler(
    filename = paste("NRS_PopulationProjections", ".csv", sep = ""),
    content = function(con) {
      write.csv(dlData, con, row.names = FALSE)
    }
  )
  
  output$HealthyLE <- DT::renderDataTable({
    Hldata <- datatable(HLEdta, extensions = "Scroller", rownames = FALSE, 
                        options = list(pageLength = 32, dom  = "t", scrollY = 700),
                        colnames = c("Local Authority", "Male Healthy Life Expectancy at Birth, 2013", 
                                     "Female Healthy Life Expectancy at Birth, 2013"))
  })
  
  output$AggDtaTbl <- DT::renderDataTable({
    aggDta <- aggData()
    aggDta$YrOn <- round((aggDta$value/lag(aggDta$value, 1)-1)*100, 2)
    p <- datatable(aggDta, extensions = "Scroller", rownames = FALSE,
                   options = list(pageLength = 32, dom = "t", scrollY = 700),
                   colnames = c("Year", "Value", "Total Percentage Change (from Base Year)",
                                "Year on Year Percentage Change")
    ) 
  })
  
  data4dl <- reactive({
    dt <- someotherData() 
    dd <- dcast(dt, LA ~ variable + Age)
    ddd <- dcast(dt, LA ~ variable + Age, value.var = "PercentageChange")
    colnames(ddd)[2:ncol(ddd)] <- paste(colnames(ddd)[2:ncol(ddd)], "%Change")
    outputDta <-left_join(dd,ddd, by = "LA")
  })
  
   output$dldata <- downloadHandler(
    filename = paste("NRS_PopulationProjections", ".csv", sep =""),
    content = function(con) {
      write.csv(data4dl(), con, row.names = FALSE)
    }
  )
   output$svPlt <- downloadHandler(
     filename = paste("Pop_Projection_Plot", ".png", sep = ""),
     content = function(con){
       ggsave(con,plot = linegraph(),device = "png", width = 8, height = 6 )
     }
   )
  
      } 