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
  
  linegraph <- function(){
    ifelse(input$AgeGroup == "Dependency Ratio", lab <- "Dependency Ratio (Population 0-15 + Over 65/ Population 16-64)", lab <-"Population")
    if(input$DispType == "Absolute Change"){
    dta <- subData()
    p <-ggplot(data = dta) +
      geom_line(aes(x = variable, y = value, fill = LA, colour = LA), size = 1.5) +
      guides(fill = FALSE, colour = FALSE) +
      theme_bw() +
      scale_x_continuous(breaks = seq(min(input$yrs), max(input$yrs),5)) +
      xlab("Year") + 
     geom_label_repel(data = dta[dta$variable == max(input$yrs),], 
            aes(x = variable, y = value, label = paste(LA, value)), 
            nudge_x = -(diff(input$yrs)/6)) +
    ylab(print(lab))
    return(p)} else{
      dta <- someotherData()
      p <-ggplot(data = dta) +
        geom_line(aes(x = variable, y = PercentageChange, fill = LA, colour = LA), size = 1.5) +
        guides(fill = FALSE, colour = FALSE) +
        theme_bw() +
        scale_x_continuous(breaks = seq(min(input$yrs), max(input$yrs),5)) +
        xlab("Year") + ylab("Percentage Change") +
        geom_label_repel(data = dta[dta$variable == max(input$yrs),], 
                      aes(x = variable, y = PercentageChange, label = paste(LA,rep(": ", length(variable)),
                                            PercentageChange, rep("%", length(variable)), sep = "")), 
                          nudge_x = -(diff(input$yrs)/6))
      return(p)
    }
  }

  output$plot <- renderPlot({
    linegraph()
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
    data <- projDta[projDta$Age == "Over 85" & projDta$variable %in% c(2012,2017,2027,2037),2:4]
    data <- dcast(data, LA ~ variable)
    data$`PercentageChange` <- round(data$`2037`/data$`2012`*100,2)
    data <- datatable(data, extensions = "Scroller", rownames = FALSE,
                      options = list(pageLength = 32, dom = "t", scrollY = 700),
                      colnames = c("Local Authority" = 1, "Percentage Change" = 6))
  })
  output$depRatioDat <- DT::renderDataTable({
    data2 <- projDta[projDta$Age == "Dependency Ratio" & projDta$variable %in% c(2012,2017,2027,2037),2:4]
    data2 <- dcast(data2, LA ~ variable)
    data2 <- datatable(data2, extensions = "Scroller", rownames = FALSE,
                      options = list(pageLength = 32, dom = "t", scrollY = 700),
                      colnames = c("Local Authority" = 1))
  })
  
  output$dlAll <- downloadHandler(
    filename = paste("NRS_PopulationProjections", ".csv", sep = ""),
    content = function(con) {
      write.csv(dlData, con, row.names = FALSE)
    }
  )
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
       ggsave(con,plot = linegraph(),device = "png", width = 8, height = 5 )
     }
   )
  
      } 