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
  
  output$plot <- renderPlot({
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
    p} else{
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
      p
    }
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
  
      } 