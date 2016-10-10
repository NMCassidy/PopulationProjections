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
    percChngTot <- round(ave(dat$value, as.factor(dat$LA), FUN = function(x) {x/x[1] * 100}), 1)
    dat <- cbind(dat, percChngTot)
  })
  
  output$plot <- renderPlot({
    if(input$DispType == "Absolute Change"){
    dta <- subData()
    p <-ggplot(data = dta) +
      geom_line(aes(x = variable, y = value, fill = LA, colour = LA), size = 1.5) +
      guides(fill = FALSE, colour = FALSE) +
      theme_bw() +
      xlab("Year") + ylab("Population") +
     geom_label_repel(data = dta[dta$variable == max(input$yrs),], 
            aes(x = variable, y = value, label = paste(LA, value)), force = 6)
    p} else{
      dta <- someotherData()
      p <-ggplot(data = dta) +
        geom_line(aes(x = variable, y = percChngTot, fill = LA, colour = LA), size = 1.5) +
        guides(fill = FALSE, colour = FALSE) +
        theme_bw() +
        xlab("Year") + ylab("Population") +
        geom_label_repel(data = dta[dta$variable == max(input$yrs),], 
                         aes(x = variable, y = percChngTot, label = paste(LA, percChngTot)), force = 14)
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
  
      } 