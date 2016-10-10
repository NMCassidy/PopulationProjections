server <- function(input, output, session){
  
  options(scipen = 1000)
  
  subData <- reactive({
    dtaLA <- projDta[projDta$LA %in% input$LocalAuth, ]
    dtaLA <- dtaLA[dtaLA$Age == input$AgeGroup,]
    dtaLA <- dtaLA[dtaLA$variable %in% seq(from = min(input$yrs), to = max(input$yrs), by = 1),]
  })
  
  output$plot <- renderPlot({
    dta <- subData()
    p <-ggplot(data = dta) +
      stat_smooth(aes(x = variable, y = value, fill = LA), se = FALSE) +
      guides(fill = FALSE) +
      theme_bw() +
      xlab("Year") + ylab("Population") +
     geom_label_repel(data = dta, aes(x = variable, y = value, label = LA))
    p
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