ui <- fluidPage(
  titlePanel("Population Projections"),
  tags$head(tags$style(
    "#chkbx {margin-top:10px; column-count:2; -webkit-column-count:2; -moz-column-count:2;
    text-align:left; vertical-align:text-top}
    .buttons {text-align:left; display:inline}
    .divider{width: 30px; height: auto; display:inline}
    #lazy {font-style: italic; margin-top:15px; margin-bottom:0px}"
  )),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("AgeGroup", "Select an Age Group to Forecast", unique(projDta$Age)),
      h4("Select Local Authority"),
      div(class = "buttons", actionButton("selAll", "Select All"),
          div(class = "divider"), actionButton("SelNon", "Select None")),
      div(id = "chkbx",checkboxGroupInput("LocalAuth", label = NA,
                            choices = unique(projDta$LA), selected = NA, inline = FALSE
                         )),
      sliderInput("yrs", "Select Time Series", min = min(projDta$variable), 
                  max = max(projDta$variable),step = 1, value = c(2012, 2037),
                  sep = ""),
      radioButtons("DispType", "Select Figure to Display", c("Absolute Change", "Percentage Change"),
                   selected = NULL, inline = TRUE),
      downloadButton("dldata", "Download Selected"),
      downloadButton("dlAll", "Download All Data"),
      div(id = "lazy", p("Data Source: NRS"))
    ),
    mainPanel(tabsetPanel(
      tabPanel("Plot",
     plotOutput("plot", height = "800px")
      ),
     tabPanel("Over 85 Population", DT::dataTableOutput("Data85")),
     tabPanel("Dependency Ratio", DT::dataTableOutput("depRatioDat")),
     tabPanel("Data Explorer", DT::dataTableOutput("dataexp"))
    )
  ))
)