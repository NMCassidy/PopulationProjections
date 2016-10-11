ui <- fluidPage(
  titlePanel("Population Projections"),
  tags$head(tags$style(
    "#chkbx {margin-top:10px; column-count:2; -webkit-column-count:2; -moz-column-count:2;
    text-align:left; vertical-align:text-top}
    .buttons {text-align:left; display:inline; padding-right:20px}
    #lazy {font-style: italic; margin-top:7px; margin-bottom:0px}
    #downloadbut {margin-bottom:10px; display:inline}
    .SvButton {margin-top:7px}",
    HTML("h5 {font-weight:bold}")
  )),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("AgeGroup", "Select an Age Group to Forecast", unique(projDta$Age)),
      h5("Select Local Authority"),
      div(class = "buttons", actionButton("selAll", "Select All"),
         actionButton("SelNon", "Select None")),
      div(id = "chkbx",checkboxGroupInput("LocalAuth", label = NA,
                            choices = unique(projDta$LA), selected = NA, inline = FALSE
                         )),
      sliderInput("yrs", "Select Time Series", min = min(projDta$variable), 
                  max = max(projDta$variable),step = 1, value = c(2012, 2037),
                  sep = ""),
      radioButtons("DispType", "Select Figure to Display", c("Absolute Change", "Percentage Change"),
                   selected = NULL, inline = TRUE),
      div(id = "downloadbut", downloadButton("dldata", "Download Selected Data"),
      downloadButton("svPlt", "Download Plot")),
      div(class = "SvButton", downloadButton("dlAll", "Download All Data")),
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