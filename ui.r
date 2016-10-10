ui <- fluidPage(
  titlePanel("Population Projections"),
  tags$head(tags$style(
    "#chkbx {margin-top:10px; column-count:2; -webkit-column-count:2; -moz-column-count:2;
    text-align:justify; display:block}"
  )),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("AgeGroup", "Select an Age Group to Forecast", unique(projDta$Age)),
      h4("Select Local Authority"),
      actionButton("selAll", "Select All"), actionButton("SelNon", "Select None"),
      div(id = "chkbx",checkboxGroupInput("LocalAuth", label = NA,
                            choices = unique(projDta$LA), selected = NA, inline = TRUE
                         )),
      sliderInput("yrs", "Select Time Series", min = min(projDta$variable), 
                  max = max(projDta$variable),step = 1, value = c(2012, 2037),
                  sep = "")
    ),
    mainPanel(
      plotOutput("plot", height = "800px")
    )
  )
)