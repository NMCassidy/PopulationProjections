ui <- navbarPage(id = "mainList",
  title = "Population Projections", theme = shinytheme("simplex"),
  tabPanel("Cover Page",
           br(),
           includeHTML("CoverPage.html"),
           img(src = "http://www.improvementservice.org.uk/benchmarking/images/islogo.png", align = "top")
           ),
 tabPanel("Local Authority Projections",
          #Some css
  tags$head(tags$style(
    ".chckBx {margin-top:10px; column-count:2; -webkit-column-count:2; -moz-column-count:2;
    text-align:left; vertical-align:middle; position:relative; display:block; margin-bottom:0px;
    font-size:1.5vh}",
    ".buttons {text-align:left; display:inline; padding-right:20px;}",
    ".downloadbut {margin-bottom:5px; display:inline-block;}",
    ".SvButton {margin-top:5px;}",
    ".ReferencePg p{font-style: italic; margin-top:7px; margin-bottom:0px; display:inline-block;}",
    ".ReferencePg a{font-style: italic; margin-top:7px; margin-bottom:0px; display:inline-block;}",
    "#plot {height: 80vh !important;}",
    "#AggPlot {height: 80vh !important;}",
    HTML(
 #  " h5 {font-weight:bold;}
   "div.checkbox {margin-top: 0px;}")
  )),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("AgeGroup", "Select an Age Group to Forecast", unique(projDta$Age)),
      h5("Select Local Authority"),
      div(class = "buttons", actionButton("selAll", "Select All"),
         actionButton("SelNon", "Clear All")),
      div(class = "chckBx",checkboxGroupInput("LocalAuth", label = NA,
                            choices = unique(projDta$LA), selected = NA, inline = FALSE)),
      sliderInput("yrs", "Select Time Series", min = min(projDta$variable), 
                  max = max(projDta$variable),step = 1, value = c(2014, 2039),
                  sep = ""),
      radioButtons("DispType", "Select Figure to Display", c("Absolute Change", "Percentage Change"),
                   selected = NULL, inline = TRUE),
      checkboxInput("adjustedFigures", "Use Adjusted Figures"),
      div(class = "downloadbut", list(downloadButton("dldata", "Download Selected Data"),
      downloadButton("svPlt", "Download Plot"))),
      div(class = "SvButton", downloadButton("dlAll", "Download All Data")),
      tags$div(class = "ReferencePg", list(p("Data Sources: "), 
          a("NRS", href = "http://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-projections/sub-national-population-projections/2014-based", 
                 target = "_blank"), p(" and "), a("ScotPHO", href ="http://www.scotpho.org.uk/", target = "_blank"))),
      height = "95%"
    ),
    mainPanel(tabsetPanel(
      tabPanel("Plot",
     plotOutput("plot")
      ),
     tabPanel("Over 85 Population", DT::dataTableOutput("Data85")),
     tabPanel("Dependency Ratio", DT::dataTableOutput("depRatioDat")),
     tabPanel("Healthy Life Expectancy", DT::dataTableOutput("HealthyLE")),
     tabPanel("Data Explorer", DT::dataTableOutput("dataexp"))
     
    )
  ))
 ),
 
##Second Tab dealing with aggregated projections
 tabPanel("Regional Aggregation",
    sidebarLayout(
      sidebarPanel(
        selectInput("AgeGroupAgg", "Select an Age Group to Forecast", unique(projDta$Age)),
        h5("Select Local Authorities to Group"),
        div(class = "buttons", actionButton("selAllAgg", "Select All"),
            actionButton("SelNonAgg", "Clear All")),
        div(class = "chckBx",checkboxGroupInput("LocalAuthAgg", label = NA,
                           choices = unique(projDta$LA), selected = NA, inline = FALSE)),
        sliderInput("yrsAgg", "Select Time Series", min = min(projDta$variable), 
                    max = max(projDta$variable),step = 1, value = c(2014, 2039),
                    sep = ""),
        radioButtons("DispTypeAgg","Select Figure to Display", c("Absolute Change", "Percentage Change"),
                     selected = NULL, inline = TRUE)
      ),
      mainPanel(tabsetPanel(
        tabPanel("Plot",plotOutput("AggPlot")),
        tabPanel("Aggregated Population Figures", dataTableOutput("AggDtaTbl"))
      )
        )
    )
  ),
  tabPanel("Metadata",
           includeHTML("metadata.html"))
)