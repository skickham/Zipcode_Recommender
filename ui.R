##############################
#  NYC Zip Code Recommender  #
#     CKM : PRE : Elfta      #
#      by Sean Kickham       #
##############################


# ===================================================================
# 
# 
#
# ===================================================================


# ------------------------------  ------------------------------

navbarPage(
  
  # ------------------------------ BASIC INFO ------------------------------
  
  id = 'nav',
  div(
    style = 'font-size: 20px', 'NYC Zip Code Recommender'
  ),
  windowTitle = 'NYC Zip COde Retail Recommender',
  collapsible = TRUE,
  
  # ------------------------------ MAP!! ------------------------------
  
  tabPanel("Interactive Map",
           div(class="outer",
               
               #include custom CSS
               tags$head(
                 includeCSS("style.css")
               ),
               
               tags$style(type = 'text/css', 
                          ".radio label {font-size: 10px;}"
                          ),
               
               #map output (percents work with CSS)
               leafletOutput("map", width = "100%", height = "100%"),
               
               # ------------------------------ input panel ------------------------------
               
               #draggable, transparent input panel
               absolutePanel(id="controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, 
                             top = 100, left = 30, right = "auto", bottom = 'auto',
                             width = 500, height = 'auto',
                             
                             #Input panel title
                             h2("Choose Inputs & Weights"),
                             
                             
                             #CATEGORY CONTROLS
                             #cost
                             fluidRow(
                               #first decide if you want to include this metric or not
                               column(4,
                                      checkboxInput("choosecost",
                                                    "Price per Square Foot",
                                                    value = FALSE,
                                                    width = NULL)),
                               #if yes, control inputs
                               conditionalPanel(
                                 condition = "input.choosecost",
                                 column(4, 
                                        numericInput("w1",
                                                     div(style='font-size: 12px;', "weight"),
                                                     20, min = 1, max = 100)),
                                 column(4,
                                        sliderInput("lim1",
                                                    div(style='font-size: 12px;', "$/sqft"),
                                                    min = 0, max = 100, 
                                                    value = c(0,100),
                                                    ticks=FALSE))
                                 
                               )
                             ),
                             
                             #crime
                             fluidRow(
                               #first decide if you want to include this metric or not
                               column(4,
                                      checkboxInput("choosecrime",
                                                    "Crime Rate",
                                                    value = FALSE,
                                                    width = NULL)),
                               #if yes, control inputs
                               conditionalPanel(
                                 condition = "input.choosecrime",
                                 column(4, 
                                        numericInput("w2",
                                                     div(style='font-size: 12px;', "weight"),
                                                     20, min = 1, max = 100)),
                                 column(4,
                                        sliderInput("lim2",
                                                    div(style='font-size: 12px;', "crimes/person"),
                                                    min = 0, max = 100, 
                                                    value = c(0,100),
                                                    ticks=FALSE))
                                 
                               )
                             ),
                             
                             #complaints
                             fluidRow(
                               #first decide if you want to include this metric or not
                               column(4,
                                      checkboxInput("choosecomplaints",
                                                    "Complaint Rate",
                                                    value = FALSE,
                                                    width = NULL)),
                               #if yes, control inputs
                               conditionalPanel(
                                 condition = "input.choosecomplaints",
                                 column(4, 
                                        numericInput("w3",
                                                     div(style='font-size: 12px;', "weight"),
                                                     20, min = 1, max = 100)),
                                 column(4,
                                        sliderInput("lim3",
                                                    div(style='font-size: 12px;', "complaints/person"),
                                                    min = 0, max = 100, 
                                                    value = c(0,100),
                                                    ticks=FALSE))
                                 
                               )
                             ),
                             
                             #fire
                             fluidRow(
                               #first decide if you want to include this metric or not
                               column(4,
                                      checkboxInput("choosefire",
                                                    "Fire Rate",
                                                    value = FALSE,
                                                    width = NULL)),
                               #if yes, control inputs
                               conditionalPanel(
                                 condition = "input.choosefire",
                                 column(4, 
                                        numericInput("w4",
                                                     div(style='font-size: 12px;', "weight"),
                                                     20, min = 1, max = 100)),
                                 column(4,
                                        sliderInput("lim4",
                                                    div(style='font-size: 12px;', "fires/person"),
                                                    min = 0, max = 100, 
                                                    value = c(0,100),
                                                    ticks=FALSE))
                                 
                               )
                             ),
                             
                             #population
                             fluidRow(
                               #first decide if you want to include this metric or not
                               column(4,
                                      checkboxInput("choosepop",
                                                    "Population",
                                                    value = FALSE,
                                                    width = NULL)),
                               #if yes, control inputs
                               conditionalPanel(
                                 condition = "input.choosepop",
                                 column(4, 
                                        numericInput("w5",
                                                     div(style='font-size: 12px;', "weight"),
                                                     20, min = 1, max = 100)),
                                 column(4,
                                        sliderInput("lim5",
                                                    div(style='font-size: 12px;', "population"),
                                                    min = 0, max = 100, 
                                                    value = c(0,100),
                                                    ticks=FALSE))
                                 
                               ),
                               
                               actionButton("do", "Update Map",icon("search"), width="100%")
                               
                             )
                             
                             )
               
               )),
  
  # ------------------------------ DATA!! ------------------------------
  
  tabPanel("The Data",
           DT::dataTableOutput("ziptable"))
  
  
)

