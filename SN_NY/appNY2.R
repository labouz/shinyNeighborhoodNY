library(grid)
suppressPackageStartupMessages(library(leaflet))
library(neighborhoodNY)
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(shiny))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))


data(usaPlaces2010)
data("NYcounties")


counties <- NYcounties %>%
  mutate(county = paste0(county, " County")) 

counties <- bind_rows(data.frame(county = "New York State", county_code = "000"), 
              counties)

places <- usaPlaces2010 %>%
  filter(STATE == "NY") %>%
  #filter the county subdivisions
  filter(TYPE != "County Subdivision") %>%
  arrange(COUNTY)%>% 
  #add new york
  rbind(data.frame(STATE = "NY",
                   STATEFP = "36",
                   PLACEFP = "xxxxx",
                   PLACENAME = "New York State",
                   TYPE = "State",
                   FUNCSTAT = "X",
                   COUNTY = "New York"))


ui <- fluidPage(
  
  #browser(),
  style = "background-color: #F2EEEC", 
  
  fluidRow(
    img(src = "ShinyNeighborhood.png", width = 1000),
    align = "center"
  ),
  
  br(),
  
  fluidRow(
    h1("NEW YORK PROTOTYPE"),
    align = "center"
  ),
  
  br(),
  
  sidebarPanel(
    selectInput(inputId = "county",
                "Choose county (or other large area):",
                selected = "New York State",
                choices = counties$county) ,
    
    conditionalPanel(
      condition = "input.county != 'New York State'",
      selectInput(
        inputId = "place",
        "Choose a place:",
        selected = "New York city",
        choices = character(0))
    ),
    
    
    
    p("All gives a summary for the entire county. The other places listed above are cities or census defined places (CDP) that the US government identified.",
      style = "font-size: 12px; font-style: italic"),
    br(),
    br(),
    wellPanel( h3("Data Display"),
               br(),
               tags$style("#mode{font-size: 16px}"),
               radioButtons(
                 inputId = "mode",
                 "Show percentages as:",
                 choices = list("Bar charts" = 3,
                                "Waffle plots" = 2)
               ),
               p("Waffle plots are an alternative to pie charts.  They are made of a 10 by 10 gird. Every block in the waffle represents 1%.",
                 style = "font-size: 12px; font-style: italic")
    )
  ), # end sidebarPanel
  
  mainPanel(
    
    wellPanel(
      h3("New York Map"),
      leafletOutput("working_map", height = 450)
    ),
    
    br(),
    
    tabsetPanel(
      tabPanel("Demographics", style = "background-color: #F2EEEC;",
               fluidRow(
                 column(6, plotOutput("ethnicityPlot", height = 250)),
                 column(6, plotOutput("racePlot", height = 250))
               ),
               br(),
               fluidRow(
                 # column(6, plotOutput("englishPlot", height = 250)),
                 # column(6, plotOutput("nativePlot", height = 250))
               ),
               br(),
               fluidRow(
                 # column(6, plotOutput("educationPlot", height = 250))
               ),
               br(),
               fluidRow(
                 # column(8,  offset = 2, plotOutput("agePlot", height = 600))
               )
               
      ), # end tabPanel Demographics
      tabPanel("Housing", style = "background-color: #F2EEEC;",
               fluidRow(
                 # column(6, plotOutput("ownRentPlot", height = 250)),
                 # column(6, plotOutput("vacancyPlot", height = 250))
               ),
               br(),
               fluidRow(
                 # column(6, plotOutput("yearBuiltPlot", height = 250)),
                 # column(6, plotOutput("urbanRuralPlot", height = 250))
               )
      ), # end tabPanel Housing
      tabPanel("Economic", style = "background-color: #F2EEEC;",
               fluidRow(
                 column(6, plotOutput("commutePlot", height = 250))
                 #column(6, plotOutput("povertyPlot", height = 250))
                 
               ),
               br(),
               fluidRow(
                 #column(6, plotOutput("rentincomePlot", height = 250)),
                 column(6, plotOutput("vehiclePlot", height = 250))
               ),
               br()
               # fluidRow(
               #   column(6, plotOutput("unemploymentPlot", height = 250))
               #
               # )
      ), # end tabPanel Economic
      
      tabPanel("Insurance", style = "background-color: #F2EEEC;",
               fluidRow(
                 
                 # column(2,
                 # radioButtons(
                 #   inputId = "insurance",
                 #   "Show insurance by:",
                 #   choices = list("Age and Sex" = 2,
                 #                  "Age" = 4,
                 #                  "Sex" = 6))),
                 #
                 # column(6, plotOutput("insurancePlot", height = 500))
               ),
               br(),
               fluidRow(
                 
                 # column(2,
                 #        radioButtons(
                 #          inputId = "insuranceType",
                 #          "Show insurance type by:",
                 #          choices = list("Age and Sex" = 2,
                 #                         "Age" = 4,
                 #                         "Sex" = 6))),
                 #
                 # column(10, plotOutput("insuranceTypePlot", height = 500))
               )
      )#end tab Insurance
    ) #end tabset panel
  ) #end main panel
) # end UI

server <- function(input, output, session) {
  
  observe({
    req(input$county)
    x = places[str_detect(places$COUNTY, input$county), 4]
    
    x <- rbind("All", x)
    names(x)[1] <- " " #without this, it would put the variable in the drop down 
    
    # Can also set the label and select items
    updateSelectInput(session, "place",
                      label = "Choose a place:",
                      choices = x #,
                      #selected = head(x, 1)
    )
  })
  
  theCounty <- reactive({
    #req(thePlace())
    if(!(input$county == "New York State")){
      as.numeric(counties[counties$county == input$county, 2])
    }else{
      36
    }
  })
  
  # added for showing districts
  output$theCounty <- reactive(
    theCounty()
  )
  
  thePlace <- reactive({
    req(input$place)
    if (input$place == "All") {
      -1
    } else{
      as.numeric(places[places$PLACENAME == input$place, 3])
    }
  })
  
  
  # added for showing districts
  output$thePlace <- reactive(
    thePlace()
  )
  
  output$working_map <- renderLeaflet({
    req(theCounty())
    req(thePlace())
    req(input$county)
    req(input$place)
    
    placeNum <-
      as.numeric(places[places$PLACENAME == input$place, 3])
    
    # print(paste("thePlace", thePlace()))
    # print(paste("theCounty", theCounty()))
    # print(paste("placeNum", placeNum))
    
    if(theCounty() == 36){
      map_place(geo = "state", geoID = 36)
    }else if (theCounty() != 36 & thePlace() == -1){
      map_place(geo = "county", geoID = theCounty())
    }else(
      map_place(geo = "place", geoID = placeNum)
      )
      
  }) # End working_map renderLeaflet
  
  output$ethnicityPlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_ethnicity(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_ethnicity(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_ethnicity(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  output$racePlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_race(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_race(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_race(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  output$commutePlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)

    
    if(theCounty() == 36) {
      grid.draw(plot_commute(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_commute(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_commute(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  
  output$vehiclePlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    if(theCounty() == 36) {
      grid.draw(plot_vehicle(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_vehicle(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_vehicle(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")

  
} #close server

# Run the application
shinyApp(ui = ui, server = server)