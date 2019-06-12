library(grid)
suppressPackageStartupMessages(library(leaflet))
library(neighborhoodNY)
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(shiny))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
#layla was here and ray is great


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
    ),
    br(),
    wellPanel( h3("Neighborhood Snapshot", style = "font-style: bold" ),
               br(),
               h5("Population: ", style = "display:inline; font-size: 18px; font-style: bold; color: #000000"),
               textOutput("totalpop", inline = TRUE), tags$style("#totalpop{font-size: 18px; font-style: bold; color: #F26B68}"),
               br(),
               br(),
               h5("Population Under 18: ", style = "display:inline; font-size: 18px; font-style: bold; color: #000000"),
               textOutput("under18", inline = TRUE), tags$style("#under18{font-size: 18px;  font-style: bold; color: #F26B68}"),
               br(),
               h5("Population Over 65: ", style = "display:inline; font-size: 18px; font-style: bold; color: #000000"),
               textOutput("over65", inline = TRUE), tags$style("#over65{font-size: 18px;  font-style: bold; color: #F26B68}"),
               br(),
               h5("Median Income: ", style = "display:inline; font-size: 18px; font-style: bold; color: #000000"),
               textOutput("income", inline = TRUE), tags$style("#income{font-size: 18px; font-style: bold; color: #F26B68}"),
               br(),
               h5("Unemployment Rate: ", style = "display:inline; font-size: 18px; font-style: bold; color: #000000"),
               textOutput("unemployment", inline = TRUE), tags$style("#unemployment{font-size: 18px; font-style: bold; color: #F26B68}")
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
                 column(6, plotOutput("englishPlot", height = 250)),
                 column(6, plotOutput("nativePlot", height = 250))
               ),
               br(),
               fluidRow(
                  column(6, plotOutput("educationPlot", height = 250))
               ),
               br(),
               fluidRow(
                 column(8, offset = 2, plotOutput("agePlot", height = 600))
               )
               
      ), # end tabPanel Demographics
      tabPanel("Housing", style = "background-color: #F2EEEC;",
               fluidRow(
                 # column(6, plotOutput("ownRentPlot", height = 250)),
                  column(6, plotOutput("vacancyPlot", height = 250))
               ),
               br(),
               fluidRow(
                 column(6, plotOutput("yearBuiltPlot", height = 250)),
                 column(6, plotOutput("urbanRuralPlot", height = 250))
               )
      ), # end tabPanel Housing
      tabPanel("Economic", style = "background-color: #F2EEEC;",
               fluidRow(
                 column(6, plotOutput("commutePlot", height = 250)),
                 column(6, plotOutput("povertyPlot", height = 250))
                 
               ),
               br(),
               fluidRow(
                 column(6, plotOutput("rentincomePlot", height = 250)),
                 column(6, plotOutput("vehiclePlot", height = 250))
               ),
               br(),
               fluidRow(
                 column(6, plotOutput("unemploymentPlot", height = 250))

               )
      ), # end tabPanel Economic
      
      tabPanel("Insurance", style = "background-color: #F2EEEC;",
               fluidRow(
                 
                 column(2,
                 radioButtons(
                   inputId = "insurance",
                   "Show insurance by:",
                   choices = list("Age and Sex" = 2,
                                  "Age" = 4,
                                  "Sex" = 6))),

                 column(6, plotOutput("insurancePlot", height = 500))
               ),
               br(),
               fluidRow(
                 
                 column(2,
                        radioButtons(
                          inputId = "insuranceType",
                          "Show insurance type by:",
                          choices = list("Age and Sex" = 2,
                                         "Age" = 4,
                                         "Sex" = 6))),

                 column(10, plotOutput("insuranceTypePlot", height = 500))
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
  
  
  output$thePlace <- reactive(
    thePlace()
  )
  
  #####NEIGHBORHOOD SNAPSHOT OUTPUTS  
  output$totalpop <-
    renderText({
      req(theCounty())
      req(thePlace())
      req(input$mode)
      
      
      if(theCounty() == 36) {
        plot_over65(geo = "state", geoId = 36)$data[4]
      }else if (theCounty() != 36 & thePlace() == -1){
        plot_over65(geo = "county", geoId = theCounty())$data[4]
      }else(
        plot_over65(geo = "place", geoId = thePlace())$data[4]
      )
    })
  
  
  output$under18 <-
    renderText({
      req(theCounty())
      req(thePlace())
      req(input$mode)
      
      
      if(theCounty() == 36) {
        paste0(plot_over65(geo = "state", geoId = 36)$data[2], "%")
      }else if (theCounty() != 36 & thePlace() == -1){
        paste0(plot_over65(geo = "county", geoId = theCounty())$data[2], "%")
      }else(
        paste0(plot_over65(geo = "place", geoId = thePlace())$data[2], "%")
      )
    })
  
  output$over65 <-
    renderText({
      req(theCounty())
      req(thePlace())
      req(input$mode)
      
      
      if(theCounty() == 36) {
        paste0(plot_over65(geo = "state", geoId = 36)$data[1], "%")
      }else if (theCounty() != 36 & thePlace() == -1){
        paste0(plot_over65(geo = "county", geoId = theCounty())$data[1], "%")
      }else(
        paste0(plot_over65(geo = "place", geoId = thePlace())$data[1], "%")
      )
    })
  
  output$income <-
    renderText({
      req(theCounty())
      req(thePlace())
      req(input$mode)
      
      
      if(theCounty() == 36) {
        plot_income(geo = "state", geoId = 36)$data[1]
      }else if (theCounty() != 36 & thePlace() == -1){
        plot_income(geo = "county", geoId = theCounty())$data[1]
      }else(
        plot_income(geo = "place", geoId = thePlace())$data[1]
      )
    })
  
  output$unemployment <-
    renderText({
      req(theCounty())
      req(thePlace())
      req(input$mode)
      
      
      if(theCounty() == 36) {
        paste0(round((1 - plot_unemployment(geo = "state", 
                                            geoId = 36)$data[1]) * 100 ),
               "%")
      }else if (theCounty() != 36 & thePlace() == -1){
        paste0(round((1 - plot_unemployment(geo = "county", 
                                            geoId = theCounty())$data[1]) * 100 ),
               "%")
      }else(
        paste0(round((1 - plot_unemployment(geo = "place", 
                                            geoId = thePlace())$data[1]) * 100 ),
               "%")
      )
    })
  
  #####LEAFLET MAP
  
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
  
  #####CENSUS VARIABLES WAFFLE/BAR OUTPUTS
  
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
  
  output$englishPlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_english(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_english(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_english(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  output$nativePlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_native(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_native(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_native(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  output$educationPlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_education(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_education(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_education(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  output$agePlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      plot_age(geo = "state", geoId = 36)
    }else if (theCounty() != 36 & thePlace() == -1){
      plot_age(geo = "county", geoId = theCounty())
    }else(
      plot_age(geo = "place", geoId = thePlace())
    )
    
    
  }, bg = "transparent")
  
  output$vacancyPlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_vacancy(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_vacancy(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_vacancy(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  output$yearBuiltPlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_year_built(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_year_built(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_year_built(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  output$urbanRuralPlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_urbanRural(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_urbanRural(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_urbanRural(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
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
  
  output$povertyPlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_poverty(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_poverty(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_poverty(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  output$rentincomePlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    
    if(theCounty() == 36) {
      grid.draw(plot_rentincome(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_rentincome(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_rentincome(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
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
  
  output$unemploymentPlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    if(theCounty() == 36) {
      grid.draw(plot_unemployment(geo = "state", geoId = 36)[[ as.numeric(input$mode) ]])
    }else if (theCounty() != 36 & thePlace() == -1){
      grid.draw(plot_unemployment(geo = "county", geoId = theCounty())[[ as.numeric(input$mode) ]])
    }else(
      grid.draw(plot_unemployment(geo = "place", geoId = thePlace())[[ as.numeric(input$mode) ]])
    )
    
    
  }, bg = "transparent")
  
  output$insurancePlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    if(theCounty() == 36) {
      plot_insurance(geo = "state", geoId = 36)[[as.numeric(input$insurance)]]
    }else if (theCounty() != 36 & thePlace() == -1){
      plot_insurance(geo = "county", geoId = theCounty())[[as.numeric(input$insurance)]]
    }else(
      plot_insurance(geo = "place", geoId = thePlace())[[as.numeric(input$insurance)]]
    )
    
    
  }, bg = "transparent")
  
  output$insuranceTypePlot <- renderPlot({
    req(theCounty())
    req(thePlace())
    req(input$mode)
    
    if(theCounty() == 36) {
      plot_insurance_type(geo = "state", geoId = 36)[[as.numeric(input$insuranceType)]]
    }else if (theCounty() != 36 & thePlace() == -1){
      plot_insurance_type(geo = "county", geoId = theCounty())[[as.numeric(input$insuranceType)]]
    }else(
      plot_insurance_type(geo = "place", geoId = thePlace())[[as.numeric(input$insuranceType)]]
    )
    
    
  }, bg = "transparent")

  
} #close server

# Run the application
shinyApp(ui = ui, server = server)