library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)
library(DT)
library(imager)
library(readr)
library(plotly)

timezone_picture<-load.image("time.png")
countries<-read.csv("tdat_orig.csv",  encoding = "UTF-8")
countries$Summertime <- NULL
countries$latitude<-as.numeric(format(format(countries$latitude, digits = 2)))
countries$longitude<-as.numeric(format(countries$longitude, digits = 2))
countries <- countries %>% arrange(country)
sun_time_vectorized<-function(latitude, longitude, timezone) {
  require(lubridate)
  require(suncalc)
  # Create a sequence of dates for the whole year
  start_date <- as.Date(paste0(year(now()), "-01-01"))
  end_date <- as.Date(paste0(year(now()), "-12-31"))
  all_dates <- seq(start_date, end_date, by = "1 day")
  
  # Get the sunrise and sunset times for all dates and the given location
  sun_times <- suncalc::getSunlightTimes(all_dates, latitude, longitude, tz = "UTC")
  sunrise <- sun_times$sunrise
  sunset <- sun_times$sunset
  
  # Calculate the length of daylight hours in hours and minutes
  daylight_length <- difftime(sunset, sunrise, units = "hours")
  daylight_hours <- floor(as.numeric(daylight_length))
  daylight_minutes <- round((as.numeric(daylight_length) - daylight_hours) * 60)
  
  # Create the result dataframe
  Time <- data.frame(
    Date = all_dates,
    Sunrise = sunrise,
    Sunset = sunset#,
    #Daylight = paste0(daylight_hours, "h ", daylight_minutes, "m")
  )
  
  Time <- Time %>% 
    mutate(Daylight = as.POSIXct(Sunset) - as.POSIXct(Sunrise))
          #Date = as.Date(Date, origin = paste0(year(Sys.Date()), "-01-01")))
  
  Time$Daylight <- round(Time$Daylight, digits = 2)
  Time$Sunrise <- strftime(Time$Sunrise, format = "%H:%M")
  Time$Sunset <- strftime(Time$Sunset, format = "%H:%M")
  Time$Day <- as.integer(strftime(Time$Date, format = "%j"))
  Time <- Time %>% 
    select(Day, Sunrise, Sunset, Daylight, Date)
  return(Time)
  
}

Table_function <-function(){
   list(
    dom = 'BRfrltpi',
    buttons =
      list('copy', 'print', list(
        extend = 'collection',
        buttons = list(
          list(extend = 'csv', filename = "All Times", 
               exportOptions = list(modifier = list(page = "all"))),
          list(extend = 'excel', filename = "All Times"),
          list(extend = 'pdf', filename = "All Times")),
        text = 'Download'
      )),
    columnDefs = list(list(className = 'dt-center', targets="_all")),
    autoWidth=TRUE,
    rownames= FALSE,
    pageLength = 1000,
    deferRender = TRUE,
    scrollY = 600,
    scroller = TRUE)
}

####################
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(plotly)

ui <- dashboardPage(
  skin = "blue",  # You can change the skin color to your preference
  dashboardHeader(title = "Lumina"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphics", tabName = "graphics", icon = icon("line-chart")),
      menuItem("All Dates", tabName = "tables", icon = icon("table")),
      menuItem("Timezones", tabName = "timezones", icon = icon("clock"))
    ),
    dateInput("date", "Date",
              format = "dd-mm-yyyy",
              value = Sys.Date()),
    tags$style(HTML(".datepicker {z-index:99999 !important;}")),
    radioGroupButtons(
      inputId = "interface",
      label = "",
      choices = c("Capital", "Manual"),
      selected = "Manual",
      status = "primary",
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon"))
    ),
    conditionalPanel(
      condition = "input.interface == 'Capital'",
      uiOutput("capital_select")
    ),
    conditionalPanel(
      condition = "input.interface == 'Manual'",
      sliderInput("latitude", "Latitude",
                  min = -90 , max = 90, value = 0)),
    conditionalPanel(
      condition = "input.interface == 'Manual'",
      sliderInput("longitude", "Longitude",
                  min = -180, max = 180, value = 0)
      #uiOutput("latitude_select"),
      #uiOutput("longitude_select")
    )
  ),
  dashboardBody(
    tabItems(
      # Graphics tab
      tabItem(tabName = "graphics",
              fluidRow(
                box(
                  collapsible = T,
                  title = "Map",
                  status = "primary",
                  leafletOutput("Map"),
                  width = 6
                ),
                box(
                  collapsible = T,
                  title = "",
                  status = "success",
                  plotlyOutput("Plot")
                )
              ),
              box(
                collapsible = T,
                title = "",
                status = "primary",
                tableOutput("closest")
              ),
              box(
                collapsible = T,
                title = "",
                status = "info",
                dataTableOutput("sentence"),
                width = 6
              )
      ),
      # All sunrises and sunsets tab
      tabItem(tabName = "tables",
              box(
                collapsible = T,
                title = "All sunrises and sunsets for your location",
                status = "warning",
                dataTableOutput("tables")
              )
      ),
      # Timezones tab
      tabItem(tabName = "timezones",
              box(
                collapsible = T,
                title = "Countries",
                status = "info",
                dataTableOutput("countries")
              ),
              box(
                collapsible = T,
                title = "Timezones",
                status = "danger",
                plotOutput("timezones_output")
              )
      )
    )
  )
)



server <- function(input, output,session) {
  session$onSessionEnded({
    #print("Stop!")
    stopApp
  })
  ### UI ###
  
  output$capital_select<-renderUI({
    if(input$interface == "Capital"){
      #selectInput("capital", "Country",
      #            sort(unique(countries$country)), multiple = TRUE, selected = "Falkland Islands")
      country <- countries$country
      capitals <- countries$capital
       pickerInput("capital", "Country",
                  choices =  country, multiple = T,
                  selected = "Falkland Islands",
                  options = pickerOptions(
                    size = 10,
                    `actions-box` = TRUE,
                    `live-search` = TRUE),
                  choicesOpt = list(
                    subtext = c(capitals)))
      }else{NULL}
  })
  #output$latitude_select<-renderUI({
  #  if(input$interface == "Manual"){
  #    sliderInput("latitude", "Latitude",
  #                min = -90 , max = 90, value = 0)}else{NULL}
  #})
  #output$longitude_select<-renderUI({
  #  if(input$interface == "Manual"){
  #    sliderInput("longitude", "Longitude",
  #                min = -180, max = 180, value = 0)}else{NULL}
  #})
  #
  #### Lat and Long ###
  
  lat <- reactive({
    if(input$interface == "Manual"){input$latitude}else
      if(input$interface == "Capital"){target_capital()$latitude}else{NULL}
  })
  lng <- reactive({
    if(input$interface == "Manual"){input$longitude}else
      if(input$interface == "Capital"){target_capital()$longitude}else{NULL}
  })
  
  ###
  
  date <- reactive({input$date})
  timezone <- reactive({target()$timezone})
  
  ### Target capital for Timezone detection ###
  
  target_capital<-reactive({
    target_capital<-countries %>% 
      filter(country %in% input$capital)
  })
  
  ### Today ###
  
  time_today<-reactive({
    Today<-as.POSIXlt(input$date)
    Today<-Today$yday
    Time_today<- Time_manual() %>%
      mutate(Day = Day-1) %>% 
      filter(Day == Today)
  })
  time_today2<-reactive({
    Today<-as.POSIXlt(input$date)
    Today<-Today$yday
  })

  ### Closest match for Map ###
  
  target<-reactive({
    fun_target<-function(){
      library(dplyr)
      set1<-data.frame(lat(), lng())
      names(set1)[1]<-"latitude"
      names(set1)[2]<-"longitude"
      set2<-countries %>% select(latitude, longitude)
      set3<-rbind(set2,set1)
      set3[,1]<-as.numeric(set3[,1])
      set3[,2]<-as.numeric(set3[,2])
      library(RANN)
      closest <- nn2(data=set3, k=2)[[1]]
      closest <- as.data.frame(closest)
      result<-closest[length(closest$V1),2]
      filtered<-countries[result,]
      return(filtered)}
    if(input$interface == "Manual"){fun_target()}else
      if(input$interface == "Capital"){countries %>% filter(country %in% input$capital)}else{NULL}
  })
  observe(print(input$interface))
  
  ### Map ###
  
  map<-reactive({
    if(is.null(input$capital)){NULL}
    if(is.null(input$latitude)){NULL}
    
    icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion')
    icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
    icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = 'black')
    
    
    if(input$interface == "Manual"){
      countries %>% 
        leaflet() %>%
        setView(lat = lat(), lng = lng(), zoom = 5) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        addProviderTiles(providers$Stamen.Watercolor, group = "Water Color") %>% 
        #addProviderTiles(provider =  providers[[provider()]], group = providers[[provider()]]) %>% 
        addLayersControl(baseGroups = c("World Imagery", "Water Color", "Toner Lite")) %>%
        addAwesomeMarkers(#latitude = latitude, longitude = longitude,
          icon = icon.glyphicon,
          label = countries$capital,
          #clusterOptions = markerClusterOptions(),
          popup = paste(as.character(countries$country),"|", as.character(countries$capital),"|", "Timezone:", countries$offset, sep = "\n")) %>% 
        addMiniMap(
          toggleDisplay = TRUE,
          tiles = providers$Stamen.TonerLite
        ) %>% 
        addAwesomeMarkers(lng= lng(), lat= lat(),
                          popup="You are here", 
                          icon = icon.ion
                          )
      }else
                            
                            if(input$interface == "Capital"){
                              target_capital() %>% 
                                leaflet() %>%
                                setView(lat = target_capital()$latitude[1], lng = target_capital()$longitude[1], zoom = 5) %>%
                                addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
                                addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
                                addProviderTiles(providers$Stamen.Watercolor, group = "Water Color") %>% 
                                #addProviderTiles(provider =  providers[[provider()]], group = providers[[provider()]]) %>% 
                                addLayersControl(baseGroups = c("World Imagery", "Water Color", "Toner Lite")) %>%
                                addAwesomeMarkers(#latitude = latitude, longitude = longitude,
                                  icon = icon.glyphicon,
                                  label = target_capital()$capital,
                                  #clusterOptions = markerClusterOptions(),
                                  popup = paste(as.character(target_capital()$country),"|", as.character(target_capital()$capital),"|", "Timezone:", target_capital()$offset, sep = "\n")) %>% 
                                addMiniMap(
                                  toggleDisplay = TRUE,
                                  tiles = providers$Stamen.TonerLite
                                )
                            }
    
  })
  
  ### Time manual ###
  
  Time_manual<-reactive({
    sun_time_vectorized(lat(), lng(), timezone())
  })
  
  Time <- reactive({
    build_dataframe<-function(){
      target<-unlist(input$capital)
      countr<-countries %>% filter(country %in% target)
      Time4<-data.frame()
      Time<-data.frame()
      for(i in 1:length(countr$country)){
        Time<-sun_time_vectorized(countr[i,5],countr[i,6],countr[i,4])
        Time$country<-countr[i,1]
        Time4<-rbind(Time, Time4)
      }
      Time4<-as.data.frame(Time4)
      return(Time4)}
    
    if(input$interface == "Manual"){sun_time_vectorized(lat(), lng(), timezone())}else
      if(input$interface == "Capital"){build_dataframe()}
  })
  
  Time_capital<-reactive({
    build_dataframe<-function(){
      target<-unlist(input$capital)
      countr<-countries %>% filter(country %in% target)
      Time4<-data.frame()
      Time<-data.frame()
      for(i in 1:length(countr$country)){
        Time<-sun_time_vectorized(countr[i,5],countr[i,6],countr[i,4])
        Time$country<-countr[i,1]
        Time4<-rbind(Time, Time4)
      }
      Time4<-as.data.frame(Time4)
      return(Time4)}
    #if(is.null(input$interface)){NULL}
    #if(is.null(input$capital)){NULL}
    #if(is.null(input$latitude)){NULL}
    #if(is.null(input$longitude)){NULL}
    if(input$interface == "Capital"){build_dataframe()
    }else{NULL}
  })
  
  ### Outputs ###
  
  output$capital_times<-renderDataTable({Time_capital()})
  output$sentence<-renderDataTable(Time() %>% filter(Date == input$date))
  
  output$closest<-renderTable(target(),rownames= FALSE,filter = 'top',
    extensions = c('Buttons',"Scroller","FixedColumns"),options =  Table_function())
  
  output$tables <-renderDataTable(
    Time(),rownames= FALSE,filter = 'top',
    extensions = c('Buttons',"Scroller","FixedColumns"),options =  Table_function())
  
  
  
  output$Map<- renderLeaflet({map()})
  
  output$Plot<-renderPlotly({
    if(input$interface == "Manual"){
      ggplot(data=Time_manual(), aes(x=Date, y=Daylight))+
        geom_point(size = 0.1, alpha = 0.5)+
        #geom_smooth(method = "loess", span = 0.1, se = F)+
        geom_line()+
        geom_point(data=time_today(), aes(x=Date, y=Daylight), color = "red", size = 5)+
        theme_minimal()+
        #facet_wrap(~country)+
        theme(legend.position = "none")
    }else
      if(is.null(input$interface[1])){NULL}else 
        if(input$interface == "Capital"){
          ggplot()+
            geom_point(data=Time_capital(), aes(x=Date, y=Daylight, color = country), size = 0.1, alpha = 0.5)+
            geom_line(data=Time_capital(), aes(x=Date, y=Daylight, color = country))+
            geom_vline(aes(xintercept = as.numeric(input$date)), color = "red")+
            #geom_bar(data=Time_capital(), aes(x=Sys.Date(), y=max(Daylight)), stat = "identity", color="red")+
            theme_minimal()+
            #facet_wrap(~country)+
            theme(legend.position = "none")
        }
  })
  
  output$timezones_output<-renderPlot({plot(timezone_picture)})
  output$countries<-renderDataTable(countries,rownames= FALSE,filter = 'top',
                                    extensions = c('Buttons',"Scroller","FixedColumns"),options =  Table_function())
}

shinyApp(ui, server)
