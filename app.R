
library(shiny)
library(dplyr)
library(googleway)
library(rsconnect)
library(stringr)
library(emojifont)
library(ggplot2)
library(ggmap)
library(shinythemes)
library(wordcloud2)
library(png)
library(wordcloud2)
library(wordcloud)
library(memoise)

api_key <- ""

#Read and clean CSA data
df1 <- read.csv("csa.csv")
result<-str_match(df1$location_address,"(.+), (.+), (.+) (.+)")[ ,-1]
df1$state <- result[,3]
df1<-data.frame(df1$listing_id, df1$location_address, df1$listing_name, df1$location_x, df1$location_y, df1$state, df1$products, df1$season_month_1, df1$season_month_2, df1$season_month_3,
                df1$season_month_4, df1$season_month_5, df1$season_month_6, df1$season_month_7, df1$season_month_8, df1$season_month_9, df1$season_month_10, df1$season_month_11, df1$season_month_12)
state_abbr <- c("MT", "NY", "ID", "MA", "FL", "NC", "MI", "VA 24072", "IA", "WA", "WI", "KY", "CA", "NH", "DE", "OH", "TX", 
                "Southampton NJ", "PA 16923", "IL", "NJ", "VA", "North Carolina 27377")
state_full <- c("Montana", "New York", "Idaho", "Massachusetts", "Florida", "North Carolina", 
                "Michigan", "Virginia", "Iowa", "Washington", "Wisconsin", "Kentucky", "California",
                "New Hampshire", "Delaware", "Ohio", "Texas", "New Jersey", "Pennsylvania", "Illinois", "New Jersey", "Virginia",
                "North Carolina")
lookup_table <- data.frame(abbr = state_abbr, full = state_full)
df1$df1.state <- ifelse(str_detect(df1$df1.state, paste(state_abbr, collapse = "|")),
                        lookup_table[match(df1$df1.state, lookup_table$abbr), "full"], df1$df1.state)
df1 <- df1[!is.na(df1$df1.state),]

df1 <- df1[df1$df1.state != "California,", ]

california_data <- subset(df1, df1.state == "California")

alabama_data <- subset(df1, df1.state == "Alabama")


#Read and clean farmers market data
df2 <- read.csv("farmersmarket.csv")
result1<-str_match(df2$location_address,"(.+), (.+), (.+) (.+)")[ ,-1]
df2$state <- result1[,3]
df2<-data.frame(df2$listing_id, df2$location_address, df2$listing_name, df2$location_x, df2$location_y, df2$state)
df2$df2.state <- ifelse(str_detect(df2$df2.state, paste(state_abbr, collapse = "|")),
                        lookup_table[match(df2$df2.state, lookup_table$abbr), "full"], df2$df2.state)
df2 <- df2[!is.na(df2$df2.state),]
df2 <- df2[df2$df2.state != "California,", ]



#Read and clean agritourism market data
df3 <- read.csv("agtourism.csv")
df3 <- df3[!is.na(df3$df3.state),]
df3 <- df3[df3$df3.state != "California,", ]
df3 <- df3[df3$df3.state != "Colorado", ]


ui1 <- fluidPage(navbarPage(title = div(
  style = "font-size: 40px; font-weight: bold; color:black;",
  "Your Guide to Local Agriculture in America"
),
tags$style(
  HTML(
    "
      .navbar {
        background-color: #ADD8E6;
      }
      "
  )
),
tabsetPanel(tabPanel("Find Your Local Farmer", tags$head(tags$style(".my-main-panel { margin-bottom: 200px; }",
    HTML("
        body {
          background-color: lightblue;
        }
      ")
  )
),
tags$div(style = "height: 10px;"),
p(emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"), 
  emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"),
  emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"),
  emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),
  style = "font-size: 20px;"),
tags$div(style = "height: 20px;"),
fluidRow(
  style = "background-color: lightblue;",
         column(
           width = 3,
           selectInput(inputId = "inputState", label = "Select state:", multiple = TRUE, choices = sort(df1$df1.state), selected = "Alabama"),
           selectInput("type_selector", "Select a type:", choices = c("Community Supported Agriculture", "AgriTourism", "Farmers' Markets"))
         ),
         column(
           width = 9,
           uiOutput(outputId = "map")
         ),
  tags$div(style = "height: 450px;"),
         mainPanel(style = "background-color: lightblue;",
           tags$strong(style = "font-size: 21px; color:black;","WELCOME TO THE MOST USEFUL GUIDE TO LOCAL AGRICULTURE ACROSS AMERICA!"),
           HTML("<br><br>"),
           p("Do you want to support the local farmers' in your area? 
      Are you interested in getting out in the country for a bit? 
      Then this app is for you! it uses USDA data to help you navigate your farming experience. 
        To use this app, simply select your state and what sort of local agricultural experience you're looking for. 
        You'll have the option between Community Supported Agriculture (which is a network of farmers that offer regular deliveries to the community),
        options for AgriTourism, and farmers' markets. I hope you enjoy using this app as much as I did making it!"), width = 12
         ))),
tabPanel("What's Available at CSAs in Your State?", tags$div(style = "height: 10px;"),
         p(emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"), 
           emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"),
           emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"),
           emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),
           style = "font-size: 20px;"),
         fluidRow(
         selectInput(inputId = "SecondState", label = "Select state:", multiple = TRUE, choices = sort(df1$df1.state), selected = "Alabama"),
         column(
           width = 9,
           wordcloud2Output(outputId = "wordcloud"),
           HTML("<br><br>"),
           p("Note that this wordcloud only uses data from CSAs. No data is available for Farmers Markets and AgriTourism. Drag cursor over word to see how often it appears in the data.")
         ))
),
tabPanel("When are CSAs Available in Your State?", p(emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"), 
                                                     emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"),
                                                     emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"),
                                                     emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),
                                                     style = "font-size: 20px;"), selectInput(inputId = "ThirdState", label = "Select state:", multiple = TRUE, choices = sort(df1$df1.state), selected = "Alabama"),
plotOutput(outputId = "barplot")),
tabPanel("About the Author", p(emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"), 
                                 emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"),
                                 emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),emoji("sheep"),emoji("peach"),
                                 emoji("smile"), emoji("orange"),emoji("tomato"),emoji("apple"),emoji("cow"),emoji("corn"),emoji("pear"),
                                 style = "font-size: 20px;"),   tags$div(style = "height: 50px;"),      column(width = 9,
                                                                                                               # Add user information, such as name and description
                                                                                                               h2("Matt Woolf"),
                                                                                                               p(HTML(paste("This app was created by Matt Woolf, an economics PhD student at the Australian National University,
                                                                                                                            Data Science Fellow at the New York Data Science Academy, and farmer. Please visit my website", a("here", href = "https://www.mattcharleswoolf.com/", target = "_blank"), " or GitHub",
                                                                                                                            a("here", href = "https://github.com/mattcwoolf", target = "_blank"), "to learn more about me.")))
                                 ), img(src = "image.png", align = "right", width = "20%", height = "20%"))
)
)
)

server1 <- function(input, output) {
  
 # map_message <- reactive({
#    if (input$type_selector == "AgriTourism" && input$inputState == "Alabama" || 
 #       input$type_selector == "AgriTourism" && input$inputState == "California"
  #      || input$type_selector == "AgriTourism" && input$inputState == "Alaska" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Arizona" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Arkansas" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Colorado" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Connecticut" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Delaware" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "District of Columbia" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Georgia" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Hawaii" ||
  #     input$type_selector == "AgriTourism" && input$inputState == "Idaho" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Illinois" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Indiana" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Kansas" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Kentucky" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Louisiana" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Maine" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Maryland" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Massachusetts" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Minnesota" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Mississippi" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Missouri" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Nebraska" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Nevada" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "New Hampshire" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "New Mexico" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "North Dakota" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Ohio" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Oklahoma" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Oregon" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Pennsylvania" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Rhode Island" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "South Carolina" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "South Dakota" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Utah" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Vermont" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Virginia" ||
  #      input$type_selector == "Community Supported Agriculture" && input$inputState == "Virginia" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Washington" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "West Virginia" ||
  #      input$type_selector == "AgriTourism" && input$inputState == "Wyoming") {

      
   #   paste0("no data")
      
      
      
  #  } else {
  #    paste0(" ")
  #  }
  #})
  
  
  # Render map message  -----------------------------------------------------------------------------

  
  data1 <- reactive({
    df1 %>%
      filter(df1.state %in% input$inputState)%>%
      mutate(INFO = paste0(df1.listing_name, " | ", df1.location_address))
  })
  
  data2 <- reactive({
    df2 %>%
      filter(df2.state %in% input$inputState)%>%
      mutate(INFO = paste0(df2.listing_name, " | ", df2.location_address))
  })
  
  data3 <- reactive({
    df3 %>%
      filter(df3.state %in% input$inputState)%>%
      mutate(INFO = paste0(df3.listing_name, " | ", df3.location_address))
  })
  
  
  

  output$map <- renderUI({
    
  
    if (input$type_selector == "Community Supported Agriculture") {
      renderGoogle_map(
      google_map(data = data1(), key = api_key) %>%
      add_markers(lat = "df1.location_y", lon = "df1.location_x", mouse_over = "INFO")
      )
  } else if (input$type_selector == "Farmers' Markets") {
      renderGoogle_map(
      google_map(data = data2(), key = api_key) %>%
        add_markers(lat = "df2.location_y", lon = "df2.location_x", mouse_over = "INFO")
      )
    }
    
    else if (input$type_selector == "AgriTourism" && input$inputState == "Tennessee" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
      
    }
    
    else if (input$type_selector == "AgriTourism" && input$inputState == "Montana" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }
    
    else if (input$type_selector == "AgriTourism" && input$inputState == "Florida" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }
    
    else if (input$type_selector == "AgriTourism" && input$inputState == "Wisconsin" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }
    
    else if (input$type_selector == "AgriTourism" && input$inputState == "New York" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }
    
    else if (input$type_selector == "AgriTourism" && input$inputState == "New Jersey" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }
    
    else if (input$type_selector == "AgriTourism" && input$inputState == "Texas" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }
    
    else if (input$type_selector == "AgriTourism" && input$inputState == "Iowa" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }

    
    else if (input$type_selector == "AgriTourism" && input$inputState == "North Carolina" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }

    else if (input$type_selector == "AgriTourism" && input$inputState == "Michigan" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }
    
    else if (input$type_selector == "AgriTourism" && input$inputState == "Wisconsin" ) {
      renderGoogle_map(
      google_map(data = data3(), key = api_key) %>%
        add_markers(lat = "df3.location_y", lon = "df3.location_x", mouse_over = "INFO")
      )
    }
    else{
      tags$img(src = "WHOOPS.png", width = "100%", height = "100%")
    }
  }
  )
  
  output$wordcloud <- renderWordcloud2({
    # Filter the data by the selected state
    state_data <- subset(df1, df1.state == input$SecondState)
    
    # Split the crop data column into a list of individual crops
    crop_list <- strsplit(state_data$df1.products, ";")
    
    # Combine the crop list into a single character vector
    crop_text <- unlist(crop_list)
    
    # Create a frequency table of the crop text
    crop_freq <- table(crop_text)
    
    # Create the wordcloud using wordcloud2
    wordcloud2(data = crop_freq, color = "random-light",
               size = 0.8, minSize = 0, gridSize = 12,
               backgroundColor = "lightblue")
  })
  
  
  output$barplot <- renderPlot({
    # Filter the data by the selected state
    barchartdata <- subset(df1, df1.state == input$ThirdState)
    
    numbers<-data.frame(January = sum(barchartdata$df1.season_month_1, na.rm = TRUE), February = sum(barchartdata$df1.season_month_2, na.rm = TRUE), March = sum(barchartdata$df1.season_month_3, na.rm = TRUE),
                              April = sum(barchartdata$df1.season_month_4, na.rm = TRUE), May = sum(barchartdata$df1.season_month_5, na.rm = TRUE), June = sum(barchartdata$df1.season_month_6, na.rm = TRUE), 
                              July = sum(barchartdata$df1.season_month_7, na.rm = TRUE), August = sum(barchartdata$df1.season_month_8, na.rm = TRUE), September = sum(barchartdata$df1.season_month_9, na.rm = TRUE),
                              October = sum(barchartdata$df1.season_month_10, na.rm = TRUE), November = sum(barchartdata$df1.season_month_11, na.rm = TRUE), December = sum(barchartdata$df1.season_month_12, na.rm = TRUE))
    
    par(bg = "lightblue")
     barplot(as.matrix(numbers), 
            xlab = "Month", 
            ylab = "Number of CSA Farms Open", 
            col = "black", 
            main = "Farm Openings by Month",
            border = NA)
    
  })
  
}


shinyApp(ui = ui1, server = server1)
