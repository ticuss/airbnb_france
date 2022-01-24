library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(lubridate)
library(leaflet)
library(shinydashboard)
library(tm)

#### dataset ####
listing <- read_csv('listing_paris.csv', na = c("", "NA", "0", NaN))
listing <- listing %>% 
    mutate(
        income_monthly = round(price*availability_365/12),
        highly_available = availability_365 >=60,
        freq_review = (today() - last_review) <=180
    )

listing_map <- listing %>% 
    select(id, neighbourhood, longitude, latitude, room_type, price, number_of_reviews, availability_365, income_monthly) %>% 
    group_by(neighbourhood, room_type) %>% 
    summarise(
        nb_bnb = n(),
        price = mean(price, na.rm = T),
        nb_reviews = mean(number_of_reviews, na.rm = T),
        availability_365 = mean(availability_365, na.rm = T),
        income_monthly = mean(income_monthly, na.rm = T),
        longitude = median(longitude),
        latitude = median(latitude)
    )

col_def <- tibble(
    'neighbourhood_group'="Quartier de l'hébergement", 
    'room_type'= "Type de logement",
    'highly_available' = 'True/False: la location est disponible au moins 60 jours par an',
    'freq_review' = 'True/False: le dernier avis date moins de 6 mois',
    'price'= 'Prix - moyenne', 
    'minimum_nights' = 'Nombre de nuits minimum',
    'number_of_reviews' = "Nombre d'avis",
    'reviews_per_month' = "Nombre d'avis par mois - moyenne",
    'availability_365' = "Nombre de jour disponible à la location par an", 
    'income_monthly'= "Revenue par mois - moyenne", 
    'calculated_host_listings_count' = 'Nombre de logement par hôte'
)




# # Create a corpus based on the text
# docs <- Corpus(VectorSource(text))

# #Clean the text data using tm library
# docs <- docs %>%
#     tm_map(removeNumbers) %>%
#     tm_map(removePunctuation) %>%
#     tm_map(stripWhitespace)
# docs <- tm_map(docs, content_transformer(tolower))
# docs <- tm_map(docs, removeWords, stopwords("english"))

# Create a dataframe containing each word in your first column and their frequency in the second column.
# dtm <- TermDocumentMatrix(docs) 
# matrix <- as.matrix(dtm) 
# words <- sort(rowSums(matrix),decreasing=TRUE) 
# df_wordcloud <- data.frame(word = names(words),freq=words)

############## UI ############## 
    
main_sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem('Dashboard', icon = icon('chart-pie'),
                     menuSubItem('Bordeaux et ses alentours', tabName = 'tab_bdx'),
                     menuSubItem('Zoom sur les quartiers', tabName = 'tab_zones')
                     ),
            menuItem('Dataset', icon = icon('chart-bar'),
                      menuSubItem('Demo Dataset', tabName = 'tab_demo')
                      # menuSubItem('Variables Quantitatives', tabName = 'tab_quantitative'),
                      # menuSubItem('Variables Qualitatives', tabName = 'tab_qualitative'),
                      # menuSubItem('Zoom sur les variables', tabName = 'tab_2vars')
            )
    )
)
    
main_body <- dashboardBody(
        tabItems( 
            # Page 1
            tabItem( # Tab1: Demo
                tabName = 'tab_demo',
                fluidRow(
                    column(3,
                           numericInput("obs", "Nombre de lignes:", 10)
                    )),
                div(style = 'overflow-x: scroll',tableOutput("demo"))
            ),

 # Page 2
        tabItem( # Tab 1: Bordeaux et ses alentours
            tabName = 'tab_bdx',
            fluidRow(
                valueBox(round(nrow(listing)), "Nombre de logement", icon = icon("airbnb"), color = "blue"),
                valueBox(round(mean(listing$price, na.rm = T)), "Prix - moyenne", icon = icon("euro-sign"), color = "light-blue"),
                valueBox(round(mean(listing$availability_365, na.rm = T)), "Disponibilité par an - moyenne", icon = icon("door-open"), color = "aqua"),
                valueBox(round(mean(listing$number_of_reviews, na.rm = T)), "Nombre d'avis - moyenne", icon = icon("star"), color = "olive"),
                valueBox(round(mean(listing$minimum_nights, na.rm = T)), "Nombre de nuits minimum - moyenne", icon = icon("bed"), color = "green"),
                valueBox(round(mean(listing$income_monthly, na.rm = T)), "Revenue Mensuel - moyenne", icon = icon("credit-card"), color = "lime")
            ),
            fluidRow(box(leafletOutput("map_bdx"), status = "danger", title = 'Bordeaux et ses alentours', width = 8),
                     box(plotOutput("room_type_bdx"), status = "warning", title = "Répartition des types d'hébergements", width = 4)
                         ),
            # fluidRow(box(plotOutput('wordcloud'), status = "danger", title = 'Les avis', width = 4),
            #          box(plotOutput('sentiment_histo'), status = "warning", title = 'Les sentiments', width = 8)
            #          )
                     
        ),
        tabItem( # Filtre par zones
            tabName = 'tab_zones',
            fluidRow(
                column(3,
                       selectInput("zone",
                                   "Choisir le zone :",
                                   choices = unique(listing$neighbourhood)
                       )
                )),
            fluidRow(
                valueBox(textOutput('nb_bnb'), "Nombre d'hébergements", icon = icon("airbnb"), color = "blue"),
                valueBox(textOutput('price'), "Prix - moyenne", icon = icon("euro-sign"), color = "light-blue"),
                valueBox(textOutput('available'), "Disponibilité par an - moyenne", icon = icon("door-open"), color = "aqua"),
                valueBox(textOutput('rate'), "Nombre d'avis - moyenne", icon = icon("star"), color = "olive"),
                valueBox(textOutput('night'), "Nombre de nuits minimum - moyenne", icon = icon("bed"), color = "green"),
                valueBox(textOutput('income'), "Revenue Mensuel - moyenne", icon = icon("credit-card"), color = "lime")
            ),
            fluidRow(box(leafletOutput("map"), status = "danger", title = 'Bordeaux et ses alentours', width = 8),
                     box(plotOutput("room_type"), status = "warning", title = "Répartition des types d'hébergements", width = 4)
            )

        )
    )
)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = 'Airbnb à Bordeaux'),
                    main_sidebar, 
                    main_body
)

############## SERVER ##############  
server <- function(input, output) {
    # Tab 1: Demo dataset
    output$demo <- renderTable({
        head(listing, n = input$obs)
    })
    
    # Tab 5: Map Bordeaux
    output$map_bdx <- renderLeaflet({
        leaflet(data=listing_map) %>%
        addTiles() %>% 
        setView(lng = -0.5759, lat = 44.84, zoom = 5) %>% 
        addMarkers(lng=~longitude, lat=~latitude,
                   popup = ~paste(
                       "<b>", neighbourhood, "</b><br/>",
                       "Type: ", room_type, "<br/>",
                       "count: ", as.character(nb_bnb), "<br/>",
                       "price: ", round(price), "<br/>",
                       "nb_reviews: ", round(nb_reviews), "<br/>",
                       "available_per_year: ", round(availability_365), "<br/>"
                   ), 
                   clusterOptions = markerClusterOptions())
    })
    
    output$room_type_bdx <- renderPlot({
        listing %>% 
            ggplot()+
            geom_bar(aes(x=room_type), fill='sienna1')+
            theme_minimal()
    })
    
    # output$wordcloud <- renderPlot({
    #     set.seed(1234)
    #     wordcloud(words = df_wordcloud$word, freq = df_wordcloud$freq, min.freq = 3,
    #               max.words=200, random.order=FALSE, rot.per=0.35,
    #               colors=brewer.pal(8, "Dark2"))
    # })
    
    # output$sentiment_histo <- renderPlot({
    #     ggplot(sentiment, aes(ave_sentiment)) +
    #         geom_histogram(binwidth = 0.01, fill='sienna1')+
    #         theme_minimal()
    # })
    
    # Tab 6: Map des zones
    listing_zone <- reactive({
        listing %>% 
            filter(neighbourhood == input$zone)
    })
    
    output$map <- renderLeaflet({
        leaflet(data=listing_zone()) %>%
            addTiles() %>% 
            setView(lng = median(listing_zone()$longitude), lat = median(listing_zone()$latitude), zoom = 15) %>% 
            addMarkers(lng=~longitude, lat=~latitude,
                       popup = ~paste(
                           "<b>", name, "</b><br/>",
                           "type: ", room_type, "<br/>",
                           "price: ", round(price), "<br/>",
                           "minimum_nights: ", round(minimum_nights), "<br/>",
                           "available_per_year: ", round(availability_365), "<br/>",
                           "income_monthly: ", round(income_monthly), "<br/>"
                       ),
                       clusterOptions = markerClusterOptions())
    })
    
    output$room_type <- renderPlot({
        listing_zone() %>% 
            ggplot()+
            geom_bar(aes(x=room_type), fill='sienna1')+
            theme_minimal()
    })
    
   output$nb_bnb <- renderText({
       round(nrow(listing_zone()))
   })
   
   output$price <- renderText({
       round(mean(listing_zone()$price, na.rm = T))
   })
   
   output$available <- renderText({
       round(mean(listing_zone()$availability_365, na.rm = T))
   })
   
   output$night <- renderText({
       round(mean(listing_zone()$minimum_nights, na.rm = T))
   })
   
   output$rate <- renderText({
       round(mean(listing_zone()$number_of_reviews, na.rm = T))
   })
   
   output$income <- renderText({
       round(mean(listing_zone()$income_monthly, na.rm = T))
   })
}
# Run the application 
shinyApp(ui = ui, server = server)
