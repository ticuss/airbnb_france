library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(lubridate)
library(leaflet)
library(shinydashboard)
library(tm)

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
shinyUI(dashboardPage(skin = "black",
                    dashboardHeader(title = 'Airbnb à Bordeaux'),
                    main_sidebar, 
                    main_body
)
)