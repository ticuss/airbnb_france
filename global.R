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
