#' @import httr2
#' @import tidygeocoder
#' @import ggplot2
#' @import tibble

#' @export
#' @title perform_request
#'
#' @param lat latitude (numeric)
#' @param lon longitude (numeric)
#'
#' @returns a tibble with 5 row
#' @examples
#' perform_request(48.85,2.35)
perform_request <- function(lat, lon) {
  response_table <-
    request("https://api.open-meteo.com/v1/forecast") |>
    req_url_query(latitude=lat, longitude=lon,
                  hourly= c("temperature_2m",
                            "apparent_temperature",
                            "precipitation_probability",
                            "precipitation"),
                  .multi = "comma") |>
    req_perform() |>
    resp_body_json() |>
    tibble::as_tibble()
  return(response_table)
}

#' @export
#' @title unnest_response
#'
#' @param resp Reponse JSON contenant les donnees meteorologiques.
#' @return Un tibble avec les colonnes date_heure, temperature_celsius,
#'   temperature_ressentie_celsius, precipitation_proba et precipitation.
#'
#' @examples
#' resp <- perform_request(48.85,2.35)
#' unnest_response(resp)
unnest_response <- function(resp){
  tibble(date_heure = unlist(resp$hourly[1][[1]]),
         temperature_celsius = unlist(resp$hourly[2][[1]]),
         temperature_ressentie_celsius = unlist(resp$hourly[3][[1]]),
         precipitation_proba = unlist(resp$hourly[4][[1]]),
         precipitation = unlist(resp$hourly[5][[1]]))
}

#' @export
#' @title gps_to_address
#'
#' @param long Longitude.
#' @param lat Latitude.
#' @return Une adresse correspondant aux coordonnees fournies.
#'
#' @examples
#' gps_to_address( lat = 48.8534, long = 2.3488)
#'
gps_to_address <- function(lat, long) {
  coordinates_tibble <- tibble::tibble(lat = lat, long = long)
  resultat <- reverse_geocode(coordinates_tibble, lat = "lat", long = "long")
  return(resultat$address)
}


#' @export
#' @title address_to_gps
#'
#' @param address Adresse a convertir.
#' @return Un tibble contenant les coordonnees GPS correspondant a l adresse.
#'
#' @examples
#' address_to_gps("Tour Eiffel, Paris, France")
address_to_gps <- function(address) {
  df <- data.frame(address)
  resultat <- geocode(df, address)
  return(resultat)
}

#' @export
#' @title get_gps_coordinate
#'
#' @param address adresse un vecteur de caracteres representant l adresse
#'
#' @returns renvoie un vecteur numerique avec la latitude et la longitude
get_gps_coordinate <- function(address) {
  result <- address_to_gps(address)
  coordinates <- c(x = result$lat, y = result$long)
  return(coordinates)
}

#' @export
#' @title get_forecast.numeric
#'
#' @param xy un vecteur numerique contenant la latitude et la longitude
#'
#' @returns renvoie un tibble avec des donnees de prevision
get_forecast.numeric <- function(xy){
  if (length(xy) != 2 || !is.numeric(xy)) {
    stop("L argument xy doit etre un vecteur numerique de taille 2 contenant les coordonnees x, y (latitude, longitude).")
  }

  lat <- xy[1]
  lon <- xy[2]

  response <- perform_request(lat, lon)
  result <- unnest_response(response)

  return(result)

}

#' @export
#' @title get_forecast.character
#'
#' @param address un vecteur de caracteres representant l adresse
#'
#' @returns renvoie un tibble avec des donnees de prevision
get_forecast.character <- function(address) {
  if (length(address) != 1 || !is.character(address)) {
    stop("L argument address doit etre un caractere de taille 1 contenant l adresse.")
  }

  gps_coordinates <- address_to_gps(address)
  xy <- c(gps_coordinates$lat, gps_coordinates$long)

  response <- perform_request(xy[1], xy[2])
  result <- unnest_response(response)

  return(result)
}

#' @export
#' @title get_forecast
#'
#' @param address Les coordonnees sous forme d un vecteur numerique de taille 2 (latitude, longitude)
#'                 ou une adresse sous forme d un caractere.
#' @return Un tibble contenant les previsions meteorologiques.
#' @examples
#' # Exemple avec des coordonnees
#' get_forecast(c(48.8566, 2.3522))
#'
#' # Exemple avec une adresse
#' get_forecast("Place de la Concorde, Paris, France")
get_forecast <- function(address) {
  if (is.numeric(address) && length(address) == 2) {
    # Si l entree est numerique, supposons que ce sont des coordonnees (lat, lon)
    lat <- address[1]
    lon <- address[2]
  } else if (is.character(address) && length(address) == 1) {
    # Si l entree est un caractere, supposons que c est une adresse
    gps_coordinates <- address_to_gps(address)
    lat <- gps_coordinates$lat
    lon <- gps_coordinates$long
  } else {
    stop("L argument doit etre soit un vecteur numerique de taille 2 (coordonnees), soit un caractere de taille 1 (adresse).")
  }

  response <- perform_request(lat, lon)
  result <- unnest_response(response)

  return(result)
}

#' @export
#' @title visu_temp
#'
#' @param address a character vector representing the address
#'
#' @returns a ggplot object for temperature visualization
visu_temp<- function(address) {
  forecast_data <- get_forecast(address)

  # Utiliser geom_line() en specifiant une variable de groupe
  ggplot(forecast_data, aes(x = forecast_data$date_heure, y = forecast_data$temperature_celsius, group = 1)) +
    geom_line() +
    labs(title = "Previsions meteorologiques (temperature)" ,
         x = "Date et heure",
         y = "Temperature (degre Celsius)")+
    theme_bw()
}

#' @export
#' @title visu_temp_ressentie
#'
#' @param address a character vector representing the address
#'
#' @returns a ggplot object for apparent temperature visualization
visu_temp_ressentie<- function(address) {
  forecast_data <- get_forecast(address)

  # Utiliser geom_line() en specifiant une variable de groupe
  ggplot(forecast_data, aes(x =  forecast_data$date_heure, y = forecast_data$temperature_ressentie_celsius, group = 1)) +
    geom_line() +
    labs(title = "Previsions meteorologiques (temperature ressentie)" ,
         x = "Date et heure",
         y = "Temperature ressentie(degre Celsius)")+
    theme_bw()
}


#' @export
#' @title visu_precipitation
#'
#' @param address a character vector representing the address
#'
#' @returns a ggplot object for precipitation visualization
visu_precipitation<- function(address) {
  forecast_data <- get_forecast(address)

  # Utiliser geom_line() en spécifiant une variable de groupe
  ggplot(forecast_data, aes(x =  forecast_data$date_heure, y = forecast_data$precipitation, group = 1)) +
    geom_line() +
    labs(title = "Prévisions météorologiques (précipitation)" ,
         x = "Date et heure",
         y = "Précipitation")+
    theme_bw()
}

