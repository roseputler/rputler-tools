# install.packages('zipcode') ## https://blog.exploratory.io/geocoding-us-zip-code-data-with-dplyr-and-zipcode-package-7f539c3702b0

# From https://datascience.stackexchange.com/questions/12394/how-do-i-find-the-minimum-distance-between-zip-codes-in-r
greatCircleDistance = function(latAlpha, longAlpha, latBeta, longBeta, radius = 6371) {
  ## Function taken directly from Wikipedia https://en.wikipedia.org/wiki/Great-circle_distance
  ## Earth radius in km is default (6371)
  ## Long/Lats are in degrees so need helper function to convert to radians
  degreeToRadian = function(degree) (degree * pi / 180)
  deltaLong = degreeToRadian(longBeta) - degreeToRadian(longAlpha)
  sinLat = sin(degreeToRadian(latAlpha)) * sin(degreeToRadian(latBeta))
  cosLat = cos(degreeToRadian(latAlpha)) * cos(degreeToRadian(latBeta))
  ## acos is finicky with precision so we will assume if NA is thrown
  ## the argument was very close to 1 and therefore will return 0
  ## acos(1) == 0
  acosRaw = suppressWarnings(acos(sinLat + cosLat * cos(deltaLong)))
  acosSafe = ifelse(is.na(acosRaw), 0, acosRaw)
  acosSafe * radius
}

## Function to calculate the distance from the hospital to the patient's address.
## Utilizes zip codes and requires the 'zipcode' package.
## Uses "great-circle distance" over driving or other type of distance, which requires an API key for most packages.
distance_from_hosp <- function(zip_query,hosp_zip = 48109){
  if(is.numeric(zip_query)) stop("Zip codes must be characters, to account for those beginning with 0.")
  if(any(nchar(zip_query)!=5, na.rm = T)) stop("Zip codes must be 5 digits.")
  
  require(zipcode)
  data("zipcode")
  hosp_lat = zipcode[zipcode$zip == hosp_zip,"latitude"]
  hosp_long = zipcode[zipcode$zip == hosp_zip,"longitude"]
  
  distances <- zipcode[zipcode$zip %in% substr(zip_query,1,5),] 
  distances$distance_km <- greatCircleDistance(distances$latitude, distances$longitude, hosp_lat, hosp_long)
  distances$distance_mi <- distances$distance_km/1.609344
  
  return(distances)
}

# Example utilization
## distance_from_hosp(substr(summary_data$ZIP_CODE,1,5)) # SUCCESSFULLY EXECUTES
## distance_from_hosp(summary_data$ZIP_CODE) # THROWS ERROR
## distance_from_hosp(as.numeric(substr(summary_data$ZIP_CODE,1,5))) # THROWS ERROR
