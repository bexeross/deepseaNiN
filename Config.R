
## Paths

library(readxl)
# https://owncloud.imr.no/index.php/s/lCkOERjzJkPItZj

# C:\Users\genoveva\ownCloud\NiN_Mareano\inputs

#userName <- "C:/Users/genoveva/" # replace with your own name here
userName <- "C:/Users/a21448/" # bex's path
#userName <- "C:/Users/genoveva/"
  
  

dataPath1 <- "OneDrive - Havforskningsinstituttet/General/inputs"
dataPath2 <- "Havforskningsinstituttet/Video analysis lab - Reference List"

dataPath <- paste0(userName, dataPath1)
dataPath_2 <- paste0(userName, dataPath2)


#sppdens <- read.csv(file.path(dataPath,"species_densities.csv"))
#refer <- read.table(file.path(dataPath,"reference.csv"), sep ="|", header = TRUE)
#sample_info <- read.csv(file.path(dataPath,"sample_info.csv"))

#taxonary <- read_xlsx(file.path(dataPath_2, "Taxonary.xlsx"), sheet=1)


## Some functions

#convert geographic coordinates in WGS84 to UTM33N

#utmize <- function(x){
#   require(rgdal)
#   proj4string(x)=CRS("+init=epsg:4326")
#   result <- spTransform(x, CRS("+init=epsg:32633")) 
#   return(result)
# }

