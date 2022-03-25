# Mosquito abundance in Donana wetland
library(Hmsc)
library(snow)
library(corrplot)

data <- read.csv("Msq_donana_data.csv", stringsAsFactors=TRUE, header = TRUE)

