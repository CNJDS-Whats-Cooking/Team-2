# Clear all variables and devices (used for plotting) in the environment
rm(list=ls())
dev.off()

# Load the required packages (if packages are not available, install them first)
for (package in c('jsonlite')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Declare all variables
file.path = "C://Users//Linesh//Documents//Linesh//General//DS Meetup//Kaggle//Cooking//"

# Load train and test data sets
test.dat <- fromJSON(paste0(file.path, "test.json"))
train.dat <- fromJSON(paste0(file.path, "train.json"))

