# Msc Big Data Analytics - Data Science
# Martin McParland (L00143723)
# 24/03/2019
# CA2 - NI Postcodes and crime data

# Read in the NI Postcodes data set, show the structure and first 10 rows of the data frame 
NIPostcodes <- read.csv("~/NI_Postcode/NIPostcodes.csv", header=FALSE)
str(NIPostcodes)
head(NIPostcodes, 10)

# Show the total number and mean missing values of the NIPostcode data
colSums(is.na(NIPostcodes))
any(is.na(NIPostcodes))
