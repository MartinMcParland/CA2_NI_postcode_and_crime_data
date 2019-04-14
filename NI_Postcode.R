# Msc Big Data Analytics - Data Science
# Martin McParland (L00143723)
# 24/03/2019 - 14/04/2019
# CA2 - NI Postcodes and crime data

# Read in the NI Postcodes data set, show the structure and first 10 rows of the data frame
# As there are no column headers in the data file, header is set to FALSE, all blanks are set to NA 
# and any string data is not set as a factor, instead they are set as chr.
nipostcodes <- read.csv("NIPostcodes.csv", header=FALSE, stringsAsFactors = FALSE)
nrow(nipostcodes)
head(nipostcodes, 10)
str(nipostcodes)

# Add a suitable title for each attribute of the data.
# Add column headers to the data set
colnames(nipostcodes) <- c("Organisation_Name", "Sub_building_Name", "Building_Name", "Number", 
              "Primary_Thorfare", "Alt_Thorfare", "Secondary_Thorfare", "Locality", 
              "Townland", "Town", "County", "Postcode", "x_coordinates", "y_coordinates", "Primary_Key")
str(nipostcodes)
head(nipostcodes, 5)

# Show the total number and mean missing values of the NIPostcode data
# First replace all missing data with NA
nipostcodes[nipostcodes == ""] <- NA
# Show the total number of missing values for each attribute
colSums(is.na(nipostcodes))
# Show the mean number of missing values for each attribute
colMeans(is.na(nipostcodes))
# Show structure and the first 10 rows of data
str(nipostcodes)
head(nipostcodes, 10)


# Remove or replace missing entries with a suitable identifier. Decide whether it is
# best to remove missing data or to recode it.
# Show the total number of missing values for each attribute, 
# Then remove columns that are not needed and have most data missing

# The missing data can be visualised using aggr from the VIM library
library(VIM)
mice_plot <- aggr(nipostcodes, col=c('green', 'red'),
                  numbers=TRUE, sortVars=TRUE,)

# The results show that there is a significant number of NAs in 6 columns, 
# Organisation_Name, Sub_building_Name, Building_Name, Alt_Thorfare, Secondary Thorfare and Locality
# Locality is needed for the Limavady dataset but the other columns can be removed

nipostcodes$Organisation_Name <- NULL
nipostcodes$Sub_building_Name <- NULL
nipostcodes$Building_Name <- NULL
nipostcodes$Alt_Thorfare <- NULL
nipostcodes$Secondary_Thorfare <- NULL

# Show structure and the first 10 rows of data
str(nipostcodes)
head(nipostcodes, 10)


# Modify the County attribute to be a categorising factor.
is.factor(nipostcodes$County)
str(nipostcodes)
nipostcodes$County <- factor(nipostcodes$County)
str(nipostcodes$County)

# Show structure and the first 10 rows of data
str(nipostcodes)
head(nipostcodes, 10)

# Move the primary key identifier to the start of the dataset.
colnames(nipostcodes)
primary <- nipostcodes[, c(10, 1:9)]
colnames(primary)

# Show structure and the first 10 rows of data
str(nipostcodes)
head(nipostcodes, 10)

# Create a new dataset called Limavady_data. Store within it only information
# that has locality, townland and town containing the name Limavady. Store this
# information in an external csv file called Limavady

# Select a subset of the data where Town is LIMAVADY
Limavady <- subset(nipostcodes, Town=="LIMAVADY")
# Select a subset of the Limavady data where Townland contains LIMAVADY
Limavady2 <- Limavady[grep("LIMAVADY", Limavady$Townland), ] 
# Select a subset of the Limavady2 data where Locality contains LIMAVADY
Limavady_data <- Limavady2[grep("LIMAVADY", Limavady2$Locality), ]
# Limavady_data now contains all records where LIMAVADY is in Locality and Townland and Town
# Show structure and the first 10 rows of data
head(Limavady_data, 10)
nrow(Limavady_data)
str(Limavady_data)
# Store this information in an external csv file called Limavady.csv
write.csv(Limavady_data, file = "Limavady.csv")

# Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData.
write.csv(primary, file = "CleanNIPostcodeData.csv")



# (a) Using R, amalgamate all of the crime data from each csv file into one dataset.
# Save this dataset into a csv file called AllNICrimeData. Count and show the number
# of rows in the AllNICrimeData dataset.

All_CSVs <- list.files('NI Crime Data/', recursive = TRUE, full.names = TRUE)
AllNICrimeData <- do.call(rbind, lapply(All_CSVs, read.csv, stringsAsFactors = FALSE))
# Count of rows
nrow(AllNICrimeData)
# Write amalgamated dataset to AllNICrimeData.csv
write.csv(AllNICrimeData, file = "AllNICrimeData.csv")

# Show structure and first 10 rows of data
str(AllNICrimeData)
head(AllNICrimeData, 10)

# (b) Modify the structure of the newly created AllNICrimeData csv file and remove
# the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name,
# last outcome and context. Show the structure of the modified file.

str(AllNICrimeData)
# Remove columns that are not needed by selecting column numbers 
colnames(AllNICrimeData)
AllNICrimeData <- AllNICrimeData[, c(2, 5:7, 10)]
colnames(AllNICrimeData)
# Show structure and first 10 rows of data
head(AllNICrimeData, 10)
str(AllNICrimeData)


# (c) Factorise the Crime type attribute. Show the modified structure.
is.factor(AllNICrimeData$Crime.type)
AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type)
str(AllNICrimeData$Crime.type)

# Show structure and first 10 rows of data
head(AllNICrimeData, 10)
str(AllNICrimeData)

# (d) Modify the AllNICrimeData dataset so that the Location attribute contains only a
# street name. For example, the attribute value “On or near Westrock Square” should
# be modified to only contain “Westrock Square”. Modify the resultant empty location
# attributes with a suitable identifier
AllNICrimeData$Location <- gsub("On or near ", "", as.character(AllNICrimeData$Location))

# Show structure and first 10 rows of data
head(AllNICrimeData, 10)
str(AllNICrimeData)


# (e) Choose 1000 random samples of crime data from the AllNICrimeData dataset where 
# the location attribute contains location information. This means that the location 
# information should NOT contain an NA identifier. Store this data in a data frame 
# called random_crime_sample. Then create a function called find_a_postcode that takes 
# as an input each location attribute from random_crime_sample and finds a suitable 
# postcode value from the postcode dataset. Use the CleanNIPostcodeData dataset you 
# created in section 1 as the reference data to find postcodes. If there are several 
# postcodes discovered with the same location, choose the most popular postcode for 
# that location. Store the output from the find_a_postcode function in a suitably 
# named variable in your random_crime_sample data frame. Make sure there are no missing 
# postcodes in the output from your function. Show the structure of this data frame 
# once you’ve completed this task and count the number of records in the modified 
# random_crime_sample data frame.

# 1) First replace all missing data in AllNICrimeData with NA
AllNICrimeData[AllNICrimeData == ""] <- NA
colSums(is.na(AllNICrimeData))
AllNICrimeData[AllNICrimeData == "No Location"] <- NA
colSums(is.na(AllNICrimeData))

# Show structure and first 10 rows of data
head(AllNICrimeData, 10)
str(AllNICrimeData)


# 2) Remove all rows where location attribute is blank 
# and save this as remove_blank_location
remove_blank_location <- na.omit(AllNICrimeData)
colSums(is.na(remove_blank_location))

# Show structure count of rows and first 10 rows of data
str(remove_blank_location)
nrow(remove_blank_location)
head(remove_blank_location, 10)


# 3) Choose 1000 random samples of crime data from the AllNICrimeData dataset where 
# the location attribute contains location information. This means that the location 
# information should NOT contain an NA identifier. Store this data in a data frame 
# called random_crime_sample.
random_crime_sample <- remove_blank_location[sample(nrow(remove_blank_location), 1000), ]

# Show structure count of rows and first 10 rows of data
str(random_crime_sample)
nrow(random_crime_sample)
head(random_crime_sample, 10)


# 4) Use the CleanNIPostcodeData dataset was created in section 1 as 
# the reference data to find postcodes.
CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv", header=TRUE, stringsAsFactors = FALSE)

# Show structure count of rows and first 10 rows of data
str(CleanNIPostcodeData)
nrow(CleanNIPostcodeData)
head(CleanNIPostcodeData, 10)


# 5) Change both Primary_thorfare and Location to upper in order to compare, show first 10
# rows of each dataframe
random_crime_sample$Location <- toupper(random_crime_sample$Location)
CleanNIPostcodeData$Primary_Thorfare <- toupper(CleanNIPostcodeData$Primary_Thorfare)

# Show structure count of rows and first 10 rows of data
str(CleanNIPostcodeData)
nrow(CleanNIPostcodeData)
head(CleanNIPostcodeData, 10)

# Show structure count of rows and first 10 rows of data
str(random_crime_sample)
nrow(random_crime_sample)
head(random_crime_sample, 10)


# 6) Create a list of Primary_Thorfare and postcode
postcode_list <- CleanNIPostcodeData[, c(4, 9)]

# Show structure count of rows and first 10 rows of data
str(postcode_list)
nrow(postcode_list)
head(postcode_list, 10)


# 7) Create function to count the number of occurrences of a postcode for each
# Primary_Thorfare
library(plyr)
postcode_list <- ddply(postcode_list,.(Primary_Thorfare, Postcode),nrow)

# Show structure 
str(postcode_list)

# Assign column names
colnames(postcode_list) <- c("Primary_Thorfare", "Postcode", "Num_Postcode")
postcode_list <- na.omit(postcode_list)
# Sort by Num_Postcode
postcode_list <- postcode_list[order(-postcode_list$Num_Postcode),]
# Remove all except max number of postcodes
postcode_list = postcode_list[!duplicated(postcode_list$Primary_Thorfare),]

# Show structure count of rows and first 10 rows of data 
str(postcode_list)
nrow(postcode_list)
head(postcode_list, 10)

# 8) Add Postcode attribute with NA values to ramdom_crime_sample data frame 
# to hold the postcode generated as most common based on Primary_Thorfare
random_crime_sample$Postcode <- NA

# Show structure count of rows and first 10 rows of data
str(random_crime_sample)
nrow(random_crime_sample)
head(random_crime_sample, 10)


# (f) Append the data output from your find_a_postcode function to the random_crime_sample 
# dataset. Show the modified structure. 
# Add Postcode to random_crime_sample using match function of Location and Primary_Thorfare
random_crime_sample$Postcode <- postcode_list$Postcode[match(random_crime_sample$Location,
                                                             postcode_list$Primary_Thorfare)]

# Show structure count of rows and first 10 rows of data
str(random_crime_sample)
nrow(random_crime_sample)
head(random_crime_sample, 10)

# Save the modified random crime sample data frame as random_crime_sample.csv.
write.csv(random_crime_sample, file = "random_crime_sample.csv")

# (g) Now we need to update the random sample so that it contains only the following items 
# • Month 
# • Longitude 
# • Latitude 
# • Location 
# • Crime.type 
# • Postcode 
# Extract this data into a new data frame called updated_random_sample. Then create another 
# data frame called chart_data using the updated_random_sample data frame. 
str(random_crime_sample)
head(random_crime_sample, 10)
updated_random_sample <- random_crime_sample
chart_data <- updated_random_sample

# Sort the chart_data data frame by postcode where the postcode contains “BT1” and then by crime type. 
chart_data <- dplyr::filter(chart_data, grepl("BT1", Postcode))
chart_data <- chart_data[order(chart_data$Postcode, chart_data$Crime.type),]
# Show the summary statistics for the crime type from this chart_data data frame.
summary(chart_data$Crime.type)

# Show structure count of rows and first 10 rows of data
str(chart_data)
nrow(chart_data)
head(chart_data, 10)

# (h) Create a bar plot of the crime type from the chart_data data frame. Show a suitable 
# main title for the bar chart, and suitable x and y-axis labels. Make sure all labels on 
# the x-axis can be read. Show the bar plot in your CA document.

crimes <- table(chart_data$Crime.type)
barplot(sort(crimes, decreasing = TRUE), main="Crimes Reported in NI 2015-2017", 
        xlab="Crime Types", ylab = "Count of Crimes", font.lab = 3 , 
        font.axis = 2, cex.axis = 0.7, cex.names = 0.5, ylim = c(0,100), las = 2, col = "blue")

# (i) Save your modified CrimeDataWithLocation dataset in a csv file called FinalNICrimeData.
write.csv(chart_data, file = "FinalNICrimeData.csv")

