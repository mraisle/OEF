#combine all of the articles author data into the one table

library(httr)
library(jsonlite)
library(urltools)
library(dplyr)
library(purrr)


# Set the folder path where the .csv files are located
folder_path <- "/Users/meganraisle/Documents/OEF/getarticles/author_data"

# List all .csv files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Load all .csv files into a list of data frames
list_of_data_frames <- lapply(csv_files, read.csv)

# Optionally, you can merge all data frames into a single data frame
# Make sure the data frames have the same structure (columns) before merging
combined_data_frame <- do.call(rbind, list_of_data_frames)

write.csv(combined_data_frame, "fulldata_April2023.csv")

#alltogethernow <- rbind(data, clean_Liz)

#write.csv(alltogethernow, "fulldata_4.csv")