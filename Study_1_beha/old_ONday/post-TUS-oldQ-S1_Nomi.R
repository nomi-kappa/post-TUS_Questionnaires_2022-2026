#simple script for old- post-tus questionnaire - Nomi only

# Load required packages
require(pacman)
library(tidyverse) 
p_load('tidyverse', 'jsonlite')

#jatos2r function
jatos2r <- function(path) {
  
  # Read the text file from JATOS
  read_file(path) %>%
    # Split it into lines
    str_split('\n') %>% 
    first() %>%  # Take only the first element (since it's a list). he first line in a JATOS file might contain metadata or headers; we want to skip that and focus on the JSON data.
    # Filter empty rows
    discard(function(x) x == '') %>% #anonymous function that checks whether each element x of the list is equal to an empty string ''. The function returns TRUE if x is an empty string, and FALSE otherwise.
    # Parse JSON into a data frame
    map_dfr(fromJSON, flatten = TRUE) -> data # map_dfr(): apply a function to each element of a list and then combine the results into a data frame, 
                 #fromJSON:  parses JSON strings into R objects. In this context, map_dfr() is applying fromJSON to each element of the list.flatten=nested objects are flattened into a single level.

  
  return(data)  # Return the resulting data frame
}


#path  "day_of_stim_upJUL23.txt" directly as an argument to the function.
ONday_posttus_old <- jatos2r("day_of_stim_upJUL23.txt") #
# Write the dataframe to a CSV file or write_csv(data, '/path/to/save/labjs_data_output.csv')
write.csv(ONday_posttus_old, file = "ONday_posttus_old.csv", row.names = FALSE)



# Read the CSV file into a data frame
ONday_posttus_old <- read.csv("ONday_posttus_old.csv")

# Specify the columns you want to keep
columns_to_keep <- c(
  "BRICID", "studyID", "timestamp", "todayDate", "Date.of.visit", "stimulation_type",
  "Headache.value", "Headache.relation", "Neck.pain.value", "Neck.pain.relation",
  "Tooth.pain.value", "Tooth.pain.relation", "Head.value", "Head.relation",
  "Itchiness.value", "Itchiness.relation", "Hearing.changes.value", "Hearing.changes.relation",
  "Speech.value", "Speech.relation", "Vision.value", "Vision.relation",
  "Twitching.value", "Twitching.relation", "Balance.value", "Balance.relation",
  "Hand.value", "Hand.relation", "Numbness.value", "Numbness.relation",
  "Tightness.value", "Tightness.relation", "Unusual.feelings.value", "Unusual.feelings.relation",
  "Anxious.value", "Anxious.relation", "Sleepiness.value", "Sleepiness.relation",
  "Attentional.difficulties.value", "Attentional.difficulties.relation",
  "Forgetful.value", "Forgetful.relation", "Nausea.value", "Nausea.relation",
  "Dizziness.value", "Dizziness.relation", "Other.symptom", "Other.value",
  "Other.relation", "Comments"
)


# Subset the data frame to keep only the desired columns
ONday_posttus_old_clean <- ONday_posttus_old[, columns_to_keep]

# Write the subsetted data frame to a new CSV file
write.csv(ONday_posttus_old_clean, "ONday_posttus_old_clean.csv", row.names = FALSE)
View(ONday_posttus_old_clean)


# Create an empty data frame to store the combined data that has the same number of columns as data_posttus_old_clean.
#The number of rows is calculated as the number of rows in data_posttus_old_clean divided by 3, 
#because we want to combine every three rows into one.
combined_data <- data.frame(matrix(nrow = nrow(ONday_posttus_old_clean) %/% 3, ncol = ncol(ONday_posttus_old_clean), dimnames = list(NULL, colnames(ONday_posttus_old_clean))), stringsAsFactors = FALSE)

# Loop through each block of three rows and combine them into one row
for (i in 1:(nrow(ONday_posttus_old_clean) %/% 3)) { #I iterate over each block of three consecutive rows
  combined_row <- character(ncol(ONday_posttus_old_clean))  # Initialize an empty character vector to store combined row
  
  # Loop through each column
  for (j in 1:ncol(ONday_posttus_old_clean)) {
    # Combine non-NA values or comments
    combined_row[j] <- ifelse(all(is.na(ONday_posttus_old_clean[(3 * i - 2):(3 * i), j])), NA, na.omit(ONday_posttus_old_clean[(3 * i - 2):(3 * i), j])[1])#For each column, check if all three values in the current block are NA. 
    #If they are, assign NA to the corresponding position in the combined_row. Otherwise,  assign the first non-NA value or comment to the corresponding position in the combined_row.
  }
       #extra: -2 is a constant subtraction. It subtracts 2 from the result of 3 * i; used to adjust the starting index of the sequence to ensure that it begins at the correct row for each group of three rows 
      #corresponding to an ID's data. By subtracting 2, it shifts the starting index by 2 rows earlier, which aligns it with the first row of each participant's data.
      #For example, if i represents the participant number, then (3 * i - 2) calculates the index of the first row for that participant's data. Subtracting 2 ensures that the sequence starts from the correct row.
  # Assign the combined row to the combined data frame
  combined_data[i, ] <- combined_row
}

# Write the combined data to a CSV file and read it
write.csv(combined_data, "ONday_combined_data_oldQ.csv", row.names = FALSE)
ONday_combined_data_oldQ <- read.csv("ONday_combined_data_oldQ.csv")#;View(ONday_combined_data_oldQ)


# Replace values in the "studyID" column with 'HP-FouE-AI-010223' for each row
ONday_combined_data_oldQ$studyID <- rep('HP-FouE-AI-010223', nrow(ONday_combined_data_oldQ)) #rep() replicates its first argument a specified number of times. 
                                                                     #nrow returns the number of rows, thus specifying how many times to repeat the string.


#delete rows not needed (tests or wrongperson)
  ONday_combined_data_oldQ <- ONday_combined_data_oldQ[-c(17, 25, 34, 35, 36, 51, 52, 55, 56, 57, 58, 59, 60),];View(ONday_combined_data_oldQ)
#ALSO DELETE: #57	MRMH0014		HP-FouE-AI-010223	2023-02-02T16:26:25.429Z	02/02/2023	02/02/2023	TUS
              #56	LUBT0216		HP-FouE-AI-010223	2023-02-03T13:13:08.537Z	03/02/2023	01/02/2023	TUS
              #58	KDNS0120    HP-FouE-AI-010223	2023-02-01T13:47:28.073Z	01/02/2023	01/02/2023
              #25	KTWL0180		HP-FouE-AI-010223	2023-05-16T13:51:27.697Z	16/05/2023	16/05/2023	TUS
              #34	KTWL0180		HP-FouE-AI-010223	2023-04-19T14:08:52.275Z	19/04/2023	19/04/2023	TUS
              #51	MRBU0208		HP-FouE-AI-010223	2023-03-06T16:26:51.329Z	06/03/2023	06/03/2023	TUS
              #52	MRBU0208		HP-FouE-AI-010223	2023-03-06T16:25:56.860Z	06/03/2023	06/03/2023	TUS
#17, 25, 34 35, 36, 51, 52, 55, 56, 57, 58, 59, 60


#ONday_combined_data_oldQ <- read.csv("ONday_combined_data_oldQ.csv")

#capitalize#sshw0093, Zhhs0560, Ktgo2025, jaka0154, Jaka0154, Jaka0154
# Capitalize levels of BRICID column in ONday_combined_data_oldQ
# Convert BRICID column to factor
ONday_combined_data_oldQ$BRICID <- as.factor(ONday_combined_data_oldQ$BRICID)
levels(ONday_combined_data_oldQ$BRICID) <- toupper(levels(ONday_combined_data_oldQ$BRICID))

# Sample data




#fix
#SNKT0189 to SKKY0189
# Change the level in the BRICID column
# Change the level in the BRICID column
ONday_combined_data_oldQ <- ONday_combined_data_oldQ %>%
  mutate(BRICID = recode(BRICID, "SNKT0189" = "SKKY0189"))

  
#Add session column
#BRICID	session 
#1	SSTO0540	I 2	LECA0624	D 3	JYDF0098	D 4	MLGA1440	S 5	FNMA0105	I 6	SSTO0540	D 7	JYFD0098	S 8	LECA0624	I 9	KLWT0036	I
#10	DIVL0216	S 11	ZHHS0560	D 12	DIVL0216	I 13	ZHHS0560	S 14	JYDF0098	I 15	bnvn0020	D 16	LIML0315	S 18  MRMH0014  I 19	LIML0315	I
#20	TRWL0072	I 21	EEMR0429	D 22	BNVN0020	I 23	RBDN0216	S 24	SKKY0189	S 26	DIVL0216	D 27	KLWT0036	D 28	RBDN0216	I
#29	sshw0093	D 30	Zhhs0560	I 31	BNVN0100	S 32	Ktgo2025	S 33	SSHW0093	I 37	JAKA0154	D 38  SSHW0093 39 FNMA0105 D D 40	SNKT0189	I 41	Jaka0154	S 42	FNMA0105	S 43	SNKT0189	D #44	KTGO2025	D #45	EEMR0429	S
#46	LIML0315	D #47	EEMR0429	I #48	Jaka0154	I #49	KTGO2025	I #50	MRMH0014	S #53	MRMH0014	D #54	SINB0180	D

# Create the data frame with BRICID and session columns
session_data <- data.frame(
  BRICID = c('SSTO0540', 'LECA0624', 'JYDF0098', 'MLGA1440', 'FNMA0105', 
             'SSTO0540', 'JYFD0098', 'LECA0624', 'KLWT0036', 'DIVL0216', 
             'ZHHS0560', 'DIVL0216', 'ZHHS0560', 'JYDF0098', 'BNVN0020', 
             'LIML0315', 'MRMH014', 'LIML0315', 'TRWL0072', 'EEMR0429', 
             'BNVN0020', 'RBDN0216', 'SKKY0189', 'DIVL0216', 'KLWT0036', 
             'RBDN0216', 'SSHW0093', 'ZHHS560', 'BNVN0100', 'KTGO2025', 
             'SSHW0093', 'JAKA0154', 'SSHW0093', 'FNMA0105', 'SKKY0189', 
             'JAKA0154', 'FNMA0105', 'SKKY0189', 'KTGO2025', 'EEMR0429', 
             'LIML0315', 'EEMR0429', 'JAKA0154', 'KTGO2025', 'MRMH0014', 
             'MRMH0014', 'SINB0180'),
  session = c('I', 'D', 'D', 'S', 'I',
              'D', 'S', 'I', 'I', 'S', 
              'D', 'I', 'S', 'I', 'D', 
              'S', 'I', 'I', 'I', 'D', 
              'I', 'S', 'S', 'D', 'D',
              'I', 'D', 'I', 'S', 'S', 
              'I', 'D', 'D', 'D', 'I', 
              'S', 'S', 'D', 'D', 'S',
              'D', 'I', 'I', 'I', 'S', 
              'D', 'D'))



# Show the resulting data frame
session_data
# Add the session column to the ONday_combined_data_oldQ data frame
ONday_combined_data_oldQ <- cbind(ONday_combined_data_oldQ, 
                                  session_data$session)

# Get the index of the BRICID column
bricid_index <- which(names(ONday_combined_data_oldQ) == "BRICID")

# Insert the session column next to the BRICID column
ONday_combined_data_oldQ <- cbind(ONday_combined_data_oldQ[, c(1:bricid_index)], 
                                  session_data$session,
                                  ONday_combined_data_oldQ[, -(1:bricid_index)])


#write 
write.csv(ONday_combined_data_oldQ, "ONday_combined_data_oldQ.csv");View(ONday_combined_data_oldQ)
