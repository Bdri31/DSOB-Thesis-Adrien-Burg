# Set the working directory
setwd("/Users/adrienburg/Downloads")

#Select the library we need
library(ggplot2)
library(dplyr)
library(wordcloud)

# Load the vcd package for mosaic plot
library(vcd)

# Read the CSV file
survey_data <- read.csv("Les sports mécaniques et les véhicules de série.csv", header = TRUE)

# Rename the headers
colnames(survey_data)[2:22] <- c("Age", "Gender", "Level of education", "Employment status", "Randomization", "No video", "Motorsport video", "Corporate video", "Promotional video", "Motorsport interest", "Motorsport knowledge", "Have you ever attended a motorsport event?", "Car ownership", "Type of car", "Importance of the link with motorsport", "Have you ever purchased a car or car part from a brand involved in motorsport?", "Effectiveness of motorsport as a marketing tool", "Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?", "To what extent does motorsport have a global impact on car technology and innovation?", "Can motorsport contribute to the development of more sustainable and environmentally-friendly car technologies?", "Why?")

# Define a color palette
color_palette <- c("#FFB3BA", "#FFDAC1", "#FFEDCC", "#D4F1BE", "#A2FAC2", "#D0E6FF", "#B5EAD7", "#E2F0CB", "#FFF8BF", "#FFDAB9")


# Modify the 5th column to include "Other"
top_categories <- names(head(sort(table(survey_data$"Employment status"), decreasing = TRUE), ))
survey_data$"Employment status" <- ifelse(survey_data$"Employment status" %in% top_categories, survey_data$"Employment status", "Other")

# Rename one answer as "Artisan"
survey_data$"Employment status" <- ifelse(survey_data$"Employment status" == "Artisan, petit commerçant ou chef d'entreprise de moins de 10 salariés", "Artisan", survey_data$"Employment status")

# Rename one answer as "Other"
survey_data$"Employment status" <- ifelse(survey_data$"Employment status" == "", "Other", survey_data$"Employment status")

# Modify the column type of car to include "Other"
top_categories_type_of_car <- names(head(sort(table(survey_data$"Type of car"), decreasing = TRUE), 8))
survey_data$"Type of car" <- ifelse(survey_data$"Type of car" %in% top_categories_type_of_car, survey_data$"Type of car", "Other")

# Create a new column with modified age values
survey_data <- survey_data %>%
  mutate("Age" = recode(`Age`,
                               "Moins de 18" = "Under 18",
                               "18-24" = "18-24",
                               "25-34" = "25-34",
                               "35-44" = "35-44",
                               "45-54" = "45-54",
                               "55-64" = "55-64",
                               "65 ans et plus" = "65 and over"))

# Create a new column with modified gender values
survey_data <- survey_data %>%
  mutate("Gender" = recode(`Gender`,
                               "Homme" = "Male",
                               "Femme" = "Female"))

# Create a new column with modified car ownership values
survey_data <- survey_data %>%
  mutate("Car ownership" = recode(`Car ownership`,
                           "Oui" = "Yes",
                           "Non" = "No"))

# Create a new column with modified education values
survey_data <- survey_data %>%
  mutate("Level of education" = recode(`Level of education`,
                                     "Bac ou inférieur" = "High school diploma or equivalent",
                                     "Bac +2" = "Some college or associate degree",
                                     "Bac +3 (Licence / Bachelor)" = "Bachelor's degree",
                                     "Bac +5 (Master)" = "Master's degree",
                                     "Bac +8 (Doctorat)" = "Doctorate degree"))

# Create a new column with modified employment status values
survey_data <- survey_data %>%
  mutate("Employment status" = recode(`Employment status`,
                                             "Agriculteur" = "Farmer",
                                             "Artisan" = "Craftsperson",
                                             "Cadre dirigeant" = "Executive or senior manager",
                                             "Profession intermédiaire" = "Intermediate profession",
                                             "Employé" = "Employee",
                                             "Cadre" = "Executive or senior manager",
                                             "Ouvrier" = "Worker",
                                             "Retraité" = "Retiree",
                                             "Étudiant" = "Student"))

# Create a new column with modified motorsport interest values
survey_data <- survey_data %>%
  mutate("Motorsport interest" = recode(`Motorsport interest`,
                                               "Pas du tout intéressé" = "Not at all interested",
                                               "Légèrement intéressé" = "Slightly interested",
                                               "Moyennement intéressé" = "Moderately interested",
                                               "Très intéressé" = "Very interested",
                                               "Extrêmement intéressé" = "Extremely interested"))

# Create a new column with modified motorsport knowledge values
survey_data <- survey_data %>%
  mutate("Motorsport knowledge" = recode(`Motorsport knowledge`,
                                                "Pas du tout informé" = "Not at all knowledgeable",
                                                "Légèrement informé" = "Slightly knowledgeable",
                                                "Moyennement informé" = "Moderately knowledgeable",
                                                "Très informé" = "Very knowledgeable",
                                                "Extrêmement informé" = "Extremely knowledgeable"))

# Create a new column with modified values for attendance at a motorsport event
survey_data <- survey_data %>%
  mutate("Have you ever attended a motorsport event?" = recode(`Have you ever attended a motorsport event?`,
                                  "Oui" = "Yes",
                                  "Non" = "No"))

# Create a new column with modified importance values
survey_data <- survey_data %>%
  mutate("Importance of the link with motorsport" = recode(`Importance of the link with motorsport`,
                                      "Pas du tout important" = "Not at all",
                                      "Légèrement important" = "Slightly",
                                      "Moyennement important" = "Moderately",
                                      "Très important" = "Very",
                                      "Extrêmement important" = "Extremely"))


# Create a new column with modified values for purchasing car or car part from a brand involved in motorsport
survey_data <- survey_data %>%
  mutate("Have you ever purchased a car or car part from a brand involved in motorsport?" = recode(`Have you ever purchased a car or car part from a brand involved in motorsport?`,
                                                               "Oui" = "Yes",
                                                               "Non" = "No",
                                                               "Je ne sais pas" = "I don't know"))

# Create a new column with modified effectiveness values
survey_data <- survey_data %>%
  mutate("Effectiveness of motorsport as a marketing tool" = recode(`Effectiveness of motorsport as a marketing tool`,
                                         "Pas du tout efficace" = "Not at all effective",
                                         "Légèrement efficace" = "Slightly effective",
                                         "Moyennement efficace" = "Moderately effective",
                                         "Très efficace" = "Very effective",
                                         "Extrêmement efficace" = "Extremely effective"))

# Create a new column with modified values for influence of purchasing
survey_data <- survey_data %>%
  mutate("Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?" = recode(`Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?`,
                                                               "Oui" = "Yes",
                                                               "Non" = "No"))

# Create a new column with modified impact values
survey_data <- survey_data %>%
  mutate("To what extent does motorsport have a global impact on car technology and innovation?" = recode(`To what extent does motorsport have a global impact on car technology and innovation?`,
                                  "Pas du tout d'impact" = "Not at all",
                                  "Peu d'impact" = "Slightly",
                                  "Moyennement d'impact" = "Moderately",
                                  "Beaucoup d'impact" = "Very",
                                  "Extrêmement d'impact" = "Extremely"))

# Create a new column with modified values for the contribution of motorsport to the development of more sustainable car technologies
survey_data <- survey_data %>%
  mutate("Can motorsport contribute to the development of more sustainable and environmentally-friendly car technologies?" = recode(`Can motorsport contribute to the development of more sustainable and environmentally-friendly car technologies?`,
                                                                                                   "Oui" = "Yes",
                                                                                                   "Non" = "No",
                                                                                                   "Je ne suis pas certain" = "Unsure"))

# Create a new column with modified values for type of cars
survey_data <- survey_data %>%
  mutate("Type of car" = recode(`Type of car`,
                                 "Berline" = "Sedan",
                                 "SUV" = "SUV",
                                 "Compacte" = "Hatchback",
                                 "Sportive" = "Sports car",
                                 "Cabriolet" = "Convertible",
                                 "Citadine" = "Urban",
                                 "Break" = "Estate",
                                 "Ludospace" = "Station wagon",
                                 "Monospoace" = "Station wagon",
                                 "Familiale" = "Station wagon",
                                 "Utilitaire" = "Utility",
                                 "Méhari" = "Convertible",
                                 "Cabriolet " = "Convertible"))


# Function to generate pie charts
generate_pie_chart <- function(data, question) {
  # Count the frequencies of each response
  response_counts <- table(data[[question]])
  
  # Convert the frequencies to percentages
  response_percentages <- round(prop.table(response_counts) * 100, 2)
  
  # Generate the pie chart
  pie_chart <- pie(response_percentages, labels = paste0(names(response_percentages), "\n(", response_percentages, "%)"), col = color_palette, cex = 1, border = NA)
  
  # Center the chart title
  title(main = question, line = -1.5, cex.main = 1)
  
  # Return the pie chart
  return(pie_chart)
}

# Remove blank for type of car category
type_of_car_df <- na.omit(survey_data[survey_data$"Type of car" != "", ])

# Generate pie charts for each question
age_pie_chart <- generate_pie_chart(survey_data, "Age")
ggsave("age_pie_chart.png",width = 8, height = 6, dpi = 300)
sex_pie_chart <- generate_pie_chart(survey_data, "Gender")
education_pie_chart <- generate_pie_chart(survey_data, "Level of education")
professional_status_pie_chart <- generate_pie_chart(survey_data, "Employment status")
car_ownership_pie_chart <- generate_pie_chart(survey_data, "Car ownership")
motorsport_interest_pie_chart <- generate_pie_chart(survey_data, "Motorsport interest")
motorsport_knowledge_pie_chart <- generate_pie_chart(survey_data, "Motorsport knowledge")
event_attendance_pie_chart <- generate_pie_chart(survey_data, "Have you ever attended a motorsport event?")
car_type_pie_chart <- generate_pie_chart(type_of_car_df, "Type of car")
importance_link_motorsport_chart <- generate_pie_chart(survey_data, "Importance of the link with motorsport")
purchasing_brand_involved_motorsport_pie_chart <- generate_pie_chart(survey_data, "Have you ever purchased a car or car part from a brand involved in motorsport?")
marketing_tool_pie_chart <- generate_pie_chart(survey_data, "Effectiveness of motorsport as a marketing tool")
purchase_influence_pie_chart <- generate_pie_chart(survey_data, "Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?")
car_ownership_pie_chart <- generate_pie_chart(survey_data, "Car ownership")
motorsport_impact_innovation_pie_chart <- generate_pie_chart(survey_data, "To what extent does motorsport have a global impact on car technology and innovation?")
motorsport_contribution_sustainable_pie_chart <- generate_pie_chart(survey_data, "Can motorsport contribute to the development of more sustainable and environmentally-friendly car technologies?")



# Display the pie charts
print(age_pie_chart)
print(sex_pie_chart)
print(education_pie_chart)
print(professional_status_pie_chart)
print(car_ownership_pie_chart)

# Create specific data frame for each category of the experiment
no_video_df <- survey_data[, -c(8, 9, 10)]
motorsport_video_df <- survey_data [, -c(7, 9, 10)]
corporate_video_df <- survey_data [, -c(7, 8, 10)]
promotional_video_df <- survey_data [, -c(7, 8, 9)]

# For each data frame of each category of the experiment, remove the blank values to keep only the data of the specific experiment
no_video_df <- na.omit(no_video_df[no_video_df$"No video" != "", ])
motorsport_video_df <- na.omit(motorsport_video_df[motorsport_video_df$"Motorsport video" != "", ])
corporate_video_df <- na.omit(corporate_video_df[corporate_video_df$"Corporate video" != "", ])
promotional_video_df <- na.omit(promotional_video_df[promotional_video_df$"Promotional video" != "", ])

# Generate pie charts for each experiment category
no_video_pie_chart <- generate_pie_chart(no_video_df, "No video")
motorsport_video_pie_chart <- generate_pie_chart(motorsport_video_df, "Motorsport video")
corporate_video_pie_chart <- generate_pie_chart(corporate_video_df, "Corporate video")
promotional_video_pie_chart <- generate_pie_chart(promotional_video_df, "Promotional video")

# Display the experiment pie charts
print(no_video_pie_chart)
print(motorsport_video_pie_chart)
print(corporate_video_pie_chart)
print(promotional_video_pie_chart)


# Specify the column containing the text data
text_column <- "Why?"

# Filter the column to remove NA values
filtered_data <- survey_data[!is.na(survey_data[[text_column]]), ]

# Combine all the text entries into a single string
text <- paste(filtered_data[[text_column]], collapse = " ")

# Convert the text to lowercase
text <- tolower(text)

# Split the text into individual words
words <- strsplit(text, "\\W+")

# Count the frequencies of each word
word_freq <- table(unlist(words))

# Exclude words with 4 letters or less
word_freq <- word_freq[nchar(names(word_freq)) > 4]

# Generate the word cloud
wordcloud(names(word_freq), freq = word_freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


## CONTINGENGY TABLES
# Define the colors for the mosaic plots
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

## NO VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_no_video <- subset(no_video_df, select = c("Age", "No video"))

# Create the contingency table
contingency_table_no_video <- table(subset_data_no_video$Age, subset_data_no_video$`No video`)

# Print the contingency table
print(contingency_table_no_video)

# Create a mosaic plot of the contingency table
mosaicplot(contingency_table_no_video, color = colors, main = "Contingency Table Age - No Video", las = 1)



## MOTORSPORT VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_motorsport_video <- subset(motorsport_video_df, select = c("Age", "Motorsport video"))

# Create the contingency table
contingency_table_motorsport_video <- table(subset_data_motorsport_video$Age, subset_data_motorsport_video$`Motorsport video`)

# Print the contingency table
print(contingency_table_motorsport_video)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_motorsport_video, color = colors, main = "Contingency Table Age - Motorsport Video",
           cex.axis = 0.6, las = 1)


## CORPORATE VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_corporate_video <- subset(corporate_video_df, select = c("Age", "Corporate video"))

# Create the contingency table
contingency_table_corporate_video <- table(subset_data_corporate_video$Age, subset_data_corporate_video$`Corporate video`)

# Print the contingency table
print(contingency_table_corporate_video)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_corporate_video, color = colors, main = "Contingency Table Age - Corporate Video",
           cex.axis = 0.6, las = 1)


## PROMOTIONAL VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_promotional_video <- subset(promotional_video_df, select = c("Age", "Promotional video"))

# Create the contingency table
contingency_table_promotional_video <- table(subset_data_promotional_video$Age, subset_data_promotional_video$`Promotional video`)

# Print the contingency table
print(contingency_table_promotional_video)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_promotional_video, color = colors, main = "Contingency Table Age - Promotional Video",
           cex.axis = 0.6, las = 1)

#Perform the chi-squared test
chi_squared_promotional <- chisq.test(contingency_table_promotional_video)
chi_squared_corporate <- chisq.test(contingency_table_corporate_video)
chi_squared_motorsport <- chisq.test(contingency_table_motorsport_video)
chi_squared_no_video<- chisq.test(contingency_table_no_video)

# Print the test results
print(chi_squared_promotional)
print(chi_squared_corporate)
print(chi_squared_motorsport)
print(chi_squared_no_video)

# Load the vcd package for association measures
library(vcd)

# Calculate Cramer's V for promotional video
cramer_v_promotional <- assocstats(contingency_table_promotional_video)$cramer

# Calculate Cramer's V for corporate video
cramer_v_corporate <- assocstats(contingency_table_corporate_video)$cramer

# Calculate Cramer's V for motorsport video
cramer_v_motorsport <- assocstats(contingency_table_motorsport_video)$cramer

# Calculate Cramer's V for no video
cramer_v_no_video <- assocstats(contingency_table_no_video)$cramer

# Print Cramer's V
print(cramer_v_promotional)
print(cramer_v_corporate)
print(cramer_v_motorsport)
print(cramer_v_no_video)



## CONTINGENGY TABLES GENDER
# Define the colors for the mosaic plots
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

## NO VIDEO Gender
# Create a subset of the relevant columns for correlation analysis
subset_data_no_video_gender <- subset(no_video_df, select = c("Gender", "No video"))

# Create the contingency table
contingency_table_no_video_gender <- table(subset_data_no_video_gender$Gender, subset_data_no_video_gender$`No video`)

# Print the contingency table
print(contingency_table_no_video_gender)

# Create a mosaic plot of the contingency table
mosaicplot(contingency_table_no_video_gender, color = colors, main = "Contingency Table Gender - No Video", las = 1)



## MOTORSPORT VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_motorsport_video_gender <- subset(motorsport_video_df, select = c("Gender", "Motorsport video"))

# Create the contingency table
contingency_table_motorsport_video_gender <- table(subset_data_motorsport_video_gender$Gender, subset_data_motorsport_video_gender$`Motorsport video`)

# Print the contingency table
print(contingency_table_motorsport_video_gender)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_motorsport_video_gender, color = colors, main = "Contingency Table Gender - Motorsport Video",
           cex.axis = 1, las = 1)


## CORPORATE VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_corporate_video_gender <- subset(corporate_video_df, select = c("Gender", "Corporate video"))

# Create the contingency table
contingency_table_corporate_video_gender <- table(subset_data_corporate_video_gender$Gender, subset_data_corporate_video_gender$`Corporate video`)

# Print the contingency table
print(contingency_table_corporate_video_gender)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_corporate_video_gender, color = colors, main = "Contingency Table Gender - Corporate Video",
           cex.axis = 1, las = 1)


## PROMOTIONAL VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_promotional_video_gender <- subset(promotional_video_df, select = c("Gender", "Promotional video"))

# Create the contingency table
contingency_table_promotional_video_gender <- table(subset_data_promotional_video_gender$Gender, subset_data_promotional_video_gender$`Promotional video`)

# Print the contingency table
print(contingency_table_promotional_video_gender)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_promotional_video_gender, color = colors, main = "Contingency Table Gender - Promotional Video",
           cex.axis = 1, las = 1)

#Perform the chi-squared test
chi_squared_promotional_gender <- chisq.test(contingency_table_promotional_video_gender)
chi_squared_corporate_gender <- chisq.test(contingency_table_corporate_video_gender)
chi_squared_motorsport_gender <- chisq.test(contingency_table_motorsport_video_gender)
chi_squared_no_video_gender<- chisq.test(contingency_table_no_video_gender)

# Print the test results
print(chi_squared_promotional_gender)
print(chi_squared_corporate_gender)
print(chi_squared_motorsport_gender)
print(chi_squared_no_video_gender)

# Calculate Cramer's V for promotional video
cramer_v_promotional_gender <- assocstats(contingency_table_promotional_video_gender)$cramer

# Calculate Cramer's V for corporate video
cramer_v_corporate_gender <- assocstats(contingency_table_corporate_video_gender)$cramer

# Calculate Cramer's V for motorsport video
cramer_v_motorsport_gender <- assocstats(contingency_table_motorsport_video_gender)$cramer

# Calculate Cramer's V for no video
cramer_v_no_video_gender <- assocstats(contingency_table_no_video_gender)$cramer

# Print Cramer's V
print(cramer_v_promotional_gender)
print(cramer_v_corporate_gender)
print(cramer_v_motorsport_gender)
print(cramer_v_no_video_gender)






## CONTINGENGY TABLES Level of education
# Define the colors for the mosaic plots
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

## NO VIDEO education
# Create a subset of the relevant columns for correlation analysis
subset_data_no_video_education <- subset(no_video_df, select = c("Level of education", "No video"))

# Create the contingency table
contingency_table_no_video_education <- table(subset_data_no_video_education$'Level of education', subset_data_no_video_education$`No video`)

# Print the contingency table
print(contingency_table_no_video_education)

# Create a mosaic plot of the contingency table
mosaicplot(contingency_table_no_video_education, color = colors, main = "Contingency Table Level of education - No Video", las = 1)



## MOTORSPORT VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_motorsport_video_education <- subset(motorsport_video_df, select = c("Level of education", "Motorsport video"))

# Create the contingency table
contingency_table_motorsport_video_education <- table(subset_data_motorsport_video_education$'Level of education', subset_data_motorsport_video_education$`Motorsport video`)

# Print the contingency table
print(contingency_table_motorsport_video_education)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_motorsport_video_education, color = colors, main = "Contingency Table Level of education - Motorsport Video",
           cex.axis = 1, las = 1)


## CORPORATE VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_corporate_video_education <- subset(corporate_video_df, select = c("Level of education", "Corporate video"))

# Create the contingency table
contingency_table_corporate_video_education <- table(subset_data_corporate_video_education$'Level of education', subset_data_corporate_video_education$`Corporate video`)

# Print the contingency table
print(contingency_table_corporate_video_education)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_corporate_video_education, color = colors, main = "Contingency Table Education - Corporate Video",
           cex.axis = 1, las = 1)


## PROMOTIONAL VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_promotional_video_education <- subset(promotional_video_df, select = c("Level of education", "Promotional video"))

# Create the contingency table
contingency_table_promotional_video_education <- table(subset_data_promotional_video_education$'Level of education', subset_data_promotional_video_education$`Promotional video`)

# Print the contingency table
print(contingency_table_promotional_video_education)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_promotional_video_education, color = colors, main = "Contingency Table Education - Promotional Video",
           cex.axis = 1, las = 1)

#Perform the chi-squared test
chi_squared_promotional_education <- chisq.test(contingency_table_promotional_video_education)
chi_squared_corporate_education <- chisq.test(contingency_table_corporate_video_education)
chi_squared_motorsport_education <- chisq.test(contingency_table_motorsport_video_education)
chi_squared_no_video_education<- chisq.test(contingency_table_no_video_education)

# Print the test results
print(chi_squared_promotional_education)
print(chi_squared_corporate_education)
print(chi_squared_motorsport_education)
print(chi_squared_no_video_education)

# Calculate Cramer's V for promotional video
cramer_v_promotional_education <- assocstats(contingency_table_promotional_video_education)$cramer

# Calculate Cramer's V for corporate video
cramer_v_corporate_education <- assocstats(contingency_table_corporate_video_education)$cramer

# Calculate Cramer's V for motorsport video
cramer_v_motorsport_education <- assocstats(contingency_table_motorsport_video_education)$cramer

# Calculate Cramer's V for no video
cramer_v_no_video_education <- assocstats(contingency_table_no_video_education)$cramer

# Print Cramer's V
print(cramer_v_promotional_education)
print(cramer_v_corporate_education)
print(cramer_v_motorsport_education)
print(cramer_v_no_video_education)




## CONTINGENGY TABLES Employment status
# Define the colors for the mosaic plots
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

## NO VIDEO education
# Create a subset of the relevant columns for correlation analysis
subset_data_no_video_employment <- subset(no_video_df, select = c("Employment status", "No video"))

# Create the contingency table
contingency_table_no_video_employment <- table(subset_data_no_video_employment$'Employment status', subset_data_no_video_employment$`No video`)

# Print the contingency table
print(contingency_table_no_video_employment)

# Create a mosaic plot of the contingency table
mosaicplot(contingency_table_no_video_employment, color = colors, main = "Contingency Table Employment status - No Video", las = 1)



## MOTORSPORT VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_motorsport_video_employment <- subset(motorsport_video_df, select = c("Employment status", "Motorsport video"))

# Create the contingency table
contingency_table_motorsport_video_employment <- table(subset_data_motorsport_video_employment$'Employment status', subset_data_motorsport_video_employment$`Motorsport video`)

# Print the contingency table
print(contingency_table_motorsport_video_employment)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_motorsport_video_employment, color = colors, main = "Contingency Table Employment status - Motorsport Video",
           cex.axis = 1, las = 1)


## CORPORATE VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_corporate_video_employment <- subset(corporate_video_df, select = c("Employment status", "Corporate video"))

# Create the contingency table
contingency_table_corporate_video_employment <- table(subset_data_corporate_video_employment$'Employment status', subset_data_corporate_video_employment$`Corporate video`)

# Print the contingency table
print(contingency_table_corporate_video_employment)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_corporate_video_employment, color = colors, main = "Contingency Table Employment status - Corporate Video",
           cex.axis = 1, las = 1)


## PROMOTIONAL VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_promotional_video_employment <- subset(promotional_video_df, select = c("Employment status", "Promotional video"))

# Create the contingency table
contingency_table_promotional_video_employment <- table(subset_data_promotional_video_employment$'Employment status', subset_data_promotional_video_employment$`Promotional video`)

# Print the contingency table
print(contingency_table_promotional_video_employment)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_promotional_video_employment, color = colors, main = "Contingency Table Employment status - Promotional Video",
           cex.axis = 1, las = 1)

#Perform the chi-squared test
chi_squared_promotional_employment <- chisq.test(contingency_table_promotional_video_employment)
chi_squared_corporate_employment <- chisq.test(contingency_table_corporate_video_employment)
chi_squared_motorsport_employment <- chisq.test(contingency_table_motorsport_video_employment)
chi_squared_no_video_employment<- chisq.test(contingency_table_no_video_employment)

# Print the test results
print(chi_squared_promotional_employment)
print(chi_squared_corporate_employment)
print(chi_squared_motorsport_employment)
print(chi_squared_no_video_employment)

# Calculate Cramer's V for promotional video
cramer_v_promotional_employment <- assocstats(contingency_table_promotional_video_employment)$cramer

# Calculate Cramer's V for corporate video
cramer_v_corporate_employment <- assocstats(contingency_table_corporate_video_employment)$cramer

# Calculate Cramer's V for motorsport video
cramer_v_motorsport_employment <- assocstats(contingency_table_motorsport_video_employment)$cramer

# Calculate Cramer's V for no video
cramer_v_no_video_employment <- assocstats(contingency_table_no_video_employment)$cramer

# Print Cramer's V
print(cramer_v_promotional_employment)
print(cramer_v_corporate_employment)
print(cramer_v_motorsport_employment)
print(cramer_v_no_video_employment)






## Motorsport VIDEO cross analysis with motorsport interest
# Create a subset of the relevant columns for correlation analysis
subset_data_motorsport_video_interest <- subset(motorsport_video_df, select = c("Motorsport interest", "Motorsport video"))

# Create the contingency table
contingency_table_motorsport_video_interest <- table(subset_data_motorsport_video_interest$'Motorsport interest', subset_data_motorsport_video_interest$`Motorsport video`)

# Print the contingency table
print(contingency_table_motorsport_video_interest)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_motorsport_video_interest, color = colors, main = "Contingency Table Motorsport Interest - Motorsport Video",
           cex.axis = 0.6, las = 1)

# Transpose the contingency table
transposed_table_motorsport_video_interest <- t(contingency_table_motorsport_video_interest)

# Create a mosaic plot of the transposed contingency table with colors
mosaicplot(transposed_table_motorsport_video_interest, color = colors, main = "Contingency Table Motorsport Interest - Motorsport Video",
           cex.axis = 0.6, las = 1)



#Perform the chi-squared test
chi_squared_motorsport_interest <- chisq.test(contingency_table_motorsport_video_interest)
# Print the test results
print(chi_squared_motorsport_interest)

# Calculate Cramer's V
cramer_v_motorsport_interest <- assocstats(contingency_table_motorsport_video_interest)$cramer
# Print Cramer's V
print(cramer_v_motorsport_interest)






## CONTINGENGY TABLES Owning a car
# Define the colors for the mosaic plots
colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

## NO VIDEO education
# Create a subset of the relevant columns for correlation analysis
subset_data_no_video_ownership <- subset(no_video_df, select = c("Car ownership", "No video"))

# Create the contingency table
contingency_table_no_video_ownership <- table(subset_data_no_video_ownership$'Car ownership', subset_data_no_video_ownership$`No video`)

# Print the contingency table
print(contingency_table_no_video_ownership)

# Create a mosaic plot of the contingency table
mosaicplot(contingency_table_no_video_ownership, color = colors, main = "Contingency Table Car ownership - No Video", las = 1)



## MOTORSPORT VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_motorsport_video_ownership <- subset(motorsport_video_df, select = c("Car ownership", "Motorsport video"))

# Create the contingency table
contingency_table_motorsport_video_ownership <- table(subset_data_motorsport_video_ownership$'Car ownership', subset_data_motorsport_video_ownership$`Motorsport video`)

# Print the contingency table
print(contingency_table_motorsport_video_ownership)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_motorsport_video_ownership, color = colors, main = "Contingency Table Car ownership - Motorsport Video",
           cex.axis = 1, las = 1)


## CORPORATE VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_corporate_video_ownership <- subset(corporate_video_df, select = c("Car ownership", "Corporate video"))

# Create the contingency table
contingency_table_corporate_video_ownership <- table(subset_data_corporate_video_ownership$'Car ownership', subset_data_corporate_video_ownership$`Corporate video`)

# Print the contingency table
print(contingency_table_corporate_video_ownership)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_corporate_video_ownership, color = colors, main = "Contingency Table Car ownership - Corporate Video",
           cex.axis = 1, las = 1)


## PROMOTIONAL VIDEO
# Create a subset of the relevant columns for correlation analysis
subset_data_promotional_video_ownership <- subset(promotional_video_df, select = c("Car ownership", "Promotional video"))

# Create the contingency table
contingency_table_promotional_video_ownership <- table(subset_data_promotional_video_ownership$'Car ownership', subset_data_promotional_video_ownership$`Promotional video`)

# Print the contingency table
print(contingency_table_promotional_video_ownership)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_promotional_video_ownership, color = colors, main = "Contingency Table Car ownership - Promotional Video",
           cex.axis = 1, las = 1)

#Perform the chi-squared test
chi_squared_promotional_ownership <- chisq.test(contingency_table_promotional_video_ownership)
chi_squared_corporate_ownership <- chisq.test(contingency_table_corporate_video_ownership)
chi_squared_motorsport_ownership <- chisq.test(contingency_table_motorsport_video_ownership)
chi_squared_no_video_ownership<- chisq.test(contingency_table_no_video_ownership)

# Print the test results
print(chi_squared_promotional_ownership)
print(chi_squared_corporate_ownership)
print(chi_squared_motorsport_ownership)
print(chi_squared_no_video_ownership)

# Calculate Cramer's V for promotional video gender
cramer_v_promotional_ownership <- assocstats(contingency_table_promotional_video_ownership)$cramer

# Calculate Cramer's V for corporate video
cramer_v_corporate_ownership <- assocstats(contingency_table_corporate_video_ownership)$cramer

# Calculate Cramer's V for motorsport video
cramer_v_motorsport_ownership <- assocstats(contingency_table_motorsport_video_ownership)$cramer

# Calculate Cramer's V for no video
cramer_v_no_video_ownership <- assocstats(contingency_table_no_video_ownership)$cramer

# Print Cramer's V
print(cramer_v_promotional_ownership)
print(cramer_v_corporate_ownership)
print(cramer_v_motorsport_ownership)
print(cramer_v_no_video_ownership)





## Motorsport VIDEO cross analysis with motorsport as an effective marketing tool
# Create a subset of the relevant columns for correlation analysis
subset_data_motorsport_video_marketing <- subset(motorsport_video_df, select = c("Effectiveness of motorsport as a marketing tool", "Motorsport video"))

# Create the contingency table
contingency_table_motorsport_video_marketing <- table(subset_data_motorsport_video_marketing$'Effectiveness of motorsport as a marketing tool', subset_data_motorsport_video_marketing$`Motorsport video`)

# Print the contingency table
print(contingency_table_motorsport_video_marketing)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_motorsport_video_marketing, color = colors, main = "Contingency Table Effectiveness of motorsport as a marketing tool - Motorsport Video",
           cex.axis = 0.6, las = 1)


#Perform the chi-squared test
chi_squared_motorsport_marketing <- chisq.test(contingency_table_motorsport_video_marketing)
# Print the test results
print(chi_squared_motorsport_marketing)

# Calculate Cramer's V 
cramer_v_motorsport_marketing <- assocstats(contingency_table_motorsport_video_marketing)$cramer
# Print Cramer's V
print(cramer_v_motorsport_marketing)







## Motorsport VIDEO cross analysis with respondents saying they've already been influenced by a brand's involvement in motorsports
# Create a subset of the relevant columns for correlation analysis
subset_data_motorsport_video_influence <- subset(motorsport_video_df, select = c("Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?", "Motorsport video"))

# Create the contingency table
contingency_table_motorsport_video_influence <- table(subset_data_motorsport_video_influence$'Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?', subset_data_motorsport_video_marketing$`Motorsport video`)

# Print the contingency table
print(contingency_table_motorsport_video_influence)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_motorsport_video_influence, color = colors, main = "Contingency Table Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport? - Motorsport Video",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_motorsport_influence <- chisq.test(contingency_table_motorsport_video_influence)
# Print the test results
print(chi_squared_motorsport_influence)

# Calculate Cramer's V
cramer_v_motorsport_influence <- assocstats(contingency_table_motorsport_video_influence)$cramer
# Print Cramer's V
print(cramer_v_motorsport_influence)




## Motorsport VIDEO cross analysis with respondents saying the level of importance of a link with motorsport
subset_data_motorsport_video_link <- subset(motorsport_video_df, select = c("Importance of the link with motorsport", "Motorsport video"))

# Create the contingency table
contingency_table_motorsport_video_link <- table(subset_data_motorsport_video_link$'Importance of the link with motorsport', subset_data_motorsport_video_link$`Motorsport video`)

# Print the contingency table
print(contingency_table_motorsport_video_link)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_motorsport_video_link, color = colors, main = "Contingency Table Importance of the link with motorsport - Motorsport Video",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_motorsport_link <- chisq.test(contingency_table_motorsport_video_link)
# Print the test results
print(chi_squared_motorsport_link)

# Calculate Cramer's V 
cramer_v_motorsport_link <- assocstats(contingency_table_motorsport_video_link)$cramer
# Print Cramer's V
print(cramer_v_motorsport_link)




## Link between interest and knowledge
subset_data_interest_knowledge <- subset(survey_data, select = c("Motorsport interest", "Motorsport knowledge"))

# Create the contingency table
contingency_table_interest_knowledge <- table(subset_data_interest_knowledge$'Motorsport interest', subset_data_interest_knowledge$`Motorsport knowledge`)

# Print the contingency table
print(contingency_table_interest_knowledge)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_interest_knowledge, color = colors, main = "Contingency Table Motorsport interest and Motorsport knowledge",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_interest_knowledge <- chisq.test(contingency_table_interest_knowledge)
# Print the test results
print(chi_squared_interest_knowledge)

# Calculate Cramer's V 
cramer_v_interest_knowledge <- assocstats(contingency_table_interest_knowledge)$cramer
# Print Cramer's V
print(cramer_v_interest_knowledge)




## Link between interest and importance of the link with motorsport
subset_data_interest_link <- subset(survey_data, select = c("Motorsport interest", "Importance of the link with motorsport"))

# Create the contingency table
contingency_table_interest_link <- table(subset_data_interest_link$'Motorsport interest', subset_data_interest_link$`Importance of the link with motorsport`)

# Print the contingency table
print(contingency_table_interest_link)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_interest_link, color = colors, main = "Contingency Table Motorsport interest and Importance of the link with motorsport",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_interest_link <- chisq.test(contingency_table_interest_link)
# Print the test results
print(chi_squared_interest_link)

# Calculate Cramer's V
cramer_v_interest_link <- assocstats(contingency_table_interest_link)$cramer
# Print Cramer's V
print(cramer_v_interest_link)



# Load the 'corrplot' package
library(corrplot)

# Compute the correlation matrix
correlation_matrix <- cor(contingency_table_interest_link)

# Plot the correlation matrix as a matrix
corrplot(correlation_matrix, method = "number", type = "lower", tl.col = "black")





## Link between ownership and importance of the link with motorsport
subset_data_ownership_link <- subset(survey_data, select = c("Car ownership", "Importance of the link with motorsport"))

# Create the contingency table
contingency_table_ownership_link <- table(subset_data_ownership_link$'Car ownership', subset_data_ownership_link$`Importance of the link with motorsport`)

# Print the contingency table
print(contingency_table_ownership_link)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_ownership_link, color = colors, main = "Contingency Table Car ownership and Importance of the link with motorsport",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_ownership_link <- chisq.test(contingency_table_ownership_link)
# Print the test results
print(chi_squared_ownership_link)

# Calculate Cramer's V
cramer_v_ownership_link <- assocstats(contingency_table_ownership_link)$cramer
# Print Cramer's V
print(cramer_v_ownership_link)





## Link between ownership and importance of the link with motorsport
subset_data_cartype_link <- subset(survey_data, select = c("Type of car", "Importance of the link with motorsport"))
subset_data_cartype_link <- na.omit(subset_data_cartype_link[subset_data_cartype_link$"Type of car" != "", ])

# Create the contingency table
contingency_table_cartype_link <- table(subset_data_cartype_link$'Type of car', subset_data_cartype_link$`Importance of the link with motorsport`)

# Print the contingency table
print(contingency_table_cartype_link)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_cartype_link, color = colors, main = "Contingency Table Car type and Importance of the link with motorsport",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_cartype_link <- chisq.test(contingency_table_cartype_link)
# Print the test results
print(chi_squared_cartype_link)

# Calculate Cramer's V 
cramer_v_cartype_link <- assocstats(contingency_table_cartype_link)$cramer
# Print Cramer's V
print(cramer_v_cartype_link)





## Link between effectiveness marketing tool et influence
subset_data_marketing_influence <- subset(survey_data, select = c("Effectiveness of motorsport as a marketing tool", "Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?"))

# Create the contingency table
contingency_table_marketing_influence <- table(subset_data_marketing_influence$'Effectiveness of motorsport as a marketing tool', subset_data_marketing_influence$`Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?`)

# Print the contingency table
print(contingency_table_marketing_influence)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_marketing_influence, color = colors, main = "Contingency Table Marketing tool and influence",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_marketing_influence <- chisq.test(contingency_table_marketing_influence)
# Print the test results
print(chi_squared_marketing_influence)

# Calculate Cramer's V 
cramer_v_marketing_influence <- assocstats(contingency_table_marketing_influence)$cramer
# Print Cramer's V
print(cramer_v_marketing_influence)





## Link between importance et influence
subset_data_importance_influence <- subset(survey_data, select = c("Importance of the link with motorsport", "Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?"))

# Create the contingency table
contingency_table_importance_influence <- table(subset_data_importance_influence$'Importance of the link with motorsport', subset_data_importance_influence$`Have you ever been influenced to purchase a car or any car part based on the involvement of its brand in motorsport?`)

# Print the contingency table
print(contingency_table_importance_influence)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_importance_influence, color = colors, main = "Contingency Importance of the link with motorsport and influence",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_importance_influence <- chisq.test(contingency_table_importance_influence)
# Print the test results
print(chi_squared_importance_influence)

# Calculate Cramer's V
cramer_v_importance_influence <- assocstats(contingency_table_importance_influence)$cramer
# Print Cramer's V
print(cramer_v_importance_influence)



## Link between motorsport interest and impact
subset_data_impact_interest <- subset(survey_data, select = c("Motorsport interest", "To what extent does motorsport have a global impact on car technology and innovation?"))

# Create the contingency table
contingency_table_impact_interest <- table(subset_data_impact_interest$'Motorsport interest', subset_data_impact_interest$`To what extent does motorsport have a global impact on car technology and innovation?`)

# Print the contingency table
print(contingency_table_impact_interest)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_impact_interest, color = colors, main = "Contingency Table Motorsport interest and global impact",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_impact_interest <- chisq.test(contingency_table_impact_interest)
# Print the test results
print(chi_squared_impact_interest)

# Calculate Cramer's V
cramer_v_impact_interest <- assocstats(contingency_table_impact_interest)$cramer
# Print Cramer's V
print(cramer_v_impact_interest)





## Link between impact and link with motorsprot
subset_data_impact_link <- subset(survey_data, select = c("Importance of the link with motorsport", "To what extent does motorsport have a global impact on car technology and innovation?"))

# Create the contingency table
contingency_table_impact_link <- table(subset_data_impact_link$'Importance of the link with motorsport', subset_data_impact_link$`To what extent does motorsport have a global impact on car technology and innovation?`)

# Print the contingency table
print(contingency_table_impact_link)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_impact_link, color = colors, main = "Contingency Table Importance of the link with motorsport and global impact",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_impact_link <- chisq.test(contingency_table_impact_link)
# Print the test results
print(chi_squared_impact_link)

# Calculate Cramer's V 
cramer_v_impact_link <- assocstats(contingency_table_impact_link)$cramer
# Print Cramer's V
print(cramer_v_impact_link)





## Link between motorsport interest and sustainability
subset_data_sustainable_interest <- subset(survey_data, select = c("Motorsport interest", "Can motorsport contribute to the development of more sustainable and environmentally-friendly car technologies?"))

# Create the contingency table
contingency_table_sustainable_interest <- table(subset_data_sustainable_interest$'Motorsport interest', subset_data_sustainable_interest$`Can motorsport contribute to the development of more sustainable and environmentally-friendly car technologies?`)

# Print the contingency table
print(contingency_table_sustainable_interest)

# Create a mosaic plot of the contingency table with colors
mosaicplot(contingency_table_sustainable_interest, color = colors, main = "Contingency Table Motorsport interest and sustainability",
           cex.axis = 1, las = 1)


#Perform the chi-squared test
chi_squared_sustainable_interest <- chisq.test(contingency_table_sustainable_interest)
# Print the test results
print(chi_squared_sustainable_interest)

# Calculate Cramer's V
cramer_v_sustainable_interest <- assocstats(contingency_table_sustainable_interest)$cramer
# Print Cramer's V
print(cramer_v_sustainable_interest)
