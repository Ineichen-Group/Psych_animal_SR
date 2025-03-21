############################
### Analysis psych animal ###
#############################
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gridExtra)
library(grid)
library(ggalt)
library(cowplot)
library(openxlsx)

# Read the Excel file
file_path <- "Data_V4.xlsx"
df <- read_excel(file_path)

plot_fill_color <- "steelblue"
table(df$`Reason for exclusion`)

# Filter out rows where "Included" == 0
df <- df %>% filter(Included == 1)

#assign low risk of bias score
df <- df %>%
  mutate(low_rob = ifelse(RoB_blind == "Yes" | RoB_random == "Yes" | RoB_welfare == "Yes", 1, 0))

# Filter data: Activate or deactivate these lines as needed
unique_substances <- unique(df$Substance)
unique_substances
# df <- df %>% filter(Substance == "Psilocybin")  # Run for a specific substance

# 1. Count and plot top 10 countries
df_countries <- df %>%
  mutate(Country = str_split(Country, ";")) %>%
  unnest(Country) %>%
  count(Country, sort = TRUE) %>%
  slice_head(n = 10)

country_plot <- ggplot(df_countries, aes(x = reorder(Country, n), y = n)) +
  geom_bar(stat = "identity", fill = plot_fill_color) +
  coord_flip() +
  labs(title = "", x = "Country", y = "Number of studies") +
  theme_minimal()

sum(df_countries$n[df_countries$Country == "USA"], na.rm = TRUE)
sum(df_countries$n[df_countries$Country == "USA"], na.rm = TRUE)/nrow(df)
sum(df_countries$n[df_countries$Country == "UK"], na.rm = TRUE)
sum(df_countries$n[df_countries$Country == "UK"], na.rm = TRUE)/nrow(df)
sum(df_countries$n[df_countries$Country == "Sweden"], na.rm = TRUE)
sum(df_countries$n[df_countries$Country == "Sweden"], na.rm = TRUE)/nrow(df)
sum(df_countries$n[df_countries$Country == "Canada"], na.rm = TRUE)
sum(df_countries$n[df_countries$Country == "Canada"], na.rm = TRUE)/nrow(df)
sum(df_countries$n[df_countries$Country == "Germany"], na.rm = TRUE)
sum(df_countries$n[df_countries$Country == "Germany"], na.rm = TRUE)/nrow(df)
sum(df_countries$n)

# 2. List unique substances
unique_substances <- unique(df$Substance)
unique_substances

# 3. Count and plot top 10 substances
df_substances <- df %>%
  count(Substance, sort = TRUE) %>%
  slice_head(n = 10)

# Create bar plot for number of studies per substance
substance_plot <- df %>%
  count(Substance, sort = TRUE) %>%
  slice_head(n = 10) %>%  # Take top 10 substances
  ggplot(aes(x = reorder(Substance, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "", y = "Number of studies") +
  theme_minimal(base_size = 12)

sum(df_substances$n[df_substances$Substance == "LSD"], na.rm = TRUE)
sum(df_substances$n[df_substances$Substance == "LSD"], na.rm = TRUE)/sum(df_substances$n)
sum(df_substances$n[df_substances$Substance == "5-MeO-DMT"], na.rm = TRUE)
sum(df_substances$n[df_substances$Substance == "5-MeO-DMT"], na.rm = TRUE)/sum(df_substances$n)
sum(df_substances$n[df_substances$Substance == "DMT"], na.rm = TRUE)
sum(df_substances$n[df_substances$Substance == "DMT"], na.rm = TRUE)/sum(df_substances$n)
sum(df_substances$n[df_substances$Substance == "Ibogaine"], na.rm = TRUE)
sum(df_substances$n[df_substances$Substance == "Ibogaine"], na.rm = TRUE)/sum(df_substances$n)
sum(df_substances$n[df_substances$Substance == "Mescaline"], na.rm = TRUE)
sum(df_substances$n[df_substances$Substance == "Mescaline"], na.rm = TRUE)/sum(df_substances$n)
sum(df_substances$n[df_substances$Substance == "Psilocybin"], na.rm = TRUE)
sum(df_substances$n[df_substances$Substance == "Psilocybin"], na.rm = TRUE)/sum(df_substances$n)
sum(df_substances$n[df_substances$Substance == "Salvinorin A"], na.rm = TRUE)
sum(df_substances$n[df_substances$Substance == "Salvinorin A"], na.rm = TRUE)/sum(df_substances$n)
sum(df_substances$n)
unique_substances

# 4. List unique species
unique_species <- unique(df$Species)
unique_species

# 5. Count and plot top 10 species
df_species <- df %>%
  count(Species, sort = TRUE) %>%
  slice_head(n = 10)

species_plot <- ggplot(df_species, aes(x = reorder(Species, n), y = n)) +
  geom_bar(stat = "identity", fill = plot_fill_color) +
  coord_flip() +
  labs(title = "", x = "", y = "Number of studies") +
  theme_minimal()

sum(df_species$n[df_species$Species == "Rats"], na.rm = TRUE)
sum(df_species$n[df_species$Species == "Rats"], na.rm = TRUE)/nrow(df)
sum(df_species$n[df_species$Species == "Mice"], na.rm = TRUE)
sum(df_species$n[df_species$Species == "Mice"], na.rm = TRUE)/nrow(df)

# 6. Count studies by sex category
df_sex_counts <- df %>%
  count(Sex, sort = TRUE) %>%
  filter(!is.na(Sex)) %>%  # Remove NA values
  mutate(Sex = factor(Sex, levels = rev(Sex[order(-n)]))) # Order by count descending

# Inverted bar plot with correct ordering
sex_plot <- ggplot(df_sex_counts, aes(x = reorder(Sex, n), y = n)) +
  geom_bar(stat = "identity", fill = plot_fill_color) +
  coord_flip() +
  labs(title = "", x = "", y = "Number of studies") +
  theme_minimal()

# Calculate percentage of studies using only male, only female, or both sexes (excluding "Not reported")
df_sex_percent <- df %>%
  filter(Sex %in% c("male", "female", "both")) %>%
  count(Sex) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(desc(percentage))

df_sex_percent

# 7. Calculate mean number of animals per study (excluding "Not reported")
df_mean_animals <- df %>%
  filter(N_animals != "Not reported") %>%
  mutate(N_animals = as.numeric(N_animals)) %>%
  summarise(mean_N_animals = median(N_animals, na.rm = TRUE))

min(as.numeric(df$N_animals), na.rm = TRUE)
max(as.numeric(df$N_animals), na.rm = TRUE)

df_mean_animals*nrow(df)

unique(df$N_animals)

# 8. List unique application forms and follow-up time
unique_application_forms <- unique(df$Application_regimen)
unique_application_forms

# Count and get the three most common application forms with proportions
total_studies <- nrow(df)

df_application_regimen_counts <- df %>%
  count(Application_regimen, sort = TRUE) %>%
  mutate(proportion = (n / total_studies) * 100)

top_3_application_regimens <- df_application_regimen_counts %>%
  slice_head(n = 3)

# Count occurrences of "Not reported"
count_not_reported_application_regimen <- sum(df$Application_regimen == "Not reported", na.rm = TRUE)
prop_not_reported_application_regimen <- (count_not_reported_application_regimen / total_studies) * 100

# Count studies with "single" or "multiple" applications and calculate proportions
df_application_frequency_counts <- df %>%
  count(Application_frequency) %>%
  filter(Application_frequency %in% c("single", "multiple")) %>%
  mutate(proportion = (n / total_studies) * 100)

# Print results
cat("Top 3 most common application forms:\n")
print(top_3_application_regimens)

cat(sprintf("\nApplication_regimen - Not reported: %d (%.2f%% of all studies)\n",
            count_not_reported_application_regimen, prop_not_reported_application_regimen))

cat("\nApplication frequency counts:\n")
print(df_application_frequency_counts)

# Count occurrences of each follow-up time
df_follow_up_counts <- df %>%
  count(Follow_up, sort = TRUE) %>%
  mutate(proportion = (n / nrow(df)) * 100)

# Get the most common follow-up time
most_common_follow_up <- df_follow_up_counts %>%
  slice_head(n = 1)

# Count and calculate the proportion of "Not reported"
count_not_reported_follow_up <- sum(df$Follow_up == "Not reported", na.rm = TRUE)
prop_not_reported_follow_up <- (count_not_reported_follow_up / nrow(df)) * 100

# Print results
cat("Most common follow-up time:\n")
print(most_common_follow_up)

cat(sprintf("\nFollow-up time - Not reported: %d (%.2f%% of all studies)\n",
            count_not_reported_follow_up, prop_not_reported_follow_up))


# 9. Count and plot top 10 application forms
# Create bar plot for number of studies per application regimen
application_regimen_plot <- df %>%
  count(Application_regimen, sort = TRUE) %>%
  slice_head(n = 10) %>%  # Take top 10 application regimens
  ggplot(aes(x = reorder(Application_regimen, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "", y = "Number of studies") +
  theme_minimal(base_size = 12)

# 10. List unique application regimens
unique_application_regimens <- unique(df$Application_frequency)
unique_application_regimens

# 11. Count and plot top 10 application regimens
df_application_regimens <- df %>%
  count(Application_frequency, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(df_application_regimens, aes(x = reorder(Application_frequency, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Application Regimens", x = "Application Regimen", y = "Count")

# 12. Count and plot top 10 categories
df_category <- df %>%
  count(Category, sort = TRUE) %>%
  slice_head(n = 10)

focus_plot <- ggplot(df_category, aes(x = reorder(Category, n), y = n)) +
  geom_bar(stat = "identity", fill = plot_fill_color) +
  coord_flip() +
  labs(title = "", x = "", y = "Number of studies") +
  theme_minimal()

sum(df_category$n[df_category$Category == "Behavioural Study"], na.rm = TRUE)
sum(df_category$n[df_category$Category == "Behavioural Study"], na.rm = TRUE)/sum(df_category$n)
sum(df_category$n[df_category$Category == "Mechanism"], na.rm = TRUE)
sum(df_category$n[df_category$Category == "Mechanism"], na.rm = TRUE)/sum(df_category$n)
sum(df_category$n)

# 13. Count and plot years
# Create a complete sequence of years
all_years <- as.data.frame(seq(min(as.numeric(df$Year), na.rm = TRUE), max(as.numeric(df$Year), na.rm = TRUE), by = 1))
colnames(all_years) <- "Year"

# Convert Year column to numeric and merge with full year sequence to include missing years
df_year <- df %>%
  filter(Year != "NA") %>%
  count(Year, sort = FALSE) %>%
  right_join(all_years, by = "Year") %>%
  mutate(n = replace_na(n, 0))  # Fill missing years with 0

# Plot
year_plot <- ggplot(df_year, aes(x = as.factor(Year), y = n)) +
  geom_bar(stat = "identity", fill = plot_fill_color) +
  labs(title = "", x = "", y = "Number of studies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 14. weight and age
# Count instances of "Not reported" in Weight_mean and Age
count_weight_not_reported <- sum(df$Weight_mean == "Not reported", na.rm = TRUE)
count_age_not_reported <- sum(df$Age == "Not reported", na.rm = TRUE)

# Calculate proportion
total_studies <- nrow(df)
prop_weight_not_reported <- count_weight_not_reported / total_studies * 100
prop_age_not_reported <- count_age_not_reported / total_studies * 100

# Print results
cat(sprintf("Weight_mean - Not reported: %d (%.2f%% of all studies)\n", 
            count_weight_not_reported, prop_weight_not_reported))
cat(sprintf("Age - Not reported: %d (%.2f%% of all studies)\n", 
            count_age_not_reported, prop_age_not_reported))


################################
##### Critical appraisal #######
################################
# Define RoB columns
rob_columns <- c("RoB_blind", "RoB_random", "RoB_welfare", "RoB_SampleSize",
                 "RoB_ConflictOfInterestStatement", "RoB_ConflictOfInterestReported",
                 "RoB_Industry", "RoB_ARRIVE")

# Count occurrences of "Yes", "No", and "unclear" for each RoB category
df_rob_summary <- df %>%
  select(all_of(rob_columns)) %>%
  pivot_longer(cols = everything(), names_to = "RoB_Item", values_to = "Response") %>%
  filter(Response %in% c("Yes", "unclear", "No")) %>%  # Ignore anything outside of these 3 levels
  group_by(RoB_Item, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Response = factor(Response, levels = c("Yes", "unclear", "No")))  # Switched order

# Normalize counts to percentages
df_rob_summary <- df_rob_summary %>%
  group_by(RoB_Item) %>%
  mutate(Percentage = (Count / sum(Count)))

# Order bars from highest to lowest "Yes" percentage
df_rob_summary <- df_rob_summary %>%
  group_by(RoB_Item) %>%
  mutate(Yes_Percentage = Percentage[Response == "Yes"]) %>%  # Extract "Yes" percentages
  ungroup() %>%
  mutate(RoB_Item = reorder(RoB_Item, Yes_Percentage, na.rm = TRUE))  # Reorder categories

# Rename labels for better readability
df_rob_summary <- df_rob_summary %>%
  mutate(RoB_Item = recode(RoB_Item,
                           "RoB_blind" = "Blinding",
                           "RoB_random" = "Randomization",
                           "RoB_welfare" = "Animal welfare statement",
                           "RoB_SampleSize" = "Sample size calculation",
                           "RoB_ConflictOfInterestStatement" = "Conf. of interest statement",
                           "RoB_ConflictOfInterestReported" = "Conf. of interest reported",
                           "RoB_Industry" = "Industry involvement",
                           "RoB_ARRIVE" = "ARRIVE guidelines"
  ))

# Define the **corrected** color palette (adjusted yellow for better contrast)
rob_colors <- c("Yes" = "#7bcd5b", "unclear" = "#FFD700", "No" = "#ff574e")

png("rob_figure.png", width = 10, height = 6, units = "in", res = 300)

# Create the risk of bias plot
ggplot(df_rob_summary, aes(x = RoB_Item, y = Percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = rob_colors,
                    name = "Risk of Bias",
                    labels = c("Yes (Low risk)", "Unclear", "No (High risk)"),
                    guide = guide_legend(reverse = TRUE)) +  # Ensures Yes is on the left
  coord_flip() +
  labs(title = "", x = "", y = "Percentage") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),  # Larger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Larger legend title
    axis.text.x = element_text(size = 14),  # Larger x-axis labels
    axis.text.y = element_text(size = 14),  # Larger y-axis labels
    axis.title.x = element_text(size = 16, face = "bold"),  # Larger x-axis title
    axis.title.y = element_text(size = 16, face = "bold")   # Larger y-axis title
  )  

dev.off()

############################## RUN ENTIRE CODE BELOW!!!
### Dose conversions ##########
##############################
# Define the Km values for species
km_values <- c("Rats" = 6, "Cat" = 9, "Mice" = 3, "Guinea-Pigs" = 8, 
               "Baboon" = 20, "Monkey" = 9, "Sheep" = 8.4, "Dogs" = 20, 
               "Rabbits" = 12, "Pigs" = 9, "Chickens" = 10, "Hamsters" = 5, 
               "Ferrets" = 7, "Marmosets" = 6)

human_km <- 37  # Km value for humans

df <- df %>% 
  mutate(Dose_to_convert_min = as.numeric(Dosage_min_mg_per_kg),
         Dose_to_convert_max = as.numeric(Dosage_max_mg_per_kg),
         animal_dose_perkg_mean = ifelse(is.na(Dosage_max_mg_per_kg), 
                                         Dosage_min_mg_per_kg,
                                         (Dosage_min_mg_per_kg + Dosage_max_mg_per_kg) / 2),
         HED_perkg_min = ifelse(Species %in% names(km_values),
                                Dose_to_convert_min / (human_km / km_values[Species]),
                                NA),
         HED_perkg_max = ifelse(Species %in% names(km_values),
                                Dose_to_convert_max / (human_km / km_values[Species]),
                                NA),
         HED_perkg_mean = ifelse(is.na(HED_perkg_max), 
                                 HED_perkg_min,
                                 (HED_perkg_min + HED_perkg_max) / 2),
         HED_per70kg_min = HED_perkg_min * 70,
         HED_per70kg_max = HED_perkg_max * 70,
         HED_per70kg_mean = HED_perkg_mean * 70)

# Now select the substance
options(scipen = 999)  # Turn off scientific notation
#selected_substance <- "LSD"
selected_substance <- "5-MeO-DMT"
# selected_substance <- "Ibogaine"
# selected_substance <- "Mescaline"
# selected_substance <- "Psilocybin"
# selected_substance <- "DMT"
# selected_substance <- "Salvinorin A"

# Define the threshold
threshold <- 52.5 # Adjust per substance: lsd = 0.15, 5-MeO-DMT = 11, psilocybin = 15, DMT = 52.5 mg, mescaline = 500 mg, salvinorin A = 713.125 mg, ibogaine = 120 mg

df_selected <- df %>% filter(Substance == selected_substance)

#Sensitivity analysis
#df_selected <- df_selected %>% filter(low_rob == 1) #Deactivate if you want all studies independent of Rob
#df_selected <- df_selected %>% filter(Tox_psychedelic_substance == "No") #Deactivate if you want all studies, not excluding Tox studies

# Calculate summary statistics only for selected substance
animal_dose_perkg_overallmean <- mean(df_selected$animal_dose_perkg_mean, na.rm = T)
animal_dose_perkg_overallmedian <- median(df_selected$animal_dose_perkg_mean, na.rm = T)
animal_dose_perkg_overallmin <- min(df_selected$Dosage_min_mg_per_kg, na.rm = T)
animal_dose_perkg_overallmax <- max(df_selected$Dosage_max_mg_per_kg, na.rm = T)

HED_perkg_overallmean <- mean(df_selected$HED_perkg_mean, na.rm = T)
HED_perkg_overallmedian <- median(df_selected$HED_perkg_mean, na.rm = T)
HED_perkg_overallmin <- min(df_selected$HED_perkg_min, na.rm = T)
HED_perkg_overallmax <- max(df_selected$HED_perkg_max, na.rm = T)

HED_per70kg_overallmean <- mean(df_selected$HED_per70kg_mean, na.rm = T) #per adult
HED_per70kg_overallmedian <- median(df_selected$HED_per70kg_mean, na.rm = T) #per adult
HED_per70kg_overallmin <- min(df_selected$HED_per70kg_min, na.rm = T) #per adult
HED_per70kg_overallmax <- max(df_selected$HED_per70kg_max, na.rm = T) #per adult

#Print data frame
write.xlsx(df_selected, "df_selected_sanity_check.xlsx")


#################################################
###### How many animal studies above dose  ######
#################################################

# Count total studies
total_studies <- nrow(df_selected)

# Count studies with min dose above the threshold
above_threshold <- nrow(df_selected %>% filter(HED_per70kg_mean > threshold))

# Calculate percentage
percentage_above <- (above_threshold / total_studies) * 100

# Print all result for table
cat(sprintf("Animal dose in mg per kg median: %.2f (min: %.2f - max: %.2f)\n", 
            animal_dose_perkg_overallmedian, 
            animal_dose_perkg_overallmin, 
            animal_dose_perkg_overallmax))
cat(sprintf("Animal dose in mg per kg mean: %.2f\n", animal_dose_perkg_overallmean))
cat(sprintf("HED in mg per kg median: %.2f (min: %.2f - max: %.2f)\n", 
            HED_perkg_overallmedian, 
            HED_perkg_overallmin, 
            HED_perkg_overallmax))
cat(sprintf("HED in mg per kg mean: %.2f\n", HED_perkg_overallmean))
cat(sprintf("HED in mg per 70 kg median: %.2f (min: %.2f - max: %.2f)\n", 
            HED_per70kg_overallmedian, 
            HED_per70kg_overallmin, 
            HED_per70kg_overallmax))
cat(sprintf("HED in mg per 70 kg mean: %.2f\n", HED_per70kg_overallmean))
cat(sprintf("Number of studies with Human-adjusted Dose_min above %.2f: %d\n", threshold, above_threshold))
cat(sprintf("Total number of studies: %d\n", total_studies))
cat(sprintf("Percentage of studies with Human-adjusted Dose_min above %.2f: %.2f%%\n", threshold, percentage_above))


#############################
### Statistical comparison ###
#############################

# Extract HED values
HED_values <- df_selected$HED_per70kg_mean

# Check if there are enough valid data points for statistical testing
if (length(HED_values) > 2) {
  
  cat("Data is assumed to be non-normally distributed. Running Wilcoxon signed-rank tests.\n")
  
  # Two-sided test (HED ≠ Threshold)
  test_two_sided <- wilcox.test(HED_values, mu = threshold, alternative = "two.sided")
  
  # One-sided tests
  test_greater <- wilcox.test(HED_values, mu = threshold, alternative = "greater")  # HED > threshold
  test_less <- wilcox.test(HED_values, mu = threshold, alternative = "less")  # HED < threshold
  
  # Print test results
  cat("\n### Two-Sided Test (HED ≠ Threshold) ###\n")
  print(test_two_sided)
  
  cat("\n### One-Sided Test: Is HED Significantly Greater Than Threshold? (HED > Threshold) ###\n")
  print(test_greater)
  
  cat("\n### One-Sided Test: Is HED Significantly Smaller Than Threshold? (HED < Threshold) ###\n")
  print(test_less)
  
} else {
  cat("Not enough data points for statistical testing.\n")
}

############################################
### Count studies outside clinical range ###
############################################

# Define clinical dose ranges per substance (in mg per 70kg)
clinical_dose_ranges <- list(
  "LSD" = c(0.1, 0.2),
  "5-MeO-DMT" = c(7, 15),
  "Ibogaine" = c(700, 1540),
  "Mescaline" = c(200, 500),
  "Psilocybin" = c(15, 25),
  "DMT" = c(35, 70),
  "Salvinorin A" = c(0.25, 1)
)

#Conservative clinical dose ranges
#clinical_dose_ranges <- list( #Deactivate if you want standard clinical ranges
#  "LSD" = c(0.05, 0.4),
#  "5-MeO-DMT" = c(3.5, 30),
#  "Ibogaine" = c(30, 360),
#  "Mescaline" = c(100, 1600),
#  "Psilocybin" = c(5, 40),
#  "DMT" = c(17.5, 140),
#  "Salvinorin A" = c(13.125, 2800)
#)

# Extract the selected substance name
selected_substance <- unique(df_selected$Substance)

# Check if the selected substance exists in the dose range dictionary
if (selected_substance %in% names(clinical_dose_ranges)) {
  lower_bound <- clinical_dose_ranges[[selected_substance]][1]
  upper_bound <- clinical_dose_ranges[[selected_substance]][2]
  
  # Filter out NA values in HED_per70kg_mean
  df_filtered <- df_selected %>% filter(!is.na(HED_per70kg_mean))
  
  # Count total valid studies for the selected substance
  total_valid_studies <- nrow(df_filtered)
  
  # Count studies below and above clinical range
  below_range <- sum(df_filtered$HED_per70kg_mean < lower_bound, na.rm = TRUE)
  above_range <- sum(df_filtered$HED_per70kg_mean > upper_bound, na.rm = TRUE)
  total_outside <- below_range + above_range
  
  # Calculate percentages excluding NAs
  percent_below <- (below_range / total_valid_studies) * 100
  percent_above <- (above_range / total_valid_studies) * 100
  percent_outside <- (total_outside / total_valid_studies) * 100
  
  # Print results
  cat(sprintf("\n### %s: Studies Outside Clinical Range ###\n", selected_substance))
  cat(sprintf("Total valid studies: %d\n", total_valid_studies))
  cat(sprintf("Below range: %d (%.2f%%)\n", below_range, percent_below))
  cat(sprintf("Above range: %d (%.2f%%)\n", above_range, percent_above))
  cat(sprintf("Total outside clinical range: %d (%.2f%%)\n", total_outside, percent_outside))
} else {
  cat("\n### Selected substance not found in clinical dose range list. ###\n")
}





## Correlation analysis
# 1) Run a correlation test (Spearman's correlation due to non-normality)
df_valid <- df_selected %>% filter(!is.na(HED_per70kg_mean) & !is.na(Year))  # Remove NAs

cor_test <- cor.test(df_valid$HED_per70kg_mean, as.numeric(df_valid$Year), method = "spearman")

# Get the number of studies included in the correlation analysis
num_studies_cor <- nrow(df_valid)

# Print correlation test results and number of studies
cat("\n### Spearman Correlation Test Between HED and Year ###\n")
print(cor_test)
cat(sprintf("\nNumber of studies included in correlation analysis: %d\n", num_studies_cor))

# 2) Create and save the plot with a square format and larger text
png_filename <- paste0("HED_vs_Year_", gsub(" ", "_", selected_substance), ".png")

png(png_filename, width = 8, height = 8, units = "in", res = 300)  # Square format
ggplot(df_valid, aes(x = as.numeric(Year), y = HED_per70kg_mean)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 3) +  # Larger points
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 1.5) +  # Thicker smoothing line
  labs(
    x = "Publication Year", 
    y = paste("Mean Human-Equivalent Dose of", selected_substance, "(mg/70kg)"),
    title = paste("Spearman Correlation: rho =", round(cor_test$estimate, 3), 
                  "| p =", format.pval(cor_test$p.value, digits = 3), 
                  "| n =", num_studies_cor)
  ) +
  theme_minimal(base_size = 18) +  # Increase overall text size
  theme(
    plot.title = element_text(size = 20),  # Larger title
    axis.title = element_text(size = 20),  # Larger axis titles
    axis.text = element_text(size = 18),  # Larger axis tick labels
    plot.margin = margin(10, 10, 10, 10)  # Ensure nothing is cut off
  )
dev.off()





#Sanity check via visualization
# Plot mean doses with threshold on a logarithmic scale
ggplot(df_selected, aes(x = HED_per70kg_mean)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = threshold, color = "red", linetype = "dashed", size = 1) +
  scale_x_log10() +  # Use log scale for x-axis
  labs(
    title = "Distribution of Human-Equivalent Mean Doses (Log Scale)",
    x = "HED (mg/kg for 70kg human, Log Scale)",
    y = "Count"
  ) +
  theme_minimal()

####################################
###### Visualization #############
#################################

# Define x-axis limit
dose_max_limit <- 10 #Adjust per substance: lsd = 10, psilocybin = 50, dmt = 250, mescaline = 1000 

# Handle truncation and marking observations beyond the x-axis
df_selected <- df_selected %>%
  mutate(
    is_truncated = HED_per70kg_max > dose_max_limit,  # Flag truncated cases
    is_outside = HED_per70kg_min > dose_max_limit & HED_per70kg_max > dose_max_limit,  # Fully out-of-range
    HED_per70kg_max = ifelse(HED_per70kg_max > dose_max_limit, dose_max_limit, HED_per70kg_max),  # Cap at 20
    HED_per70kg_min = ifelse(HED_per70kg_min > dose_max_limit, dose_max_limit, HED_per70kg_min)   # Cap at 20
  )

# Create dataset for fully out-of-range points (place them at x = 21)
out_of_range_data <- df_selected %>%
  filter(is_outside) %>%
  mutate(HED_per70kg_min = dose_max_limit + 1,  # Place at x = 21
         HED_per70kg_max = dose_max_limit + 1)  # Just for plotting

# Create dumbbell plot with indicators
ggplot(df_selected, aes(y = as.factor(Year), x = HED_per70kg_min, xend = HED_per70kg_max, group = Study_ID)) +
  geom_dumbbell(size = 0.4, size_x = 1, size_xend = 1, aes(color = factor(Year))) +  # Regular dumbbells
  geom_point(aes(x = HED_per70kg_min, color = factor(Year)), size = 1) +  # Small dots for individual values
  geom_point(data = df_selected %>% filter(is_truncated), aes(x = HED_per70kg_max, y = as.factor(Year)), shape = 24, fill = "black", size = 1) +  # Triangle at x = 20 for truncated values
  geom_point(data = out_of_range_data, aes(x = HED_per70kg_min, y = as.factor(Year)), shape = 8, size = 1, color = "red") +  # Star at x = 21 for fully out-of-range
  scale_color_manual(values = scales::hue_pal()(length(unique(df_selected$Year))), name = "Year") +  
  scale_x_continuous(
    limits = c(0, dose_max_limit + 2),  # Allow space for out-of-range marker
    labels = scales::label_number(suffix = " mg")
  ) +
  geom_vline(xintercept = 0.1, linetype = "dotted", color = "gray50", alpha = 0.5) +
  geom_vline(xintercept = 0.2, linetype = "dotted", color = "black", alpha = 0.7) +
  labs(
    x = "Human-adjusted Dose (/70 kg)",
    y = "Year",
    title = "LSD" #Adjust
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

# Save the plot
ggsave("human_adjusted_dumbbell_plot.png", width = 10, height = 6, bg = "white")

##Boxplot
# Get top 6 substances with most studies
#top_substances <- df %>%
  #count(Substance, sort = TRUE) %>%
  #slice_head(n = 6) %>%
  #pull(Substance)

# Define substances in the correct order of study frequency
top_substances <- c("LSD", "5-MeO-DMT", "Ibogaine", "Mescaline", "Psilocybin", "DMT", "Salvinorin A")

# Define human dose reference values for the dotted line
human_dose_reference <- c(
  "LSD" = 0.20,
  "5-MeO-DMT" = 10,
  "Ibogaine" = 1400,
  "Mescaline" = 800,
  "Psilocybin" = 30,
  "DMT" = 50,
  "Salvinorin A" = 1
)

# Define function to generate individual boxplots with bold and larger substance names
generate_boxplot <- function(substance) {
  df_substance <- df %>% filter(Substance == substance)
  
  if (nrow(df_substance) == 0 || all(is.na(df_substance$HED_per70kg_mean))) {
    return(ggplot() + theme_void())  # Empty plot if no data
  }
  
  # Define max dose limit for better visualization
  dose_max_limit <- case_when(
    substance == "LSD" ~ 10,
    substance == "5-MeO-DMT" ~ 50,
    substance == "Ibogaine" ~ 1400,
    substance == "Mescaline" ~ 1000,
    substance == "Psilocybin" ~ 50,
    substance == "DMT" ~ 250,
    substance == "Salvinorin A" ~ 50,
    TRUE ~ 10
  )
  
  # Get clinical dose range
  lower_bound <- clinical_dose_ranges[[substance]][1]
  upper_bound <- clinical_dose_ranges[[substance]][2]
  
  df_substance <- df_substance %>%
    mutate(
      is_outside = HED_per70kg_mean > dose_max_limit,
      Substance = factor(Substance, levels = top_substances)  # Ensures correct ordering
    )
  
  num_outside <- sum(df_substance$is_outside, na.rm = TRUE)
  
  out_of_range_data <- df_substance %>%
    filter(is_outside) %>%
    mutate(HED_per70kg_mean = dose_max_limit + 1.5)
  
  p <- ggplot(df_substance, aes(y = HED_per70kg_mean, x = Substance)) +  
    geom_boxplot(fill = "steelblue", alpha = 0.3, width = 0.5, outlier.shape = NA) +  
    geom_jitter(aes(y = HED_per70kg_mean), width = 0.2, size = 2, alpha = 0.3) +  
    geom_point(data = out_of_range_data, aes(y = HED_per70kg_mean, x = Substance), 
               shape = 8, size = 3, color = "orange") +
    geom_hline(yintercept = lower_bound, linetype = "dotted", color = "red", size = 1) + 
    geom_hline(yintercept = upper_bound, linetype = "dotted", color = "red", size = 1) + 
    coord_flip() +
    scale_y_continuous(limits = c(0, dose_max_limit + 2), breaks = seq(0, dose_max_limit, by = dose_max_limit / 5)) +
    labs(x = "", y = "Human-equivalent dose (mg/70kg)", 
         caption = paste("*", num_outside, "observations exceed", dose_max_limit, "mg/70kg")) +
    theme_minimal(base_size = 14) +  # Increase base font size
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 16, face = "bold"),  # Make substance names bold and larger
      plot.margin = margin(5, 15, 5, 15) # Standardized margins for equal frame width
    )
  
  return(p)
}

# Generate boxplots for selected substances in the correct order
plot_list <- lapply(top_substances, generate_boxplot)

# Ensure no NULL values in plot list
plot_list <- Filter(Negate(is.null), plot_list)

# Combine all plots in a single column with equal widths
final_plot <- plot_grid(plotlist = plot_list, ncol = 1, align = "v", rel_widths = rep(1, length(plot_list)))

# Save the composite figure with corrected frame alignment
ggsave("hed_boxplot_top7_fixed.png", plot = final_plot, width = 10, height = length(plot_list) * 2, units = "in", dpi = 300, bg = "white")

#################################
####### Paper figures ###########
#################################

# Save the figure as a PNG file
png("summary_figure.png", width = 10, height = 14, units = "in", res = 300)

# Arrange and plot with left-aligned labels
grid.arrange(
  arrangeGrob(
    year_plot, 
    top = textGrob("A. Number of studies per year", x = 0, just = "left", gp = gpar(fontsize = 16, fontface = "bold"))
  ),
  arrangeGrob(
    species_plot, sex_plot, 
    ncol = 2,
    top = textGrob("  B. Species                                                                         C. Animal sex", x = 0, just = "left", gp = gpar(fontsize = 16, fontface = "bold"))
  ),
  arrangeGrob(
    focus_plot, country_plot, 
    ncol = 2,
    top = textGrob("  D. Study focus                                                                     E. Most prolific countries", x = 0, just = "left", gp = gpar(fontsize = 16, fontface = "bold"))
  ),
  arrangeGrob(
    substance_plot, application_regimen_plot,
    ncol = 2,
    top = textGrob("  F. Substance                                                                       G. Application regimen", 
                   x = 0, just = "left", gp = gpar(fontsize = 16, fontface = "bold"))
  ),
  ncol = 1, heights = c(1, 1, 1, 1)
)

# Close the graphics device
dev.off()


# If df_year does not include a 'Substance' column, create an aggregated dataset from the full data (df)
df_substance_year <- df %>% 
  filter(Substance %in% top_substances) %>% 
  group_by(Year, Substance) %>% 
  summarise(n = n(), .groups = "drop")

# Plot the aggregated data using a psychedelic color scheme
year_plot <- ggplot(df_substance_year, aes(x = as.factor(Year), y = n, fill = Substance)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = psychedelic_colors) +
  labs(title = "",
       x = "Publication Year",
       y = "Number of Studies",
       fill = "Substance") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 14),  # Bigger x-axis text
    axis.text.y = element_text(size = 14),  # Bigger y-axis text
    axis.title.x = element_text(size = 18),  # Bigger x-axis title
    axis.title.y = element_text(size = 18),  # Bigger y-axis title
    legend.title = element_text(size = 16),  # Bigger legend title
    legend.text = element_text(size = 14),  # Bigger legend text
    panel.background = element_rect(fill = "white", color = NA),  # White panel background
    plot.background = element_rect(fill = "white", color = NA)    # White plot background
  )

# Save the plot in wide format with 300 dpi resolution
ggsave("year_distribution_top7_wide.png", plot = year_plot, width = 12, height = 6, units = "in", dpi = 300, bg = "white")