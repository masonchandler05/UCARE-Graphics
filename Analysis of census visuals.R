
#importing dataset 
# Set the file path (adjust the file name to match your actual CSV file)
file_path <- "~/Downloads/IPUMS_microsample_data.csv"

# Load the CSV file into a data frame
census_data <- read.csv(file_path)

#seperates the whole data frame into 2 smaller ones split by whether they are from 1860 or 1870 to garentee complete datasets from each 
cen_1860 <- census_data[substr(census_data$SAMPLE,1,4) == "1860",]
cen_1870 <- census_data[substr(census_data$SAMPLE,1,4) == "1870",]

#creates a state lookup code to match state identifing number to the state: identifiers were copied straight from the IPUMS website 
#joining this with the main dataset 
library(dplyr)
library(tibble)

# Your lookup vector
state_code_lookup <- c(
  "1" = "Alabama", "5" = "Arkansas", "6" = "California", "9" = "Connecticut",
  "10" = "Delaware", "12" = "Florida", "13" = "Georgia", "17" = "Illinois",
  "18" = "Indiana", "19" = "Iowa", "20" = "Kansas", "21" = "Kentucky",
  "22" = "Louisiana", "23" = "Maine", "24" = "Maryland", "25" = "Massachusetts",
  "26" = "Michigan", "27" = "Minnesota", "28" = "Mississippi", "29" = "Missouri",
  "31" = "Nebraska", "33" = "New Hampshire", "34" = "New Jersey", "36" = "New York",
  "37" = "North Carolina", "39" = "Ohio", "41" = "Oregon", "42" = "Pennsylvania",
  "44" = "Rhode Island", "45" = "South Carolina", "47" = "Tennessee",
  "48" = "Texas", "50" = "Vermont", "51" = "Virginia", "55" = "Wisconsin"
)

# Convert to a tibble data frame so it can be joined to the dataset 
state_lookup_df <- tibble(
  STATEFIP = names(state_code_lookup),
  State = unname(state_code_lookup)
)

# Make sure STATEFIP is character in census_data for join
census_data <- census_data %>%
  mutate(STATEFIP = as.character(STATEFIP)) %>%
  left_join(state_lookup_df, by = "STATEFIP")

# filtering out only people that are either blind, deaf, dumb, idiotic, or insane
## 1538 of the 656954 met one of these criteria meaning a ~.235% disability rate amung these disabilities
census_data_disability <- census_data[
  census_data$BLIND == 2 |
    census_data$DEAF == 2 |
    census_data$IDIOTIC == 2 |
    census_data$INSANE == 2,
]
disability_rate <- (nrow(census_data_disability) / nrow(census_data)) * 100
disability_rate

# getting count data for each state seperated by year and state 
count_of_disability <- census_data_disability %>% 
  filter(!is.na(State)) %>%
  group_by(YEAR, STATEFIP) %>%
  summarise(
    blind_count = sum(BLIND == 2, na.rm = TRUE),
    deaf_count = sum(DEAF == 2, na.rm = TRUE),
    idiotic_count = sum(IDIOTIC == 2, na.rm = TRUE),
    insane_count = sum(INSANE == 2, na.rm = TRUE)
  )%>%
  left_join(state_lookup_df, by = "STATEFIP") %>%
  select(STATEFIP, State, everything())

# 1860 has 0 in [Deleware, florida, kansas, nebraska]
# 1870 has 0 in [oregon]
# adding these to the dataset 
missing_states <- data.frame(
  STATEFIP = c("10","12","20","31","41"),
  State = c("Delaware", "Florida", "Kansas", "Nebraska", "Oregon"),
  YEAR = c(1860, 1860, 1860, 1860, 1870),
  blind_count = 0,
  deaf_count = 0,
  idiotic_count = 0,
  insane_count = 0,
  stringsAsFactors = FALSE
)

count_of_disability2 <- rbind(count_of_disability, missing_states)

# turning pixel diameter measurements into areas 
file_path <- "~/Desktop/UCARE project with Dr. VanderPlas/Gimp Pixel Measurements.csv"
Gimp_Pixel_Measurements_1 <- read.csv(file_path)

pixel_counts <- Gimp_Pixel_Measurements_1 %>%
  mutate(
    blind_area_top   = pi * (blind_count_top / 2)^2,
    blind_area_side  = pi * (blind_count_side / 2)^2,
    
    deaf_area_top    = pi * (deaf_count_top / 2)^2,
    deaf_area_side   = pi * (deaf_count_side / 2)^2,
    
    idiotic_area_top = pi * (idiotic_count_top / 2)^2,
    idiotic_area_side= pi * (idiotic_count_side / 2)^2,
    
    insane_area_top  = pi * (insane_count_top / 2)^2,
    insane_area_side = pi * (insane_count_side / 2)^2
  )


# gathering totals for converting to total populations using the 1% microsample 
# 1860
blind_total_60 <- sum(census_data_disability$BLIND == 2 & census_data_disability$YEAR == 1860) * 100
deaf_total_60 <- sum(census_data_disability$DEAF == 2 & census_data_disability$YEAR == 1860) * 100
idiotic_total_60 <- sum(census_data_disability$IDIOTIC == 2 & census_data_disability$YEAR == 1860) * 100
insane_total_60 <- sum(census_data_disability$INSANE == 2 & census_data_disability$YEAR == 1860) * 100

# 1870
blind_total_70 <- sum(census_data_disability$BLIND == 2 & census_data_disability$YEAR == 1870) * 100
deaf_total_70 <- sum(census_data_disability$DEAF == 2 & census_data_disability$YEAR == 1870) * 100
idiotic_total_70 <- sum(census_data_disability$IDIOTIC == 2 & census_data_disability$YEAR == 1870) * 100
insane_total_70 <- sum(census_data_disability$INSANE == 2 & census_data_disability$YEAR == 1870) * 100

# adding up area totals to scale back down to total populations 
# 1860 totals
blind_top_area_60    <- sum(pixel_counts$blind_area_top[pixel_counts$YEAR == 1860])
blind_side_area_60   <- sum(pixel_counts$blind_area_side[pixel_counts$YEAR == 1860])

deaf_top_area_60     <- sum(pixel_counts$deaf_area_top[pixel_counts$YEAR == 1860])
deaf_side_area_60    <- sum(pixel_counts$deaf_area_side[pixel_counts$YEAR == 1860])

idiotic_top_area_60  <- sum(pixel_counts$idiotic_area_top[pixel_counts$YEAR == 1860])
idiotic_side_area_60 <- sum(pixel_counts$idiotic_area_side[pixel_counts$YEAR == 1860])

insane_top_area_60   <- sum(pixel_counts$insane_area_top[pixel_counts$YEAR == 1860])
insane_side_area_60  <- sum(pixel_counts$insane_area_side[pixel_counts$YEAR == 1860])

# 1870 totals
blind_top_area_70    <- sum(pixel_counts$blind_area_top[pixel_counts$YEAR == 1870])
blind_side_area_70   <- sum(pixel_counts$blind_area_side[pixel_counts$YEAR == 1870])

deaf_top_area_70     <- sum(pixel_counts$deaf_area_top[pixel_counts$YEAR == 1870])
deaf_side_area_70    <- sum(pixel_counts$deaf_area_side[pixel_counts$YEAR == 1870])

idiotic_top_area_70  <- sum(pixel_counts$idiotic_area_top[pixel_counts$YEAR == 1870])
idiotic_side_area_70 <- sum(pixel_counts$idiotic_area_side[pixel_counts$YEAR == 1870])

insane_top_area_70   <- sum(pixel_counts$insane_area_top[pixel_counts$YEAR == 1870])
insane_side_area_70  <- sum(pixel_counts$insane_area_side[pixel_counts$YEAR == 1870])

# making scalars to convert area into population 
# 1860 scalars
blind_60_scalar_top    <- blind_total_60 / blind_top_area_60
blind_60_scalar_side   <- blind_total_60 / blind_side_area_60

deaf_60_scalar_top     <- deaf_total_60 / deaf_top_area_60
deaf_60_scalar_side    <- deaf_total_60 / deaf_side_area_60

idiotic_60_scalar_top  <- idiotic_total_60 / idiotic_top_area_60
idiotic_60_scalar_side <- idiotic_total_60 / idiotic_side_area_60

insane_60_scalar_top   <- insane_total_60 / insane_top_area_60
insane_60_scalar_side  <- insane_total_60 / insane_side_area_60

# 1870 scalars
blind_70_scalar_top    <- blind_total_70 / blind_top_area_70
blind_70_scalar_side   <- blind_total_70 / blind_side_area_70

deaf_70_scalar_top     <- deaf_total_70 / deaf_top_area_70
deaf_70_scalar_side    <- deaf_total_70 / deaf_side_area_70

idiotic_70_scalar_top  <- idiotic_total_70 / idiotic_top_area_70
idiotic_70_scalar_side <- idiotic_total_70 / idiotic_side_area_70

insane_70_scalar_top   <- insane_total_70 / insane_top_area_70
insane_70_scalar_side  <- insane_total_70 / insane_side_area_70

# dividing area by scalars to get into actual population sized as estimated by the 1% microsample 
pixel_counts <- pixel_counts %>%
  mutate(
    blind_top_scaled = round(ifelse(YEAR == 1860, blind_area_top * blind_60_scalar_top, blind_area_top * blind_70_scalar_top)),
    blind_side_scaled = round(ifelse(YEAR == 1860, blind_area_side * blind_60_scalar_side, blind_area_side * blind_70_scalar_side)),
    
    deaf_top_scaled = round(ifelse(YEAR == 1860, deaf_area_top * deaf_60_scalar_top, deaf_area_top * deaf_70_scalar_top)),
    deaf_side_scaled = round(ifelse(YEAR == 1860, deaf_area_side * deaf_60_scalar_side, deaf_area_side * deaf_70_scalar_side)),
    
    idiotic_top_scaled = round(ifelse(YEAR == 1860, idiotic_area_top * idiotic_60_scalar_top, idiotic_area_top * idiotic_70_scalar_top)),
    idiotic_side_scaled = round(ifelse(YEAR == 1860, idiotic_area_side * idiotic_60_scalar_side, idiotic_area_side * idiotic_70_scalar_side)),
    
    insane_top_scaled = round(ifelse(YEAR == 1860, insane_area_top * insane_60_scalar_top, insane_area_top * insane_70_scalar_top)),
    insane_side_scaled = round(ifelse(YEAR == 1860, insane_area_side * insane_60_scalar_side, insane_area_side * insane_70_scalar_side))
  )

# scaling microsample data into full data 
count_of_disability2 <- count_of_disability2 %>%
  mutate(
    blind_count_scaled = blind_count * 100,
    deaf_count_scaled = deaf_count * 100,
    idiotic_count_scaled = idiotic_count * 100,
    insane_count_scaled = insane_count * 100
  )

# merginbg the dataset to plot 
library(dplyr)
merged_full_data <- inner_join(count_of_disability2, pixel_counts, by = c("State", "YEAR"))

# Plotting estimated populations from 1% microsample and pixel approximations (seperated by top to bottom measurements and side to side measurements)(seperated by 1860 and 1870)
## This just shows a regression fit and does not take into account the sample size and variance(need further analysis on that)
library(ggplot2)

# Blind Top
ggplot(merged_full_data, aes(x = blind_count_scaled, y = blind_top_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Scaled Blind Count (Microdata)",
    y = "Scaled Blind Area Top (Pixel Data)",
    title = "Blind Comparison - Top measurement"
  ) +
  facet_wrap(~YEAR) 

# Blind Side
ggplot(merged_full_data, aes(x = blind_count_scaled, y = blind_side_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Scaled Blind Count (Microdata)",
    y = "Scaled Blind Area Side (Pixel Data)",
    title = "Blind Comparison - Side measurement"
  ) +
  facet_wrap(~YEAR) 
# Deaf Top
ggplot(merged_full_data, aes(x = deaf_count_scaled, y = deaf_top_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Scaled Deaf Count (Microdata)",
    y = "Scaled Deaf Area Top (Pixel Data)",
    title = "Deaf Comparison - Top measurement"
  ) +
  facet_wrap(~YEAR) 

# Deaf Side
ggplot(merged_full_data, aes(x = deaf_count_scaled, y = deaf_side_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Scaled Deaf Count (Microdata)",
    y = "Scaled Deaf Area Side (Pixel Data)",
    title = "Deaf Comparison - Side measurement"
  ) +
  facet_wrap(~YEAR) 

# Insane Top
ggplot(merged_full_data, aes(x = insane_count_scaled, y = insane_top_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Scaled Insane Count (Microdata)",
    y = "Scaled Insane Area Top (Pixel Data)",
    title = "Insane Comparison - Top measurement"
  ) +
  facet_wrap(~YEAR) 

# Insane Side
ggplot(merged_full_data, aes(x = insane_count_scaled, y = insane_side_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Scaled Insane Count (Microdata)",
    y = "Scaled Insane Area Side (Pixel Data)",
    title = "Insane Comparison - Side measurement"
  ) +
  facet_wrap(~YEAR) 

# Idiotic Top
ggplot(merged_full_data, aes(x = idiotic_count_scaled, y = idiotic_top_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Scaled Idiotic Count (Microdata)",
    y = "Scaled Idiotic Area Top (Pixel Data)",
    title = "Idiotic Comparison - Top measurement"
  ) +
  facet_wrap(~YEAR) 

# Idiotic Side
ggplot(merged_full_data, aes(x = idiotic_count_scaled, y = idiotic_side_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Scaled Idiotic Count (Microdata)",
    y = "Scaled Idiotic Area Side (Pixel Data)",
    title = "Idiotic Comparison - Side measurement"
  ) +
  facet_wrap(~YEAR) 


#BREAK TO PLOTTING WITH FULL CENSUS DATA 

#importing data
file_path <- "~/Desktop/UCARE project with Dr. VanderPlas/Full data census.csv"
Full_data_census <- read.csv(file_path)

#Debugging to ensure df merge properly 
merged_full_data <- merged_full_data %>% mutate(YEAR = as.integer(YEAR))
Full_data_census <- Full_data_census[!apply(Full_data_census, 1, function(row) all(is.na(row) | row == "")), ]

# making all the scalars and totals for the full census data
#1860
blind_total_60_1 <- Full_data_census %>% filter(YEAR == 1860) %>% pull(Blind) %>% sum(na.rm = TRUE)
insane_total_60_1 <- Full_data_census %>% filter(YEAR == 1860) %>% pull(Insane) %>% sum(na.rm = TRUE)

blind_total_70_1 <- Full_data_census %>% filter(YEAR == 1870) %>% pull(Blind) %>% sum(na.rm = TRUE)
deaf_total_70_1 <- Full_data_census %>% filter(YEAR == 1870) %>% pull(Deaf) %>% sum(na.rm = TRUE)
idiotic_total_70_1 <- Full_data_census %>% filter(YEAR == 1870) %>% pull(Idiotic) %>% sum(na.rm = TRUE)
insane_total_70_1 <- Full_data_census %>% filter(YEAR == 1870) %>% pull(Insane) %>% sum(na.rm = TRUE)

# 1860 totals
blind_top_area_60_1   <- sum(pixel_counts$blind_area_top[pixel_counts$YEAR == 1860])
blind_side_area_60_1 <- sum(pixel_counts$blind_area_side[pixel_counts$YEAR == 1860])

insane_top_area_60_1   <- sum(pixel_counts$insane_area_top[pixel_counts$YEAR == 1860])
insane_side_area_60_1  <- sum(pixel_counts$insane_area_side[pixel_counts$YEAR == 1860])

# 1870 totals
blind_top_area_70_1    <- sum(pixel_counts$blind_area_top[pixel_counts$YEAR == 1870])
blind_side_area_70_1   <- sum(pixel_counts$blind_area_side[pixel_counts$YEAR == 1870])

deaf_top_area_70_1     <- sum(pixel_counts$deaf_area_top[pixel_counts$YEAR == 1870])
deaf_side_area_70_1    <- sum(pixel_counts$deaf_area_side[pixel_counts$YEAR == 1870])

idiotic_top_area_70_1  <- sum(pixel_counts$idiotic_area_top[pixel_counts$YEAR == 1870])
idiotic_side_area_70_1 <- sum(pixel_counts$idiotic_area_side[pixel_counts$YEAR == 1870])

insane_top_area_70_1   <- sum(pixel_counts$insane_area_top[pixel_counts$YEAR == 1870])
insane_side_area_70_1  <- sum(pixel_counts$insane_area_side[pixel_counts$YEAR == 1870])

# making scalars to convert area into population 
# 1860 scalars
blind_60_scalar_top_1    <- blind_total_60_1 / blind_top_area_60_1
blind_60_scalar_side_1   <- blind_total_60_1 / blind_side_area_60_1

insane_60_scalar_top_1   <- insane_total_60_1 / insane_top_area_60_1
insane_60_scalar_side_1  <- insane_total_60_1 / insane_side_area_60_1

# 1870 scalars
blind_70_scalar_top_1    <- blind_total_70_1 / blind_top_area_70_1
blind_70_scalar_side_1   <- blind_total_70_1 / blind_side_area_70_1

deaf_70_scalar_top_1     <- deaf_total_70_1 / deaf_top_area_70_1
deaf_70_scalar_side_1    <- deaf_total_70_1 / deaf_side_area_70_1

idiotic_70_scalar_top_1  <- idiotic_total_70_1 / idiotic_top_area_70_1
idiotic_70_scalar_side_1 <- idiotic_total_70_1 / idiotic_side_area_70_1

insane_70_scalar_top_1   <- insane_total_70_1 / insane_top_area_70_1
insane_70_scalar_side_1  <- insane_total_70_1 / insane_side_area_70_1

# dividing area by scalars to get into actual population sized as estimated by the 1% microsample 
pixel_counts <-pixel_counts %>%
  mutate(
    blind_top_scaled_full = round(ifelse(YEAR == 1860, blind_area_top * blind_60_scalar_top_1, blind_area_top * blind_70_scalar_top_1)),
    blind_side_scaled_full = round(ifelse(YEAR == 1860, blind_area_side * blind_60_scalar_side_1, blind_area_side * blind_70_scalar_side_1)),
    
    deaf_top_scaled_full = round(deaf_area_top * deaf_70_scalar_top_1),
    deaf_side_scaled_full = round( deaf_area_side * deaf_70_scalar_side_1),
    
    idiotic_top_scaled_full = round(idiotic_area_top * idiotic_70_scalar_top_1),
    idiotic_side_scaled_full = round(idiotic_area_side * idiotic_70_scalar_side_1),
    
    insane_top_scaled_full = round(ifelse(YEAR == 1860, insane_area_top * insane_60_scalar_top_1, insane_area_top * insane_70_scalar_top_1)),
    insane_side_scaled_full = round(ifelse(YEAR == 1860, insane_area_side * insane_60_scalar_side_1, insane_area_side * insane_70_scalar_side_1))
  )

# merging datasets again 
merged_full_data <- inner_join(count_of_disability2, pixel_counts, by = c("State", "YEAR"))

#cleaning and merging data
Full_data_census$State <- trimws(Full_data_census$State)
Full_data_census$State <- toupper(Full_data_census$State)
merged_full_data$State <- trimws(merged_full_data$State)
merged_full_data$State <- toupper(merged_full_data$State)
full_data_merge <- full_join(Full_data_census, merged_full_data, by = c("State", "YEAR"))



#plotting pixel measurements against full census data
# Blind - Side
ggplot(full_data_merge, aes(x = Blind, y = blind_side_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  facet_wrap(~YEAR) + 
  coord_fixed(ratio = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Blind Area Side (Pixel Data)",
    title = "Blind Comparison - Side Measurement"
  ) 

# Blind - Top
ggplot(full_data_merge, aes(x = Blind, y = blind_top_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  facet_wrap(~YEAR)+
  coord_fixed(ratio = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Blind Area Top (Pixel Data)",
    title = "Blind Comparison - Top Measurement"
  ) 
# Deaf - Side
ggplot(full_data_merge, aes(x = Deaf, y = deaf_side_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  coord_fixed(ratio = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Deaf Area Side (Pixel Data)",
    title = "Deaf Comparison - Side Measurement"
  ) 

# Deaf - Top
ggplot(full_data_merge, aes(x = Deaf, y = deaf_top_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  coord_fixed(ratio = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Deaf Area Top (Pixel Data)",
    title = "Deaf Comparison - Top Measurement"
  ) 

# Insane - Side
ggplot(full_data_merge, aes(x = Insane, y = insane_side_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  facet_wrap(~YEAR) + 
  coord_fixed(ratio = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Insane Area Side (Pixel Data)",
    title = "Insane Comparison - Side Measurement"
  ) 

# Insane - Top
ggplot(full_data_merge, aes(x = Insane, y = insane_top_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  facet_wrap(~YEAR) + 
  coord_fixed(ratio = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Insane Area Top (Pixel Data)",
    title = "Insane Comparison - Top Measurement"
  ) 

# Idiotic - Side
ggplot(full_data_merge, aes(x = Idiotic, y = idiotic_side_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  coord_fixed(ratio = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Idiotic Area Side (Pixel Data)",
    title = "Idiotic Comparison - Side Measurement"
  ) 

# Idiotic - Top
ggplot(full_data_merge, aes(x = Idiotic, y = idiotic_top_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  coord_fixed(ratio = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Idiotic Area Top (Pixel Data)",
    title = "Idiotic Comparison - Top Measurement"
  )

#converting to plotly to get hover-over features to see what states are the outliers 
library(plotly)

# Blind - Side
a <- ggplot(full_data_merge, aes(x = Blind, y = blind_side_scaled_full, text = paste("State:", State))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Blind Area Side (Pixel Data)",
    title = "Blind Comparison - Side Measurement"
  ) +
  theme_minimal()

# Blind - Top
b <- ggplot(full_data_merge, aes(x = Blind, y = blind_top_scaled_full, text = paste("State:", State))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Blind Area Top (Pixel Data)",
    title = "Blind Comparison - Top Measurement"
  ) +
  theme_minimal()

# Deaf - Side
c <- ggplot(full_data_merge, aes(x = Deaf, y = deaf_side_scaled_full, text = paste("State:", State))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Deaf Area Side (Pixel Data)",
    title = "Deaf Comparison - Side Measurement"
  ) +
  theme_minimal()

# Deaf - Top
d <- ggplot(full_data_merge, aes(x = Deaf, y = deaf_top_scaled_full, text = paste("State:", State))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Deaf Area Top (Pixel Data)",
    title = "Deaf Comparison - Top Measurement"
  ) +
  theme_minimal()

# Insane - Side
e <- ggplot(full_data_merge, aes(x = Insane, y = insane_side_scaled_full, text = paste("State:", State))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Insane Area Side (Pixel Data)",
    title = "Insane Comparison - Side Measurement"
  ) +
  theme_minimal()

# Insane - Top
f <- ggplot(full_data_merge, aes(x = Insane, y = insane_top_scaled_full, text = paste("State:", State))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Insane Area Top (Pixel Data)",
    title = "Insane Comparison - Top Measurement"
  ) +
  theme_minimal()

# Idiotic - Side
g <- ggplot(full_data_merge, aes(x = Idiotic, y = idiotic_side_scaled_full, text = paste("State:", State))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Idiotic Area Side (Pixel Data)",
    title = "Idiotic Comparison - Side Measurement"
  ) +
  theme_minimal()

# Idiotic - Top
h <- ggplot(full_data_merge, aes(x = Idiotic, y = idiotic_top_scaled_full, text = paste("State:", State))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Full Census Data",
    y = "Scaled Idiotic Area Top (Pixel Data)",
    title = "Idiotic Comparison - Top Measurement"
  ) +
  theme_minimal()

# Convert to interactive with tooltips
ggplotly(a, tooltip = "text")
ggplotly(b, tooltip = "text")
ggplotly(c, tooltip = "text")
ggplotly(d, tooltip = "text")
ggplotly(e, tooltip = "text")
ggplotly(f, tooltip = "text")
ggplotly(g, tooltip = "text")
ggplotly(h, tooltip = "text")
#Virginia is the main outlier in every graph. Its size was over represented in each and every graph 

#Comparing vertical and horizontal measurements to look for deformed states
ggplot(full_data_merge, aes(x = blind_top_scaled_full, y = blind_side_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_fixed(ratio = 1) + 
  labs(
    y = "side measurements",
    x = "top measurements",
    title = "Blind"
  ) 

ggplot(full_data_merge, aes(x = insane_top_scaled_full, y = insane_side_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_fixed(ratio = 1) + 
  labs(
    y = "side measurements",
    x = "top measurements",
    title = "insane"
  )

ggplot(full_data_merge, aes(x = deaf_top_scaled_full, y = deaf_side_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_fixed(ratio = 1) + 
  labs(
    y = "side measurements",
    x = "top measurements",
    title = "deaf"
  ) 

ggplot(full_data_merge, aes(x = idiotic_top_scaled_full, y = idiotic_side_scaled_full)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_fixed(ratio = 1) + 
  labs(
    y = "side measurements",
    x = "top measurements",
    title = "idiotic"
  ) 


# plotly graph for blind as it has a far off dot 
blind_dot <- ggplot(full_data_merge, aes(x = blind_top_scaled_full, y = blind_side_scaled_full, text = paste("State:", State, "<br>Year: ", YEAR))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  coord_fixed(ratio = 1) + 
  labs(
    x = "Full Census Data",
    y = "Scaled Idiotic Area Side (Pixel Data)",
    title = "Idiotic Comparison - Side Measurement"
  ) +
  theme_minimal()

ggplotly(blind_dot, tooltip = "text")
# Blind pennsilvanyia and new york are a little mis-shapen 

# looking for shape differences by size 
# adding differences and sums to dataset 
full_data_merge <- full_data_merge %>%
  mutate(
    vert_hor_difference_blind = blind_top_scaled_full - blind_side_scaled_full,
    vert_hor_avg_blind = (blind_top_scaled_full + blind_side_scaled_full) / 2,
    
    vert_hor_difference_deaf = deaf_top_scaled_full - deaf_side_scaled_full,
    vert_hor_avg_deaf = (deaf_top_scaled_full + deaf_side_scaled_full) / 2,
    
    vert_hor_difference_idiotic = idiotic_top_scaled_full - idiotic_side_scaled_full,
    vert_hor_avg_idiotic = (idiotic_top_scaled_full + idiotic_side_scaled_full) / 2,
    
    vert_hor_difference_insane = insane_top_scaled_full - insane_side_scaled_full,
    vert_hor_avg_insane = (insane_top_scaled_full + insane_side_scaled_full) / 2
  )

#plotting 
ggplot(full_data_merge, aes(y = vert_hor_difference_blind, x = vert_hor_avg_blind)) +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue") +
  geom_point() +
  facet_wrap(~YEAR)+
  labs(
    y = "vertical - horizontal difference",
    x = "average population",
    title = "Blind: Difference vs. Average"
  ) 

# Deaf
ggplot(full_data_merge, aes(x = vert_hor_avg_deaf, y = vert_hor_difference_deaf)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point() +
  facet_wrap(~YEAR) +
  labs(
    title = "Deaf: Difference vs. Average",
    y = "vertical - horizontal difference",
    x = "average population"
  )

# Idiotic
ggplot(full_data_merge, aes(x = vert_hor_avg_idiotic, y = vert_hor_difference_idiotic)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point() +
  facet_wrap(~YEAR) +
  labs(
    title = "Idiotic: Difference vs. Average",
    y = "vertical - horizontal difference",
    x = "average population"
  )

# Insane
ggplot(full_data_merge, aes(x = vert_hor_avg_insane, y = vert_hor_difference_insane)) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_point() +
  facet_wrap(~YEAR) +
  labs(
    title = "Insane: Difference vs. Average",
    y = "vertical - horizontal difference",
    x = "average population"
  ) 



