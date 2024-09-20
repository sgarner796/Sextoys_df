library(tidyverse)
library(gridExtra)
library(ggthemes)
library(gtsummary)
library(viridis)
library(gtable)
library(gt)
library(ggpattern)

install.packages("ggplot")

#C:/Users/steph/Documents/GitHub/Sextoys_df
#M:/p/Personal/Health
setwd("C:/Users/steph/Documents/GitHub/Sextoys_df")

labels <- read.csv("Household Data Labeled.csv")
raw <- read.csv("Household Data Raw.csv")


glimpse(labels)
glimpse(raw)

raw <- raw %>% 
  select(4:73)


## Initial graphs
## Change Normal to average

us_raw <- raw %>% 
  filter(country_origin == 1&
         screening_usage == 1) %>% 
  mutate(across(as.numeric()))

us_raw %>% 
  mutate(agex2 = age*2) %>% 
  ggplot(aes(x=agex2, y= career)) +
  geom_point()+
  theme_base()
  
## Demographics Graphs 

dist <- c("age", "gender_birth", "gender_expression","employment","career","household_income","access_sextoys")
graph_dist <- list()

## Distribution by type ##

for (i in dist) {
  graph <- us_raw %>% 
    ggplot(aes_string(x = i)) +
    geom_bar()+
    theme_few()
  
  graph_dist[[i]] <- graph
}

do.call(grid.arrange, c(graph_dist, ncol = 2))


graph_dist$age <- graph_dist$age +
  scale_x_continuous(breaks = 1:6)

graph_dist$household_income +
  geom_bar(aes(fill = as.factor(gender_birth)))
  
##Super users,Prunes,trad_only, house_only, grand_masters
  
us_raw <- us_raw %>% 
  mutate(household_sum = rowSums(select(.,30:39),na.rm = TRUE),
         traditional_sum = rowSums(select(.,28:32),na.rm = TRUE),
         total_toys = rowSums(select(.,28:39),na.rm = TRUE),
         household_toy = household_sum > 0,
         superuser = household_sum >= 3) 

## Some evidence that preference for household items exist, correlation between worries and the sex toy they pick???????

## Check profile of super user and comparison to regular users
## Table

# 1, 18-25 | 2, 25-35 | 3, 35-45 | 4, 45-55 | 5, 55-65 | 6, 65+

labels %>% 
  select(gender_birth) %>% 
  distinct()

us_table <- us_raw %>%
  rename("Age" = age,
         "Gender at Birth" = gender_birth,
         "Gender Expression" = gender_expression,
         "Employment" = employment,
         "Career" = career,
         "Household Income" = household_income,
         "User Type" = superuser) %>%
  mutate(
    `User Type` = case_when(
      `User Type` == TRUE ~ "Super User",
      `User Type` == FALSE ~ "Normal or No-Use"
    ) %>% factor(levels = c("Normal or No-Use", "Super User")),
    
    Age = case_when(
      Age == 1 ~ "18-25",
      Age == 2 ~ "25-35",
      Age == 3 ~ "35-45",
      Age == 4 ~ "45-55",
      Age == 5 ~ "55-65",
      Age == 6 ~ "65+",
      TRUE ~ as.character(Age)  # To handle any other unexpected cases
    ) %>% factor(levels = c("18-25", "25-35", "35-45", "45-55", "55-65", "65+")),
    
    `Gender at Birth` = case_when(
      `Gender at Birth` == 1 ~ "Female",
      `Gender at Birth` == 2 ~ "Male",
      TRUE ~ as.character(`Gender at Birth`)
    ) %>% factor(levels = c("Female", "Male")),
    
    `Gender Expression` = case_when(
      `Gender Expression` == 1 ~ "Female",
      `Gender Expression` == 2 ~ "Male",
      `Gender Expression` == 3 ~ "Nonbinary",
      `Gender Expression` == 4 ~ "Transfeminine",
      `Gender Expression` == 5 ~ "Transmasculine",
      `Gender Expression` == 6 ~ "Prefer not to say",
      `Gender Expression` == 7 ~ "Not listed (enter below)",
      TRUE ~ as.character(`Gender Expression`)
    ) %>% factor(levels = c("Female", "Male", "Nonbinary", "Transfeminine", "Transmasculine", "Prefer not to say", "Not listed (enter below)")),
    
    Employment = case_when(
      Employment == 1 ~ "Full-time employed",
      Employment == 2 ~ "Part-time employed",
      Employment == 3 ~ "Self-employed",
      Employment == 4 ~ "Unemployed",
      Employment == 5 ~ "Student",
      Employment == 6 ~ "Retired",
      Employment == 7 ~ "Homemaker",
      Employment == 8 ~ "Other (enter below)",
      TRUE ~ as.character(Employment)
    ) %>% factor(levels = c("Full-time employed", "Part-time employed", "Self-employed", "Unemployed", "Student", "Retired", "Homemaker", "Other (enter below)")),
    
    Career = case_when(
      Career == 1 ~ "Healthcare and Medical",
      Career == 2 ~ "Education",
      Career == 3 ~ "Business and Finance",
      Career == 4 ~ "Technology and Information Technology (IT)",
      Career == 5 ~ "Engineering",
      Career == 6 ~ "Arts and Entertainment",
      Career == 7 ~ "Government and Public Administration",
      Career == 8 ~ "Science and Research",
      Career == 9 ~ "Legal",
      Career == 10 ~ "Social Services and Non-Profit",
      Career == 11 ~ "Sales and Marketing",
      Career == 12 ~ "Protective Services",
      Career == 13 ~ "Food Preparation and Serving Related Occupations",
      Career == 14 ~ "Construction and Maintenance Services",
      Career == 15 ~ "Personal Care Services",
      Career == 16 ~ "Office and Administrative Support",
      Career == 17 ~ "Farming, Fishing, and Forestry",
      Career == 18 ~ "Transportation and Material Moving",
      Career == 19 ~ "Production",
      Career == 20 ~ "Military",
      Career == 21 ~ "Other (enter below)",
      TRUE ~ as.character(Career)
    ) %>% factor(levels = c("Healthcare and Medical", "Education", "Business and Finance", "Technology and Information Technology (IT)", "Engineering", "Arts and Entertainment", "Government and Public Administration", "Science and Research", "Legal", "Social Services and Non-Profit", "Sales and Marketing", "Protective Services", "Food Preparation and Serving Related Occupations", "Construction and Maintenance Services", "Personal Care Services", "Office and Administrative Support", "Farming, Fishing, and Forestry", "Transportation and Material Moving", "Production", "Military", "Other (enter below)")),
    
    `Household Income` = case_when(
      `Household Income` == 1 ~ "Less than $14,999",
      `Household Income` == 2 ~ "$15,000 - $24,999",
      `Household Income` == 3 ~ "$25,000 - $34,999",
      `Household Income` == 4 ~ "$35,000 - $49,999",
      `Household Income` == 5 ~ "$50,000 - $74,999",
      `Household Income` == 6 ~ "$75,000 - $99,999",
      `Household Income` == 7 ~ "Over $100,000",
      TRUE ~ as.character(`Household Income`)
    ) %>% factor(levels = c("Less than $14,999", "$15,000 - $24,999", "$25,000 - $34,999", "$35,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999", "Over $100,000"))
  )


table_1 <- us_table %>% 
  select(Age, `Gender at Birth`,`Gender Expression`,
         , `Household Income`, `User Type`, Employment) %>% 
  tbl_summary(by = `User Type`) %>% 
  add_overall() %>% 
  add_p() %>% 
  bold_labels() %>%
  modify_header(label = "**Variable**") %>%  # Modify default headers
  modify_spanning_header(
    all_stat_cols() ~ "**Distribution by User Type**"  # Span header over summary columns
  ) %>%
  modify_caption(caption = "Table 1: Demographic Descriptives ")

table_1 %>%
  as_gt() %>%
  gtsave(filename = "C:/Users/steph/Desktop/table_1.png")


## Safety Concerns Graph

us_safety <- us_raw %>% 
  rename("Material Safety" = safety_concerns___1,
         "Hygiene" = safety_concerns___2,
         "Lubricant Compability" = safety_concerns___3,
         "Storage" = safety_concerns___4,
         "Power Source" = safety_concerns___5,
         "User Type" = superuser) %>% 
  mutate(
    `User Type` = case_when(
      `User Type` == TRUE ~ "Super User",
      `User Type` == FALSE ~ "Normal or No-Use"
    )) %>%
  pivot_longer(cols = c(`Material Safety`, Hygiene, `Lubricant Compability`,Storage, `Power Source`),
               names_to = "Safety Concerns",
               values_to = "Count") %>% 
  group_by(`User Type`,`Safety Concerns`) %>% 
  summarise(Count = sum(Count))

## Change colors into paternn 

us_safety %>%
  ggplot(aes(x = `Safety Concerns`, y = Count, pattern = `User Type`)) +  # Use pattern instead of fill
  geom_col_pattern(
    aes(pattern_density = `User Type`),  # Optional: Use density to distinguish patterns
    pattern = "stripe",  # You can choose different pattern types (e.g., stripe, crosshatch, circle, etc.)
    pattern_angle = 45,  # Angle for the pattern (e.g., 45 degrees)
    position = "dodge",
    fill = "white",  # Keep the background fill as white
    color = "black",  # Set the outline color of the bars
    pattern_fill = "black"  # Fill for the pattern
  ) +
  labs(
    title = "Safety Concerns by User Type",
    x = "Safety Concerns",
    y = "Count"
  ) +
  theme_fivethirtyeight() +  # Optional: You can keep or change the theme
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA)
  )


## Sextoys

us_table_satisfaction <- us_table %>% 
  rename("Vibrator" = categorize_sextoys___1,
         "Dildo" = categorize_sextoys___2,
         "Anal Toys" = categorize_sextoys___3,
         "Sleeves" = categorize_sextoys___4,
         "Penis Ring" = categorize_sextoys___5,
         "Toothbrush" = household_select___1,
         "Showerhead" = household_select___2,
         "Washing Machine" = household_select___3,
         "Wand" = household_select___4,
         "Scarves" = household_select___5,
         "Ice Cube" = household_select___6,
         "Produce" = household_select___7,
         "Furniture" = household_select___8,
         "Pillow" = household_select___9) 

us_table_satisfaction_ttoys <- us_table_satisfaction %>% 
  select(`User Type`,Vibrator, Dildo, `Anal Toys`,
         Sleeves,`Penis Ring`,Toothbrush,
         Showerhead,`Washing Machine`,Wand,
         Scarves, `Ice Cube`,Produce,
         Furniture, Pillow,ends_with("satisfaction")) %>% 
  pivot_longer(cols = c(Vibrator, Dildo, `Anal Toys`,
                        Sleeves, `Penis Ring`), names_to = "Traditional Toys", values_to = "n_ttoys") 


us_table_satisfaction_hhtoys <- us_table_satisfaction %>% 
  select(`User Type`,Vibrator, Dildo, `Anal Toys`,
         Sleeves,`Penis Ring`,Toothbrush,
         Showerhead,`Washing Machine`,Wand,
         Scarves, `Ice Cube`,Produce,
         Furniture, Pillow,ends_with("satisfaction")) %>% 
  pivot_longer(cols = c(Toothbrush, Showerhead, `Washing Machine`,
                        Wand, Scarves, `Ice Cube`, Produce, Furniture, Pillow), names_to = "Household Toys", values_to = "n_hhtoys")

ttoys <- us_table_satisfaction_ttoys %>% 
  select(`User Type`, `Traditional Toys`, n_ttoys) %>%
  group_by(`User Type`,`Traditional Toys`) %>% 
  summarise(count = sum(n_ttoys)) 


hh_toys <- us_table_satisfaction_hhtoys %>% 
  select(`User Type`, `Household Toys`, n_hhtoys,
         toothbrush_satisfaction, showerhead_satisfaction, washmachine_satisfaction,
         massagewand_satisfaction, silktie_satisfaction, icecube_satisfaction,
         produce_satisfaction, furniture_satisfaction,pillow_satisfaction) %>% 
  group_by(`User Type`,`Household Toys`) %>% 
  summarise(
    Count = sum(n_hhtoys)) 

hh_satisfaction <- us_table_satisfaction_hhtoys %>% 
  select(`User Type`, 
         toothbrush_satisfaction, showerhead_satisfaction, washmachine_satisfaction,
         massagewand_satisfaction, silktie_satisfaction, icecube_satisfaction,
         produce_satisfaction, furniture_satisfaction, pillow_satisfaction) %>% 
  pivot_longer(cols = ends_with("satisfaction"), names_to = "Household Toys", values_to = "Score") %>% 
  group_by(`User Type`, `Household Toys`) %>% 
  summarise(Average = mean(Score, na.rm = TRUE)) %>% 
  mutate(
    `Household Toys` = case_when(
      `Household Toys` == "furniture_satisfaction" ~ "Furniture",
      `Household Toys` == "icecube_satisfaction" ~ "Ice Cube",
      `Household Toys` == "massagewand_satisfaction" ~ "Wand",
      `Household Toys` == "pillow_satisfaction" ~ "Pillow",
      `Household Toys` == "produce_satisfaction" ~ "Produce",
      `Household Toys` == "showerhead_satisfaction" ~ "Showerhead",
      `Household Toys` == "silktie_satisfaction" ~ "Scarves",
      `Household Toys` == "toothbrush_satisfaction" ~ "Toothbrush",
      `Household Toys` == "washmachine_satisfaction" ~ "Washing Machine",
      TRUE ~ `Household Toys`  # Keep original value if no match is found
    )
  )

household_toys <- left_join(hh_toys, hh_satisfaction, by = c("User Type", "Household Toys"))

summary_table <- household_toys %>%
  gt() %>%
  tab_header(
    title = "Household Toy Usage and Satisfaction by User Type"
  ) %>%
  cols_label(
    `User Type` = "User Type",
    `Household Toys` = "Household Toy",
    Count = "Count (%)",  # Add (%) to the count label
    Average = "Average Satisfaction"
  ) %>%
  fmt_number(
    columns = vars(Average),  # Format Average Satisfaction without decimals
    decimals = 2  # Change this if you want a different number of decimals
  ) %>%
  fmt_percent(
    columns = vars(Count),  # Add percentage formatting to the Count column
    decimals = 1  # Set the decimal points for percentage
  ) %>%
  tab_options(
    table.font.size = "medium"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")  # Bold the header labels
    ),
    locations = cells_column_labels(everything())  # Apply bolding to all column labels
  )


household_toys <- household_toys %>%
  group_by(`User Type`) %>%
  mutate(Percent = (Count / sum(Count)) * 100) %>%
  ungroup() %>%
  mutate(Count_Percent = paste0(Count, " (", round(Percent, 1), "%)"))


# Pivot data to wide format so each User Type has its own column
household_toys_wide <- household_toys %>%
  select(`Household Toys`, `User Type`, Count_Percent, Average) %>%
  pivot_wider(names_from = `User Type`, values_from = c(Count_Percent, Average))

# Check the column names
colnames(household_toys_wide)

# Create the gt summary table using the correct column names
summary_table <- household_toys_wide %>%
  gt() %>%
  tab_header(
    title = md("_Table 3: Household Toy Usage and Satisfaction by User Type_")
  ) %>%
  cols_label(
    `Household Toys` = "Toy Type",
    `Count_Percent_Normal or No-Use` = "Count (Normal or No-Use)",  # Adjust column names based on the output of colnames()
    `Count_Percent_Super User` = "Count (Super User)",
    `Average_Normal or No-Use` = "Avg Satisfaction (Normal or No-Use)",
    `Average_Super User` = "Avg Satisfaction (Super User)"
  ) %>%
  tab_spanner(
    label = "Normal or No-Use",
    columns = vars(`Count_Percent_Normal or No-Use`, `Average_Normal or No-Use`)
  ) %>%
  tab_spanner(
    label = "Super User",
    columns = vars(`Count_Percent_Super User`, `Average_Super User`)
  ) %>%
  fmt_number(
    columns = vars(`Average_Normal or No-Use`, `Average_Super User`),
    decimals = 2  # Set decimal places for satisfaction
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())  # Bold column labels
  ) %>%
  tab_options(
    table.font.size = "medium"
  )%>% 
  tab_style(
    style = list(
      cell_text(size = px(14))  # Bold and decrease title font size
    ),
    locations = cells_title(groups = "title")  # Apply style to title
  ) 

# Print the summary table
summary_table


ttoys <- us_table_satisfaction_ttoys %>%
  select(`User Type`, `Traditional Toys`, n_ttoys) %>%
  group_by(`User Type`, `Traditional Toys`) %>%
  summarise(count = sum(n_ttoys), .groups = "drop") %>%
  group_by(`User Type`) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  ungroup() %>%
  mutate(count_with_percent = paste0(count, " (", round(percentage, 1), "%)"))

# Pivot data to wide format so each User Type has its own column
ttoys_wide <- ttoys %>%
  select(`Traditional Toys`, `User Type`, count_with_percent) %>%
  pivot_wider(names_from = `User Type`, values_from = count_with_percent)

# Create the gt summary table
summary_table <- ttoys_wide %>%
  gt() %>%
  tab_header(
    title = md("_Table 2: Percentages of Traditional Toys by User Type_")
  )  %>%
  cols_label(
    `Traditional Toys` = "Toy Type",  # Label for toy type column
    `Normal or No-Use` = "Normal or No-Use",  # Adjust based on your actual User Type values
    `Super User` = "Super User"  # Adjust based on your actual User Type values
  ) %>%
  tab_spanner(
    label = "Toy Usage",
    columns = vars(`Normal or No-Use`, `Super User`)  # Group the User Type columns under a spanner
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")  # Bold the header labels
    ),
    locations = cells_column_labels(everything())  # Apply bolding to all column labels
  ) %>%
  tab_options(
    table.font.size = "medium"  # Adjust font size
  ) %>% 
  tab_style(
    style = list(
      cell_text(size = px(14))  # Bold and decrease title font size
    ),
    locations = cells_title(groups = "title")  # Apply style to title
  ) 

# Print the summary table
summary_table

