# Load necessary libraries
library(tidyverse)
library(haven)
library(patchwork)

#+ This is what I want:
#+ A figure with two line graphs. One line graph shows Black Registration across years in two groups. Group one "Control" and group two "Treated".
#+ The group "Treated" are those that have 1 in column "Understanding Clause" and group "Control" have 0 in column "Understanding Clause". 
#+ I want the graph to show registration from 0 to 0.6. The years covered are 1950 to 1970. 
#+ Figure should have title: FIGURE2. Proportion of..
#+ Put two graphs in same figure. 

# Load dataset
data <- read_dta("la_turnout_basic.dta")

# Understanding the data ----
# Print all column names
colnames(data)

# Currently data listed by parish number by year. I will need to transform to treatment group by year. 

# My columns of interest are year, understandingclause2, whitegrate, and blackgrate

# Checking for missing values 
# Select the columns of interest
cols_of_interest <- data %>% select(year, understandingclause2, whiteregrate, blackregrate)

# Check for missing values in each column
missing_summary <- cols_of_interest %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "Missing_{.col}"))

# Print the summary of missing values
print(missing_summary)

# Preparing the data ----

# Filter Data for Years 1950-1970
data_und <- data %>%
  filter(year >= 1950 & year <= 1970)

# Handling missing values
data_und <- data_und %>%
  group_by(parishnumber) %>%  # Group by Parish to apply imputation per parish
  mutate(
    # Interpolation for missing values (Linear)
    brrate = ifelse(is.na(blackregrate), approx(year, blackregrate, year, rule = 2)$y, blackregrate),
    wrrate = ifelse(is.na(whiteregrate), approx(year, whiteregrate, year, rule = 2)$y, whiteregrate)
  ) %>%
  ungroup()  # Ungroup for further processing

# Check for missing values again
# Check for missing values in each column
missing_summary <- data_und %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "Missing_{.col}"))

# Print the summary
print(missing_summary)

## From here on, I will work with year, understandingclause2, brrate, wrrate

# Transform the Data ----
#+ New columns: Year, Treatment Group, Black Registration Rate, White Registration Rate
#+ Years: 1950 to 1970 from year column, 
#+ Treatment Group = "Treated" if understandingclause2 column = 1 and = "Control" if understandingclause2 column = 0.
#+ Black Registration Rate is the average of column brrate of all observations for each year for each group Treated or Control
#+ White Registration Rate is the average of column wrrate of all observations for each year for each group Treated or Control

# Print all column names to name properly in code
colnames(data_und)

# Apply data transformations
transformed_data <- data_und %>%
  filter(year >= 1950 & year <= 1970) %>% 
  mutate(
    Treatment_Group = case_when(
      understandingclause2 == 1 ~ "Treated",
      understandingclause2 == 0 ~ "Control",
      TRUE ~ NA_character_  
    )
  ) %>%
  group_by(year, Treatment_Group) %>%
  summarise(
    Black_Registration_Rate = mean(brrate, na.rm = TRUE),  
    White_Registration_Rate = mean(wrrate, na.rm = TRUE),  
    .groups = "drop"  
  )

# View Transformed Data
print(transformed_data)

# Replication of Figure #2 ----

# Black Registration Rate Plot
plot_black <- ggplot(transformed_data, aes(x = year, y = Black_Registration_Rate, group = Treatment_Group)) +
  geom_line(aes(colour = Treatment_Group), linewidth = 0.6) +  # Thinner lines
  geom_point(aes(shape = Treatment_Group), size = 2.5, fill = "black") +  
  scale_color_manual(values = c("Control" = "#4B0082", "Treated" = "yellow")) +  
  scale_shape_manual(values = c("Control" = 16, "Treated" = 17)) +  
  labs(
    x = "Year",
    y = "Black Registration Rate",
    caption = "(a) Black Registration"
  ) +
  theme_bw() +  
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),  # Subtle border
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 14, family = "Times New Roman", hjust = 0.5, margin = margin(t = 5))
  )

# White Registration Rate Plot
plot_white <- ggplot(transformed_data, aes(x = year, y = White_Registration_Rate, group = Treatment_Group)) +
  geom_line(aes(colour = Treatment_Group), linewidth = 0.6) +  # Thinner lines
  geom_point(aes(shape = Treatment_Group), size = 2.5, fill = "black") +  
  scale_color_manual(values = c("Control" = "#4B0082", "Treated" = "yellow")) +  
  scale_shape_manual(values = c("Control" = 16, "Treated" = 17)) +  
  labs(
    x = "Year",
    y = "White Registration Rate",
    caption = "(b) White Registration"
  ) +
  theme_bw() +  
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),  # Subtle border
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.caption = element_text(size = 14, family = "Times New Roman", hjust = 0.5, margin = margin(t = 5))
  )

# Combine Plots to Match Figure 2 from Chapter
combined_plot <- (plot_black | plot_white) +  
  plot_annotation(
    title = "FIGURE 2.  Proportion of Registered Voters by Race and by Understanding Clause Status. Treated Parishes Enforced \nthe Understanding Clause and Control Parishes Did Not",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0, margin = margin(b = 8))  
    )
  )

# Print the final combined plot
print(combined_plot)

# Improving Figure #2 ----

# Pivot data for faceting (Reshape from wide to long format)
transformed_data_long <- transformed_data %>%
  pivot_longer(cols = c(Black_Registration_Rate, White_Registration_Rate),
               names_to = "Race",
               values_to = "Registration_Rate") %>%
  mutate(Race = recode(Race, 
                       "Black_Registration_Rate" = "Black Registration",
                       "White_Registration_Rate" = "White Registration"))

# Create the plot with faceting
figure_2 <- ggplot(transformed_data_long, aes(x = year, y = Registration_Rate, 
                                              group = Treatment_Group, color = Treatment_Group, shape = Treatment_Group)) +
  
  # Add a shaded area for Understanding Clause enforcement (1954-1965)
  annotate("rect", xmin = 1954, xmax = 1965, ymin = 0, ymax = 1, fill = "gray85", alpha = 0.3) +
  
  # Line plot (kept the same)
  geom_line(linewidth = 1.2) +  
  
  # Points: TRUE hollow shapes with thin black border
  geom_point(size = 3.5, stroke = 0.6, fill = "white", color = "black") +  
  
  # Custom colors and shapes (Now using 21 & 24 for hollow effect)
  scale_color_manual(values = c("Control" = "#4B0082", "Treated" = "yellow")) +  
  scale_shape_manual(values = c("Control" = 21, "Treated" = 24)) +  
  
  # Faceting to create two subplots
  facet_wrap(~Race) +
  
  # Adjust axes
  scale_x_continuous(breaks = seq(1950, 1970, by = 5), limits = c(1950, 1970)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
  
  # Labels and Title
  labs(
    x = "Year",
    y = "Registration Rate",
    title = "FIGURE 2. Proportion of Registered Voters by Race and by Understanding Clause Status",
    caption = "Shaded area represents the period (1954-1965) where the Understanding Clause was in effect."
  ) +
  
  # Theme adjustments
  theme_classic() +  # Removes gridlines
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.caption = element_text(size = 11, hjust = 0.5, face = "italic")
  )

# Print the improved figure
print(figure_2)