# ============================================================
# Project Title : Superstore Sales Analysis
# Author        : Mohammed Elmojtaba
# Description   : Data cleaning, analysis, and visualization
#                 of Superstore sales data using R
# Tools         : R, tidyverse, ggplot2, Power BI
# Created on    : 2026-01-10
# ============================================================


# ------------------------------------------------------------
# Required Packages
# ------------------------------------------------------------

# tidyverse:
# Used for data manipulation, cleaning, and visualization.
# Includes dplyr, ggplot2, readr, and other core packages.
#
# lubridate:
# Used to handle and manipulate date variables (year, month).
#
# janitor:
# Used to clean column names and improve data consistency.

# ------------------------------------------------------------
# NOTE:
# Install required packages only once if not already installed
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("janitor")
# ------------------------------------------------------------


# ------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------
# The following libraries are loaded to make their functions
# available for use throughout this analysis script.


# Load required libraries
 library(tidyverse)  # Core package for data manipulation and visualization
 library(lubridate)  # Used for working with date variables
 library(janitor)    # Used for cleaning and standardizing column names

# ============================================================
# Step 1: Load Dataset
# ============================================================

 superstore <- read_csv("Sample - Superstore.csv")

# read_csv():
# Used to read CSV files efficiently.
# Automatically detects column types and is faster
# than the base R function read.csv().


# ============================================================
# Step 2: Initial Data Inspection
# ============================================================

 glimpse(superstore)

# glimpse():
# Provides a quick overview of the dataset structure,
# including column names, data types, and sample values.


# ============================================================
# Step 3: Clean Column Names
# ============================================================

 superstore <- superstore %>%
clean_names()

# clean_names():
# Converts column names to a consistent format
# (lowercase with underscores), making them easier
# to reference in analysis and visualization.


# ============================================================
# Step 4: Convert Date Columns
# ============================================================

 superstore <- superstore %>%
  mutate(
    order_date = mdy(order_date),
    ship_date  = mdy(ship_date)
  )


# mutate():
# Used to create or modify columns in the dataset.
#
# mdy():
# Converts character dates in month-day-year format
# into proper Date objects for time-based analysis.


# ============================================================
# Step 5: Creating time variables
# ============================================================

 superstore <- superstore %>%
 mutate(order_year = year(order_date),
        order_month_num = month(order_date),
        order_month = month(order_date, label = TRUE,
        locale = "en")
     )

# Create time-based variables (year and month)
# Month names are forced to English to avoid locale issues.
 
 
 # ------------------------------------------------------------
 # Check structure after feature engineering
 # ------------------------------------------------------------
 
 str(superstore)
 
 # str():
 # Displays the structure of the dataset, allowing quick
 # verification of column types and ensuring that newly
 # created time variables have the correct data types.
 

# ============================================================
# Step 6: Checking for missing values
# ============================================================

 colSums(is.na(superstore))

# is.na():
# Identifies missing values in the dataset.
#
# colSums():
# Counts the number of missing values per column,
# helping assess data quality.


# ============================================================
# Step 7: First aggregate analysis
# ============================================================

 superstore %>%
  group_by(category) %>%
  summarise(
    total_sales  = sum(sales),
    total_profit = sum(profit)
  ) %>%
  arrange(desc(total_sales))

# group_by():
# Groups data by category to enable comparative analysis.
#
# arrange(desc()):
# Sorts the results in descending order to highlight
# top-performing categories.


# ============================================================
# Step X: Profit and Loss by Sub-Category
# ============================================================


 profit_loss_plot <- superstore %>%
  group_by(sub_category) %>%
  summarise(total_profit = sum(profit), .groups = "drop") %>%
  ggplot(aes(
    x = reorder(sub_category, total_profit),
    y = total_profit,
    fill = total_profit > 0
  )) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Profit and Loss by Sub-Category",
    x = "Sub-Category",
    y = "Total Profit"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "darkgreen", "FALSE" = "firebrick"),
    labels = c("Profit", "Loss")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())


# group_by(sub_category):
# Aggregates profit at the sub-category level.

# total_profit > 0:
# Creates a logical condition to distinguish
# profitable products from loss-making ones.

# reorder():
# Sorts sub-categories based on profit values
# to clearly show losses versus profits.

# coord_flip():
# Improves readability when many categories exist.


# ============================================================
# Viewing the image :

 profit_loss_plot

# ============================================================

# Save image inside folder :

 plots

# ============================================================

 ggsave(
  "plots/profit_loss_by_subcategory.png",
  plot = profit_loss_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# Save the profit and loss visualization as a high-resolution image.

# ============================================================
 
# Save cleaning file for Power BI:
 
  write_csv(superstore,"superstore_clean.csv")
 
# ============================================================
 
# save script file in RStudio :
  
  analysis.R
  
# use (Ctrl + S) to save
  
  or
  
# From the menu at the top left, select File and then select Save as.