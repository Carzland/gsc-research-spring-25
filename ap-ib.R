library(readxl)
library(tidyverse)

options(tibble.width = Inf)

# Base path
base_path <- "/Users/carzland/Library/CloudStorage/OneDrive-YaleUniversity/NYSED"

years <- 2019:2024

course_list <- list()

# Make sure the output directory exists
output_dir <- "ap-ib-output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Read in data
for (year in years) {
  curr_year_suffix <- substr(as.character(year), 3, 4)
  folder_name <- paste0(year - 1, "-", curr_year_suffix)
  subfolder_name <- paste0("APIB", substr(as.character(year), 3, 4))
  file_name <- paste0("AP_IB_Course_", year, ".xlsx")
  
  full_path <- file.path(base_path, folder_name, subfolder_name, file_name)
  
  if (file.exists(full_path)) {
    course_list[[as.character(year)]] <- read_excel(full_path)
    cat("Loaded:", full_path, "\n")
  } else {
    cat("Missing file:", full_path, "\n")
  }
}

# Initialize list to store aggregated results by year
course_list_agg <- list()

# Aggregate everything except 2024, which is already aggregated
for (year in head(names(course_list), -1)) {
  df <- course_list[[year]]
  names(df) <- tolower(names(df))
  
  # Make sure necessary columns exist and are the correct types
  df <- df %>%
    mutate(
      report_school_year = as.character(report_school_year),
      student_gender = as.character(student_gender),
      ethnic_desc_rc = as.character(ethnic_desc_rc),
      course_id = as.numeric(course_id),
      student_count = as.numeric(student_count)
    )
  
  # Filter out rows with missing values in grouping fields or student count
  df_filtered <- df %>%
    filter(!is.na(student_gender),
           !is.na(ethnic_desc_rc),
           !is.na(course_id),
           !is.na(student_count))

  # Perform aggregation
  aggregated_df <- df_filtered %>%
    group_by(report_school_year, student_gender, ethnic_desc_rc, course_id) %>%
    summarise(
      student_count = sum(student_count, na.rm = TRUE),
      apib_ind = first(apib_ind),
      subject_area = first(subject_area),
      course_desc = first(course_desc),
      .groups = "drop"
    )
  
  # Store result in list by year
  course_list_agg[[year]] <- aggregated_df
}

# Go from a list of dataframes to a single dataframe
course_df_agg <- bind_rows(course_list_agg)

print(head(course_df_agg))

# Racial Breakdown
course_df_agg_racial <- course_df_agg %>%
  group_by(report_school_year, ethnic_desc_rc) %>%
  summarise(total_students = sum(student_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(report_school_year) %>%
  mutate(
    percent = total_students / sum(total_students),
    label = scales::percent(percent, accuracy = 1)
  )

ethnicity = ggplot(course_df_agg_racial, aes(x = report_school_year, y = total_students, fill = ethnic_desc_rc)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  labs(title = "AP/IB Course Enrollmment by Ethnic Group",
       x = "School Year",
       y = "Number of Students",
       fill = "Ethnicity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "ethnicity_breakdown.png"), plot = ethnicity, width = 10, height = 6, dpi = 300)

# Gender Breakdown
course_df_agg_gender <- course_df_agg %>%
  group_by(report_school_year, student_gender) %>%
  summarise(total_students = sum(student_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(report_school_year) %>%
  mutate(
    percent = total_students / sum(total_students),
    label = scales::percent(percent, accuracy = 1)
  )

gender = ggplot(course_df_agg_gender, aes(x = report_school_year, y = total_students, fill = student_gender)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  labs(title = "AP/IB Course Enrollmment by Gender",
       x = "School Year",
       y = "Number of Students",
       fill = "Gender") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "gender_breakdown.png"), plot = gender, width = 10, height = 6, dpi = 300)

# Course Breakdown
course_df_agg_course <- course_df_agg %>%
  group_by(report_school_year, subject_area) %>%
  summarise(total_students = sum(student_count, na.rm = TRUE), .groups = "drop") %>%
  group_by(report_school_year) %>%
  mutate(
    percent = total_students / sum(total_students),
    label = scales::percent(percent, accuracy = 1)
  )

course = ggplot(course_df_agg_course, aes(x = report_school_year, y = total_students, fill = subject_area)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  labs(title = "AP/IB Course Enrollmment by Type",
       x = "School Year",
       y = "Number of Students",
       fill = "Course Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "course_breakdown.png"), plot = course, width = 10, height = 6, dpi = 300)
