# GolfStatR

rm(list=ls())

library(golfastr)
library(tidyverse)
library(cluster) 
library(factoextra)

# 1. Get the 2025 schedule to identify Major tournament IDs/Names
schedule_2025 <- load_schedule(2025)

# 2. Build the 2025 Season Database
# This will save a file called 'pga_2025_full.rds' in your working directory.
# Note: This step may take a few minutes as it pulls every hole for every player.
build_season(2025, file_path = "pga_2025_full.rds")

# 3. Load the completed dataset
tour_data_raw <- load_from_rds("pga_2025_full.rds")

colnames(tour_data_raw)

head(tour_data_raw)

# Cleaning and Filtering

# Define the events we want to keep
excluded_events <- c("Q-School", "Zurich Classic")

# 1. Get the list of all tournament names for 2025
tournaments_2025 <- load_schedule(2025) %>%
  filter(!grepl("Q-School|Zurich Classic", tournament_name, ignore.case = TRUE))

# 2. Pull hole-by-hole data (this is the "Big Data" step)
# We'll map through the tournament list to get the scorecards
tour_holes_raw <- map_df(tournaments_2025$tournament_name, function(t_name) {
  message(paste("Loading:", t_name))
  load_holes(2025, t_name)
})

tour_holes_raw <- tour_holes_raw %>%
  filter(!grepl("Ryder Cup", tournament_name, ignore.case = TRUE))

# Filter out the Barracuda and re-run the DNA profile
tour_holes_raw <- tour_holes_raw %>%
  filter(!grepl("Barracuda", tournament_name, ignore.case = TRUE))

# Re-run your DNA profile and Clustering code from here
# (This will move all that 'Major DNA' into much clearer clusters!)

# 3. Quick check of columns to ensure 'hole' and 'par' are there
colnames(tour_holes_raw)

# 1. Create the DNA Profile using the correct holes dataframe
tour_dna_profile <- tour_holes_raw %>%
  # Flag the Majors
  mutate(is_major = grepl("Masters|PGA Championship|U.S. Open|The Open", tournament_name, ignore.case = TRUE)) %>%
  # Group by the specific hole characteristics
  group_by(tournament_name, hole, par, is_major) %>%
  summarise(
    # 'Field Difficulty Relative to Par' (FDRP)
    avg_to_par = mean(score - par, na.rm = TRUE),
    # Score Volatility (The 'Risk' metric)
    score_sd = sd(score, na.rm = TRUE),        
    birdie_rate = mean(score < par, na.rm = TRUE),
    bogey_rate = mean(score > par, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Clean up any holes with no variance (shouldn't be any, but good practice)
  filter(!is.na(score_sd))

# 2. Check the result
head(tour_dna_profile)

# 1. Scale the data (essential for K-means)
dna_matrix <- tour_dna_profile %>%
  select(avg_to_par, score_sd, birdie_rate, bogey_rate) %>%
  scale()

# 2. Run K-Means - let's go with 4 clusters to find the 'Outliers'
set.seed(123)
kmeans_results <- kmeans(dna_matrix, centers = 4, nstart = 25)

# 3. Add clusters back to the main profile
tour_dna_profile$cluster <- as.factor(kmeans_results$cluster)

# See how Major holes are distributed across the 4 clusters
table(tour_dna_profile$cluster, tour_dna_profile$is_major)

ggplot(tour_dna_profile, aes(x = avg_to_par, y = score_sd)) +
  # Plot all tour holes in a light grey "ghost" layer
  geom_point(data = filter(tour_dna_profile, is_major == FALSE), 
             color = "grey80", alpha = 0.4, size = 1.5) +
  # Layer the Majors on top in high-contrast colours
  geom_point(data = filter(tour_dna_profile, is_major == TRUE), 
             aes(color = cluster), size = 3, alpha = 0.9) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "The 'Major' DNA: 2025 PGA Tour Hole Analysis",
    subtitle = "Highlighting Major Championship holes against the standard Tour backdrop.",
    x = "Field Relative to Par (FRP)",
    y = "Score Volatility (SD)",
    color = "DNA Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

cluster_profiles <- tour_dna_profile %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    majors_in_cluster = sum(is_major),
    avg_difficulty = mean(avg_to_par),
    avg_volatility = mean(score_sd),
    avg_birdie_rate = mean(birdie_rate),
    avg_bogey_rate = mean(bogey_rate)
  ) %>%
  arrange(desc(avg_difficulty))

print(cluster_profiles)

# 1. Real Hardest Holes (Cluster 4)
real_hard_holes <- tour_dna_profile %>%
  filter(cluster == 4) %>%
  arrange(desc(avg_to_par)) %>%
  slice_head(n = 5) %>%
  select(tournament_name, hole, par, avg_to_par, score_sd, is_major)

# 2. Real Easiest Holes (Cluster 1)
real_easy_holes <- tour_dna_profile %>%
  filter(cluster == 1) %>%
  arrange(avg_to_par) %>%
  slice_head(n = 5) %>%
  select(tournament_name, hole, par, avg_to_par, score_sd, is_major)

print("Actual 'Major DNA' Hardest Holes:")
print(real_hard_holes)

# A. Top 5 Hardest & Easiest Holes (Absolute)
top_5_hardest <- tour_dna_profile %>% arrange(desc(avg_to_par)) %>% head(5)
top_5_easiest <- tour_dna_profile %>% arrange(avg_to_par) %>% head(5)

# B. Top 5 Hardest & Easiest Tournaments (The 'Course DNA' Rank)
tournament_rankings <- tour_dna_profile %>%
  group_by(tournament_name, is_major) %>%
  summarise(
    course_difficulty = sum(avg_to_par), # Total strokes over/under par for 18 holes
    avg_hole_volatility = mean(score_sd),
    .groups = "drop"
  ) %>%
  arrange(desc(course_difficulty))

print("Hardest Tournaments of 2025:")
head(tournament_rankings, 5)

print("Easiest Tournaments of 2025:")
tail(tournament_rankings, 5)

# Create the 'Ultimate Nightmare' Course
nightmare_course <- tour_dna_profile %>%
  group_by(hole) %>%
  filter(avg_to_par == max(avg_to_par)) %>%
  slice(1) %>% # In case of ties
  ungroup()

# Create the 'Sunday Dream' Course
dream_course <- tour_dna_profile %>%
  group_by(hole) %>%
  filter(avg_to_par == min(avg_to_par)) %>%
  slice(1) %>%
  ungroup()

# Calculate the total Par for our custom courses
nightmare_par <- sum(nightmare_course$par)
dream_par <- sum(dream_course$par)

# Calculate the actual expected score (Par + the sum of relative difficulty)
true_nightmare_score <- nightmare_par + sum(nightmare_course$avg_to_par)
true_dream_score <- dream_par + sum(dream_course$avg_to_par)

print(paste("Nightmare Course Par:", nightmare_par, "| Expected Score:", round(true_nightmare_score, 1)))
print(paste("Dream Course Par:", dream_par, "| Expected Score:", round(true_dream_score, 1)))

ggplot(tour_dna_profile, aes(x = is_major, y = avg_to_par, fill = is_major)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = is_major), width = 0.2, alpha = 0.4) +
  scale_fill_manual(values = c("grey70", "#D4AF37"), labels = c("Standard Tour", "Majors")) +
  scale_color_manual(values = c("grey40", "#8B6B00"), labels = c("Standard Tour", "Majors")) +
  labs(
    title = "The Difficulty Gap: Majors vs. Standard PGA Tour",
    subtitle = "Distribution of Hole Difficulty (Field Relative to Par)",
    x = "Tournament Type",
    y = "Difficulty (Avg Strokes to Par)",
    fill = "Is it a Major?",
    color = "Is it a Major?"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

library(gt)
library(gtExtras)
library(gt)
library(gtExtras)
library(dplyr)

# Prepare the Nightmare Data with a Total Row
nightmare_display <- nightmare_course %>%
  select(hole, tournament_name, par, avg_to_par, bogey_rate) %>%
  mutate(tournament_name = ifelse(grepl("PGA Championship", tournament_name), 
                                  paste0(tournament_name, " (Quail Hollow)"), 
                                  tournament_name))

# Nightmare Scorecard with Totals and Averages
nightmare_scorecard <- nightmare_display %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_percent(columns = bogey_rate, decimals = 0) %>%
  fmt_number(columns = avg_to_par, decimals = 2, force_sign = TRUE) %>%
  cols_label(
    hole = "Hole",
    tournament_name = "Real World Venue",
    par = "Par",
    avg_to_par = "Strokes Over Par",
    bogey_rate = "Bogey %"
  ) %>%
  gt_color_rows(avg_to_par, palette = "Reds") %>%
  # Row for Sums (Total Par and Total Expected Score)
  grand_summary_rows(
    columns = c(par, avg_to_par),
    fns = list(`COURSE TOTAL` = ~sum(.)),
    formatter = fmt_number, decimals = 1
  ) %>%
  # Row for Means (Avg Par per hole, Avg Difficulty, Avg Bogey Rate)
  grand_summary_rows(
    columns = c(par, avg_to_par, bogey_rate),
    fns = list(`FIELD AVERAGE` = ~mean(.)),
    formatter = fmt_number, decimals = 2
  ) %>%
  # Fix the bogey rate average specifically as a percentage
  fmt_percent(columns = bogey_rate, rows = everything(), decimals = 0) %>%
  tab_header(
    title = "The 18 Hardest Individual Holes on the 2025 PGA Tour",
  ) %>%
  tab_source_note(source_note = "Expected Score: 76.0 | Data Credit: golfastr")

nightmare_scorecard

# Dream Scorecard with Totals and Averages
dream_scorecard <- dream_course %>%
  select(hole, tournament_name, par, avg_to_par, birdie_rate) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_percent(columns = birdie_rate, decimals = 0) %>%
  fmt_number(columns = avg_to_par, decimals = 2, force_sign = TRUE) %>%
  cols_label(
    hole = "Hole",
    tournament_name = "Real World Venue",
    par = "Par",
    avg_to_par = "Strokes Under Par",
    birdie_rate = "Birdie %"
  ) %>%
  gt_color_rows(avg_to_par, palette = "Blues") %>%
  # Row for Sums (Total Par and Total Expected Score)
  grand_summary_rows(
    columns = c(par, avg_to_par),
    fns = list(`COURSE TOTAL` = ~sum(.)),
    formatter = fmt_number, decimals = 1
  ) %>%
  # Row for Means (Avg Par per hole, Avg Difficulty, Avg Birdie Rate)
  grand_summary_rows(
    columns = c(par, avg_to_par, birdie_rate),
    fns = list(`FIELD AVERAGE` = ~mean(.)),
    formatter = fmt_number, decimals = 2
  ) %>%
  # Fix the birdie rate average specifically as a percentage
  fmt_percent(columns = birdie_rate, rows = everything(), decimals = 0) %>%
  tab_header(
    title = "The 18 Easiest Individual Holes on the 2025 PGA Tour",
  ) %>%
  tab_source_note(source_note = "Expected Score: 76.7 | Data Credit: golfastr")

dream_scorecard

install.packages("webshot2")
library(webshot2)

# Save the Nightmare Table
nightmare_scorecard %>%
  gtsave(
    filename = "hardest_scorecard.png",
    expand = 5,   # This removes the excess white space
    vwidth = 800, 
    vheight = 1000
  )

# Save the Dream Table
dream_scorecard %>%
  gtsave(
    filename = "easy_scorecard.png",
    expand = 5,
    vwidth = 800,
    vheight = 1000
  )

nightmare_scorecard %>%
  gtsave(
    filename = "hard_scorecard.png",
    vwidth = 900,   # Set these slightly wider than the table
    vheight = 1100,
    expand = 0      # This keeps the frame size fixed
  )

dream_scorecard %>%
  gtsave(
    filename = "easy_scorecard.png",
    vwidth = 900,   # Set these slightly wider than the table
    vheight = 1100,
    expand = 0      # This keeps the frame size fixed
  )
