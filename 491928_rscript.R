# Programming Asessment
# Candidate number 491928
install.packages("ggplot2")
install.packages("rstudioapi")
library(rstudioapi)
library(ggplot2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read in data from the two files
births_df <- read.csv("files/births.csv")
score_df <- read.csv("files/score.csv")

# Examine the births dataset
head(births_df)

# Transform variables to factors with labels
births_df$lowbw <- factor(births_df$lowbw,
  levels = c(0, 1),
  labels = c("Normal birthweight", "Low birthweight")
)
births_df$preterm <- factor(births_df$preterm,
  levels = c(0, 1),
  labels = c("Not preterm", "Preterm")
)
births_df$hyp <- factor(births_df$hyp,
  levels = c(0, 1),
  labels = c("No maternal hypertension", "Maternal hypertension")
)
births_df$sex <- factor(births_df$sex,
  levels = c(1, 2),
  labels = c("Male", "Female")
)

# Display the first few rows of the data frame to check the changes.
# Check the transformation into factors as expected.
head(births_df)
str(births_df)

# Cleaning the dataset by omitting rows with missing observations.
births_df_clean <- na.omit(births_df)
score_df_clean <- na.omit(score_df)

# Combine the datasets keeping both matching and unmatching records.
merged_df <- merge(births_df_clean, score_df_clean, by = "id", all = TRUE)
str(merged_df)
# This combined dataframe as 490 rows. The result is a table
# with the births dataset and variable "score" assigned to the row 
# with the same "id" number. Note that not all rows have a "score".

# Reorder the merged dataset by the "score" variable.
sorted_merged_df <- merged_df[order(merged_df$score), ]


# Name the continuous variables
continuous_variables <- merged_df[, c("bweight", "matage", "gestwks", "score")]

# Compute correlation matrix, using the argument 'use' to handle missing values
correlation_matrix <- cor(continuous_variables, use = "pairwise.complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Examine two way distribution of categorical variables as a table.
# Function to print the crosstab and its title
print_crosstab <- function(data, row_var, col_var, title) {
  cat(title, ":\n")
  print(table(data[[row_var]], data[[col_var]]))
  cat("\n") # for spacing
}

# Now call this function for each pair of variables
print_crosstab(merged_df, "sex", "hyp", "Sex and Hypertension distribution")
print_crosstab(merged_df, "sex", "preterm", "Sex and Preterm distribution")
print_crosstab(merged_df, "preterm", "hyp", "Preterm and Hypertension 
               distribution")
print_crosstab(merged_df, "sex", "lowbw", "Low birthweight and Sex 
               distribution")
print_crosstab(merged_df, "lowbw", "hyp", "Low birthweight and Hypertension 
               distribution")
print_crosstab(merged_df, "lowbw", "preterm", "Low birthweight and Preterm 
               distribution")


# Create a new variable high score that identifies scores greater than 150.


merged_df$highscore <- with(merged_df, ifelse(is.na(score), "No Score",
  ifelse(score > 150, "Yes", "No")
))

# Convert the highscore variable to a factor
merged_df$highscore <- factor(merged_df$highscore, levels = c("No", "Yes", "No 
                                                              Score"))

aggregate(bweight ~ highscore + sex, data = merged_df, FUN = mean)


# Visualize the distribution of birthweight and gestational weeks in a plot
gestwks_v_bweight <- ggplot(merged_df, aes(x = gestwks, y = bweight)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Gestational age (weeks)",
    y = "Birthweight (grams)",
    title = "Relationship Between Gestational Weeks and Birthweight"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
print(gestwks_v_bweight)

# Now you can save the plot using ggsave()
ggsave("2301367_bweight_vs_gestwks_R.pdf", plot = gestwks_v_bweight, device = 
         "pdf", width = 7, height = 7)


# Save final dataframe
write.csv(merged_df, file = "2301367_final_dataset_R.csv", row.names = FALSE)
