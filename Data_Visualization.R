library(tidyr)
library(ggplot2)
library(graphics)
library(tibble)
library(rgl)
library(dplyr)

df <- read.csv('Cleaned_Jobs_NYC_Postings_ .csv')

#displays first few rows
head(df)

#converting the same to tibble
df1 <- as_tibble(df)
df1

#displaying the discription of the dataset
str(df1)

#relationship of all the job postings over the years of 2021-2023
ggplot(df1, aes(factor(Job.Postings.Year), Total.Postings)) +
  geom_bar(stat = "summary", fun = "sum", fill = "skyblue", alpha = 0.7) +  # Adjust alpha for transparency
  labs(x = "Job Postings Year", y = "Total Postings") +
  ggtitle("Relationship between Job Postings Year and Total Postings") +
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5),  # Center align the plot title
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )

ggplot(df, aes(x = Job.Category, y = Salary.Range.To)) +
  geom_boxplot() +
  labs(x = "Job Category", y = "Salary Range To", title = "Salary Range Distribution by Job Category")

#Relationship between the job categories and the salary range extent over the years:
ggplot(df, aes(x = Job.Category, y = Salary.Range.To, fill = Job.Category)) +
  geom_boxplot(alpha = 0.8) +  # Adjust transparency
  labs(x = "Job Category", y = "Salary Range To", title = "Salary Range Distribution by Job Category", fill = "Job Category") +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center-aligned title
    axis.text.x = element_text(size = 8),  # Reduce font size for x-axis
    legend.position = "right"  # Position the legend
  )

#Total number of job postings over the years based on the career level
df_filtered <- df[!is.na(df$Total.Postings), ]

ggplot(df_filtered, aes(x = Job.Postings.Year, y = Total.Postings, fill = Career.Level)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +  # Adjust transparency
  labs(x = "Job Postings Year", y = "Total Postings", title = "Job Postings over the Years Based on Career Level", fill = "Career Level") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center-aligned title
    legend.position = "right"  # Position the legend
  )

#Relationship between the recidency requirement aover the years depicting the change in lifestyle
df_filtered <- df[!is.na(df$Residency.Requirement) & !is.na(df$Job.Postings.Year), ]
# Calculate the proportion of jobs requiring residency each year
residency_trend <- df_filtered %>%
  group_by(Job.Postings.Year, Residency.Requirement) %>%
  summarize(Count = n()) %>%
  group_by(Job.Postings.Year) %>%
  mutate(Proportion = Count / sum(Count))  # Calculating proportion manually

ggplot(residency_trend, aes(x = Job.Postings.Year, y = Proportion, group = Residency.Requirement, color = Residency.Requirement)) +
  geom_line(size = 1.2) +  # Increase line thickness
  labs(x = "Job Postings Year", y = "Proportion", title = "Proportion of Jobs Requiring Residency over the Years") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center-aligned title
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "right"  # Position the legend
  )


df$Job.Postings.Year <- as.character(df$Job.Postings.Year)
df1$Job.Postings.Year <- as.character(df1$Job.Postings.Year)

#dipicting the relationship of Salary to on other numerical values present in the dataset
numeric_df <- df[sapply(df, is.numeric)]

par(mfrow = c(ncol(numeric_df), ncol(numeric_df)))  # Set up the layout of plots

for (i in 1:ncol(numeric_df)) {
  for (j in 1:ncol(numeric_df)) {
    if (i != j) {
      plot(numeric_df[, i], numeric_df[, j], 
           xlab = colnames(numeric_df)[i], 
           ylab = colnames(numeric_df)[j],
           main = paste("Scatterplot:", colnames(numeric_df)[i], "vs", colnames(numeric_df)[j]))
    }
  }
}

