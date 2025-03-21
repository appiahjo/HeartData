# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)

# Load data
data <- read.csv('/Users/osman/Box Sync/miscellaneous/cardiac_test.csv')

# Ensure columns are numeric
data$Black.or.African.American.only <- as.numeric(data$Black.or.African.American.only)
data$Black.or.African.American.only.1 <- as.numeric(data$Black.or.African.American.only.1)

#################
## Line graphs ##
#################
# Prepare the data for plotting
data_long <- data %>%
  pivot_longer(
    cols = c(Black.or.African.American.only, Black.or.African.American.only.1),
    names_to = "Groups",
    values_to = "Rates"
  )

# Create the line graph
ggplot(data_long, aes(x = Characteristic..Prevalence.of.heart.disease., y = Rates, color = Groups)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "Prevalence of Heart Disease",
    y = "% Rates",
    title = "Prevalence of Heart Disease vs. Drug Overdose Death Rates in African Americans",
    color = "Groups"
  ) +
  scale_color_manual(
    values = c("Black.or.African.American.only" = "blue", "Black.or.African.American.only.1" = "red"),
    labels = c("Prevalence of Heart Disease in African Americans", "Drug Overdose Death Rates in African Americans")
  ) +
  theme_minimal() +
  theme(
    legend.position = 'right',
    plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
    axis.text.x = element_text(angle = 90, size = 18, face = 'bold', hjust = 1.0, vjust = 0.5),
    axis.text.y = element_text(size = 18, face = 'bold'),
    axis.title = element_text(size = 18, face = 'bold'),
    legend.text = element_text(size = 18, face = "bold")
  )

ggsave("/Users/osman/Box Sync/miscellaneous/line_graph.pdf", width = 15, height = 12)

######################
## Correlation Test ##
######################
# Calculate Pearson correlation and p-value
cor_test_result <- cor.test(data$Black.or.African.American.only, data$Black.or.African.American.only.1, method = "pearson")
correlation <- cor_test_result$estimate
p_value <- cor_test_result$p.value

# Create the scatter plot with regression line
ggplot(data, aes(x = Black.or.African.American.only, y = Black.or.African.American.only.1)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Heart Disease prevalence in African Americans",
    y = "Drug overdose death rate in African Americans",
    title = "Scatter Plot with Regression Line"
  ) +
  annotate(
    "text", 
    x = min(data$Black.or.African.American.only), 
    y = max(data$Black.or.African.American.only.1), 
    label = paste0(
      "Pearson r = ", round(correlation, 2), "\n",
      "p-value = ", format.pval(p_value, digits = 2)
    ),
    hjust = 0, vjust = 1, size = 5, color = "red"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
    axis.text.x = element_text(size = 18, face = 'bold'),
    axis.text.y = element_text(size = 18, face = 'bold'),
    axis.title = element_text(size = 18, face = 'bold')
  )

ggsave("/Users/osman/Box Sync/miscellaneous/scatter_plot.pdf", width = 15, height = 12)
