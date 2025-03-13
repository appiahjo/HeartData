# Joel's cardiac project data

# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(tidyr)
#Load data
data <- read.csv('/Users/osman/Box Sync/miscellaneous/cardiac_test.csv')

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
  geom_line(size = 1) +  # Add lines
  geom_point(size = 2) +  # Add points for clarity
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
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) + 
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  theme(legend.position = "bottom")+
  theme_bw(base_size = 24) +
  theme(
    legend.position = 'right',
    legend.background = element_rect(),
    plot.title = element_text(angle = 0, size = 18, face = 'bold', vjust = 1),
    plot.subtitle = element_text(angle = 0, size = 14, face = 'bold', vjust = 1),
    plot.caption = element_text(angle = 0, size = 14, face = 'bold', vjust = 1),
    
    axis.text.x = element_text(angle = 90, size = 18, face = 'bold', hjust = 1.0, vjust = 0.5, colour = "black"),
    axis.text.y = element_text(angle = 0, size = 18, face = 'bold', vjust = 0.5, colour = "black"),
    axis.title = element_text(size = 18, face = 'bold', colour = "black"),
    axis.title.x = element_text(size = 18, face = 'bold', colour = "black"),
    axis.title.y = element_text(size = 18, face = 'bold', colour = "black"),
    axis.line = element_line(colour = 'black'),
    
    #Legend
    legend.key = element_blank(), # removes the border
    legend.key.size = unit(1, "cm"), # Sets overall area/size of the legend
    legend.text = element_text(size = 18, face = "bold"), # Text size
    title = element_text(size = 18, face = "bold"))
ggsave("path_to_where_you_want_to_save/name_file.pdf", width = 15,
       height = 12)

######################
## correlation test ##
######################

# Calculate Pearson correlation and p-value
cor_test_result <- cor.test(data$Black.or.African.American.only, data$Black.or.African.American.only.1, method = "pearson")
correlation <- cor_test_result$estimate
p_value <- cor_test_result$p.value

# Create the scatter plot with regression line
ggplot(data, aes(x = Black.or.African.American.only, y = Black.or.African.American.only.1)) +
  geom_point() +  # Add scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression line with confidence interval
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
  theme_minimal()+ 
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  theme(legend.position = "bottom")+
  theme_bw(base_size = 24) +
  theme(
    legend.position = 'right',
    legend.background = element_rect(),
    plot.title = element_text(angle = 0, size = 18, face = 'bold', vjust = 1),
    plot.subtitle = element_text(angle = 0, size = 14, face = 'bold', vjust = 1),
    plot.caption = element_text(angle = 0, size = 14, face = 'bold', vjust = 1),
    
    axis.text.x = element_text(angle = 90, size = 18, face = 'bold', hjust = 1.0, vjust = 0.5, colour = "black"),
    axis.text.y = element_text(angle = 0, size = 18, face = 'bold', vjust = 0.5, colour = "black"),
    axis.title = element_text(size = 18, face = 'bold', colour = "black"),
    axis.title.x = element_text(size = 18, face = 'bold', colour = "black"),
    axis.title.y = element_text(size = 18, face = 'bold', colour = "black"),
    axis.line = element_line(colour = 'black'),
    
    #Legend
    legend.key = element_blank(), # removes the border
    legend.key.size = unit(1, "cm"), # Sets overall area/size of the legend
    legend.text = element_text(size = 18, face = "bold"), # Text size
    title = element_text(size = 18, face = "bold"))
ggsave("path_to_where_you_want_to_save/name_file.pdf", width = 15,
       height = 12)