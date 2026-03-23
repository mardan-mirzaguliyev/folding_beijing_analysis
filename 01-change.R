# Load necessary libraries
library(tidyverse)
library(googlesheets4)
library(janitor)


# Read Google Sheets file and Sheet
gs4_deauth()

beijing_sheet_id <- "1WwztEXyaJsgMICVlVkodxtA-7zSJtQqudVu7jCu3PN4"

beijing_change <- read_sheet(beijing_sheet_id, sheet = "Change")
beijing_change

# Conventional column names for easier data manipulation
beijing_change <- clean_names(beijing_change)
beijing_change


# Creating columns that will be used in data visualization
beijing_change <- beijing_change |>
  mutate(
    end = start + duration,
    midpoint = start + duration/2
  )

beijing_change


change_plot <- ggplot(beijing_change, aes(y = reorder(space, -start))) +
  geom_segment(aes(x = start, xend = end, yend = space, color = space),
               linewidth = 20, lineend = "butt") +
  geom_text(aes(x = midpoint, label = paste0(duration, " hours")),
            color = "white", fontface = "bold", size = 5) +
  scale_color_manual(values = c("First Space" = "#2E86AB",
                                "Second Space" = "#A23B72",
                                "Third Space" = "#F18F01")) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 46, 48, 54),
                    labels = c("12AM\nDay 1", "6AM", "12PM", "6PM",
                               "12AM\nDay 2", "6AM", "12PM", "6PM",
                               "10PM", "12AM\nDay 3", "6AM"),
                     limits = c(0, 54)) +
  geom_vline(xintercept = c(6, 30, 46), linetype = "dashed", 
             color = "#2f332e", alpha = 0.7, linewidth = 1.5) +
  annotate("text", x = 6, y = 3.7, 
           label = "CHANGE\n6 AM Day 1", 
           color = "#2f332e", size = 3.5, fontface = "bold", hjust = 0.5) +
  annotate("text", x = 30, y = 3.7, 
           label = "CHANGE\n6 AM Day 2", 
           color = "#2f332e", size = 3.5, fontface = "bold", hjust = 0.5) +
  annotate("text", x = 46, y = 3.7, 
           label = "CHANGE\n10 PM Day 2", 
           color = "#2f332e", size = 3.5, fontface = "bold", hjust = 0.5) +
  labs(
    title = "Active Time Periods in Folding Beijing",
    subtitle = "The Change happens",
    x = "Time (48-Hour Cycle)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.title.x = element_text(color = "black", size = 11),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#cbe8f5", color = NA),
    panel.background = element_rect(fill = "#cbe8f5", color = NA)
  )


# View change plot
change_plot

# Save the change plot
ggsave("images/01-Change.png", plot = change_plot, width = 14, height = 6, dpi = 300)



