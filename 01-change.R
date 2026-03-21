# Load necessary libraries
library(tidyverse)
library(googlesheets4)
library(janitor)
library(magick)
library(ggrepel)

gs4_deauth()

beijing_sheet_id <- "1WwztEXyaJsgMICVlVkodxtA-7zSJtQqudVu7jCu3PN4"

beijing_change <- read_sheet(beijing_sheet_id, sheet = "Change")
beijing_change


beijing_change <- clean_names(beijing_change)
beijing_change




beijing_change <- beijing_change |>
  mutate(
    end = start + duration,
    midpoint = start + duration/2
  )

beijing_change


p <- ggplot(beijing_change, aes(y = reorder(space, -start))) +
  geom_segment(aes(x = start, xend = end, yend = space, color = space),
               linewidth = 20, lineend = "round") +
  geom_text(aes(x = midpoint, label = paste0(duration, " hours")),
            color = "black", fontface = "bold", size = 5) +
  scale_color_manual(values = c("First Space" = "#2E86AB",
                                "Second Space" = "#A23B72",
                                "Third Space" = "#F18F01")) +
  scale_x_continuous(breaks = seq(0, 54, 6),
                     labels = c("12AM\nDay 1", "6AM", "12PM", "6PM",
                                "12AM\nDay 2", "6AM", "12PM", "6PM",
                                "12AM\nDay 3", "6AM"),
                     limits = c(0, 54)) +
  geom_vline(xintercept = c(6, 30, 46), linetype = "dashed", 
             color = "red", alpha = 0.7, linewidth = 1.2) +
  annotate("text", x = 6, y = 3.7, 
           label = "CHANGE\n6 AM Day 1", 
           color = "red", size = 3.5, fontface = "bold", hjust = 0.5) +
  annotate("text", x = 30, y = 3.7, 
           label = "CHANGE\n6 AM Day 2", 
           color = "red", size = 3.5, fontface = "bold", hjust = 0.5) +
  annotate("text", x = 46, y = 3.7, 
           label = "CHANGE\n10 PM Day 2", 
           color = "red", size = 3.5, fontface = "bold", hjust = 0.5) +
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
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p
# Save the base plot
ggsave("images/01-Change.png", plot = p, width = 14, height = 6, dpi = 300)



