# Load necessary packages
library(tidyverse)
library(googlesheets4)
library(janitor)
library(ggrepel)


# Read Google Sheets file and Sheet
gs4_deauth()

beijing_sheet_id <- "1WwztEXyaJsgMICVlVkodxtA-7zSJtQqudVu7jCu3PN4"

beijing_population <- read_sheet(beijing_sheet_id, sheet = "Population")
beijing_population

# Conventional column names for easier data manipulation
beijing_population <- beijing_population |> 
  clean_names()

# Creating column that will be used in data visualization
beijing_population <- beijing_population |> 
  mutate(hours_per_million = hours_allocated / population_in_millions)



poppulation_plot <- ggplot(beijing_population, aes(x = population_in_millions, y = hours_per_million)) +
  geom_line(color = "gray50", linetype = "dotted", linewidth = 0.8) +
  geom_point(aes(color = space), size = 10, alpha = 0.8) +
  geom_label_repel(aes(label = paste0(space, "\n", 
                                      population_in_millions, "M people\n", 
                                      hours_per_million, " hrs/M"),
                       fill = space),
                   fontface = "bold", size = 3.5, lineheight = 0.85,
                   box.padding = 0.5, point.padding = 0.5,
                   segment.color = "gray50", segment.size = 0.5,
                   color = "white", alpha = 0.9) +
  scale_color_manual(values = c("First Space" = "#2E86AB",
                                "Second Space" = "#A23B72",
                                "Third Space" = "#F18F01")) +
  scale_fill_manual(values = c("First Space" = "#2E86AB",
                               "Second Space" = "#A23B72",
                               "Third Space" = "#F18F01")) +
  annotate("text", x = 27, y = 4.2, 
           label = "Inverse Pattern:\nMore people → Less time",
           color = "darkred", size = 4.5, fontface = "bold") +
  labs(
    title = "The Inverse Relationship: Population vs. Time Per Capita",
    subtitle = "The more people in a Space, the fewer hours allocated per person",
    x = "Population (Millions)",
    y = "Hours Per Million People",
    caption = "Correlation: r = -0.996 (nearly perfect negative relationship)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5, face = "italic", color = "gray50"),
    plot.background = element_rect(fill = "#cbe8f5", color = NA),
    panel.background = element_rect(fill = "#cbe8f5", color = NA)
    ) +
  scale_x_continuous(breaks = seq(0, 50, 10), limits = c(0, 55), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 5, 1), limits = c(-0.5, 6), expand = c(0, 0))


poppulation_plot

ggsave("images/02-Population-resources.png", width = 11, height = 7, dpi = 300)


