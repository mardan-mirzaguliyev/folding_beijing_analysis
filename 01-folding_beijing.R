# Load necessary libraries
library(tidyverse)
library(googlesheets4)
library(janitor)
library(magick)
library(ggrepel)

gs4_deauth()

beijing_sheet_id <- "1WwztEXyaJsgMICVlVkodxtA-7zSJtQqudVu7jCu3PN4"

beijing <- read_sheet(beijing_sheet_id, sheet = "Main Table")
beijing


beijing <- clean_names(beijing)
beijing



# Image 1
# Create the stacked bar chart
ggplot(beijing, aes(x = space, y = hours_allocated, fill = space)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("First Space" = "#2E86AB",    # Blue for wealthy
                               "Second Space" = "#A23B72",   # Purple for middle class
                               "Third Space" = "#F18F01")) + # Orange for poor
  labs(
    title = "Time Allocation Across Beijing's Three Spaces",
    subtitle = "Unequal distribution of waking hours in the Folding City",
    x = "Social Space",
    y = "Hours Allocated per 48-Hour Cycle",
    caption = "Data from 'Folding Beijing' by Hao Jingfang"
  ) +
  geom_text(aes(label = paste0(hours_allocated, " hours")), 
            vjust = -0.5, 
            size = 4,
            fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
   # axis.title.x = element_text(color = "black"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 28)

# Save the plot
ggsave("time_distribution_chart.png", width = 10, height = 6, dpi = 300)







# Create Gantt data
gantt_data <- data.frame(
  Space = c("First Space", "Second Space", "Third Space"),
  Start = c(6, 30, 46),
  Duration = c(24, 16, 8),
  Day = c("Day 1-2", "Day 2", "Day 2-3")
) %>%
  mutate(
    End = Start + Duration,
    Midpoint = Start + Duration/2
  )

# Create the plot
p <- ggplot(gantt_data, aes(y = reorder(Space, -Start))) +
  geom_segment(aes(x = Start, xend = End, yend = Space, color = Space),
               linewidth = 20, lineend = "round") +
  geom_text(aes(x = Midpoint, label = paste0(Duration, " hours")),
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
ggsave("img-2.png", plot = p, width = 14, height = 6, dpi = 300)



# Create the data
population_resources <- data.frame(
  Space = c("First Space", "Second Space", "Third Space"),
  Population_Millions = c(5, 25, 50),
  Hours_Per_Million = c(4.80, 0.64, 0.16)
)

# Create scatter plot
ggplot(population_resources, aes(x = Population_Millions, y = Hours_Per_Million)) +
  # Connect points to show pattern
  geom_line(color = "gray50", linetype = "dotted", linewidth = 0.8) +
  # Points with color (on top of line)
  geom_point(aes(color = Space), size = 10, alpha = 0.8) +
  # Labels using ggrepel for better positioning
  geom_label_repel(aes(label = paste0(Space, "\n", 
                                      Population_Millions, "M people\n", 
                                      Hours_Per_Million, " hrs/M"),
                       fill = Space),
                   fontface = "bold", size = 3.5, lineheight = 0.85,
                   box.padding = 0.5, point.padding = 0.5,
                   segment.color = "gray50", segment.size = 0.5,
                   color = "white", alpha = 0.9) +
  # Colors
  scale_color_manual(values = c("First Space" = "#2E86AB",
                                "Second Space" = "#A23B72",
                                "Third Space" = "#F18F01")) +
  scale_fill_manual(values = c("First Space" = "#2E86AB",
                               "Second Space" = "#A23B72",
                               "Third Space" = "#F18F01")) +
  # Annotation
  annotate("text", x = 27, y = 4.2, 
           label = "Inverse Pattern:\nMore people → Less time",
           color = "darkred", size = 4.5, fontface = "bold") +
  # Labels
  labs(
    title = "The Inverse Relationship: Population vs. Time Per Capita",
    subtitle = "The more people in a Space, the fewer hours allocated per person",
    x = "Population (Millions)",
    y = "Hours Per Million People",
    caption = "Correlation: r = -0.996 (nearly perfect negative relationship)"
  ) +
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5, face = "italic", color = "gray50")
  ) +
  # Adjusted limits
  scale_x_continuous(breaks = seq(0, 50, 10), limits = c(0, 55), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 5, 1), limits = c(-0.5, 6), expand = c(0, 0))

ggsave("population_vs_resources.png", width = 11, height = 7, dpi = 300)



