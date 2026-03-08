# Load necessary libraries
library(tidyverse)
library(googlesheets4)
library(janitor)


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





library(ggplot2)
library(dplyr)
library(magick)

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
            color = "white", fontface = "bold", size = 5) +
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
    subtitle = "Red dashed lines mark 'The Change' - when the city physically folds",
    x = "Time (48-Hour Cycle)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "white"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray70"),
    axis.text.y = element_text(size = 12, face = "bold", color = "white"),
    axis.text.x = element_text(size = 9, color = "white"),
    axis.title.x = element_text(color = "white", size = 11),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray30", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#1a1a2e", color = NA),
    panel.background = element_rect(fill = "#1a1a2e", color = NA)
  )

# Save the base plot
ggsave("temp_gantt.png", plot = p, width = 14, height = 6, dpi = 300)

# Read the saved plot
plot_img <- image_read("temp_gantt.png")

# Read your folding city image
city_img <- image_read("img-2.jpeg")  # or "img-2.jpg"

# Resize the city image (adjust size as needed)
city_img_resized <- image_scale(city_img, "500")  # 500 pixels wide

# Add a subtle border/frame to the image (optional)
city_img_resized <- image_border(city_img_resized, "white", "2x2")

# Composite the images together - bottom right corner
final_plot <- image_composite(plot_img, city_img_resized, 
                              gravity = "southeast",
                              offset = "-50-50")  # 50 pixels from right and bottom edges

# Save the final image
image_write(final_plot, "gantt_timeline_final.png")

# Clean up temp file
file.remove("temp_gantt.png")

cat("✓ Final plot saved as: gantt_timeline_final.png\n")


