# Install required packages if needed
# install.packages(c("ggplot2", "ggrepel"))

library(ggplot2)
library(ggrepel)

# Create sample data for stations and lines
stations <- data.frame(
  name = c("King's Cross", "Oxford Circus", "Piccadilly Circus", "Victoria", 
           "Waterloo", "London Bridge", "Bank", "Liverpool Street", "Holborn",
           "Westminster", "Embankment", "South Kensington"),
  x = c(2, 0.5, 0, 0, 1.5, 3, 3, 4, 1.5, -0.5, 0.5, -1),
  y = c(3, 1.5, 0.5, -1, -1, -1, 1, 1.5, 2, 0, 0, -0.5),
  is_interchange = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
                    TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

# Create connections between stations (lines)
lines <- data.frame(
  from = c("King's Cross", "Oxford Circus", "Piccadilly Circus", "Victoria", 
           "Waterloo", "London Bridge", "Bank", "Liverpool Street", 
           "King's Cross", "Oxford Circus", "Holborn", "Bank",
           "Westminster", "Embankment", "Embankment", "Victoria",
           "Westminster", "Piccadilly Circus", "South Kensington"),
  to = c("Oxford Circus", "Piccadilly Circus", "Victoria", "Waterloo", 
         "London Bridge", "Bank", "Liverpool Street", "Holborn", 
         "Holborn", "Holborn", "Bank", "King's Cross",
         "Embankment", "Piccadilly Circus", "Waterloo", "Oxford Circus",
         "South Kensington", "Oxford Circus", "Victoria"),
  line = c(rep("Central", 8), rep("Piccadilly", 4), 
           rep("District", 3), rep("Victoria", 2), rep("Circle", 2)),
  stringsAsFactors = FALSE
)

# Add coordinates to the lines data frame
lines <- merge(lines, stations[, c("name", "x", "y")], by.x = "from", by.y = "name")
names(lines)[names(lines) == "x"] <- "x1"
names(lines)[names(lines) == "y"] <- "y1"
lines <- merge(lines, stations[, c("name", "x", "y")], by.x = "to", by.y = "name")
names(lines)[names(lines) == "x"] <- "x2"
names(lines)[names(lines) == "y"] <- "y2"

# Define colors for each line (using official TfL colors)
line_colors <- c(
  "Central" = "#DC241F",  # Red
  "Piccadilly" = "#0019A8",  # Dark Blue
  "District" = "#007229",  # Green
  "Victoria" = "#00A0E2",  # Light Blue
  "Circle" = "#FFD300"   # Yellow
)

# Create river data (approximate Thames path)
thames <- data.frame(
  x = c(-1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.5),
  y = c(-0.8, -0.6, -0.4, -0.6, -0.8, -0.7, -0.5)
)

# Create the London-style map
ggplot() +
  # Add the Thames river
  geom_path(data = thames, 
            aes(x = x, y = y), 
            color = "#ADD8E6", 
            size = 6, 
            lineend = "round") +
  # Add the lines
  geom_segment(data = lines, 
               aes(x = x1, y = y1, xend = x2, yend = y2, color = line),
               size = 2) +
  # Add regular station markers
  geom_point(data = subset(stations, !is_interchange), 
             aes(x = x, y = y),
             size = 4, 
             color = "white", 
             fill = "white", 
             shape = 21, 
             stroke = 2) +
  # Add interchange station markers (larger, with black ring)
  geom_point(data = subset(stations, is_interchange), 
             aes(x = x, y = y),
             size = 6, 
             color = "black", 
             fill = "white", 
             shape = 21, 
             stroke = 2) +
  # Add white inner circle for interchange stations
  geom_point(data = subset(stations, is_interchange), 
             aes(x = x, y = y),
             size = 4.5, 
             color = "white", 
             fill = "white", 
             shape = 21) +
  # Add station labels
  geom_text_repel(data = stations,
                  aes(x = x, y = y, label = name),
                  size = 3.5,
                  fontface = "bold",
                  box.padding = 0.5,
                  point.padding = 0.5) +
  # Add the "River Thames" label
  annotate("text", x = 2, y = -1.1, label = "River Thames", 
           fontface = "italic", color = "#00008B", size = 3.5) +
  # Set the color scheme
  scale_color_manual(values = line_colors) +
  # Custom theme to match the London Underground map style
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  # Equal aspect ratio
  coord_fixed(ratio = 1) +
  # Title
  labs(title = "London-Style Transit Map",
       subtitle = "With Interchange Stations and River Thames")
