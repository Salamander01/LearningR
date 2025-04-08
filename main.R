
dataTibble <- tibble(
  year = numeric(),
  month = character(),
  cart = character(),
  time = numeric()
)

csvData <- read_csv("timeData.csv")

mutateData <- mutate(csvData, Values = lapply(strsplit(Values, ","), as.numeric))

myData <- unnest_longer(mutateData, Values)

summary_data <- myData %>%
  group_by(Year, Month, Collection) %>%
  summarize(
    mean_value = mean(Values),
    min_value = min(Values),
    max_value = max(Values),
    .groups = "drop"
  )

# Convert Month to ordered factor
month_abbr <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Create a Year-Month field for proper ordering
summary_data <- summary_data %>%
  mutate(
    MonthNum = match(Month, month_abbr),
    YearMonth = factor(paste0(Year, "-", sprintf("%02d", MonthNum)))
  )

ggplot(summary_data, aes(x = YearMonth, y = mean_value, color = Collection, group = Collection, fill = Collection)) +
  ylim(10, 60) +
  scale_x_discrete(labels = function(x) month_abbr[as.numeric(sub(".*-(\\d+)$", "\\1", x))]) +

  # Add the min-max shaded regions
  geom_ribbon(aes(ymin = min_value, ymax = max_value, color = NULL), alpha = 0.2) +
  # Add the mean lines
  geom_line(size = 1) +
  geom_point(size = 3) +

  # Add text labels for mean values (slightly above the points)
  geom_text(aes(label = round(mean_value, 1)), vjust = -0.8, size = 4) +

  # Add text labels for minimum values (at the bottom of the ribbon)
  geom_text(aes(y = min_value, label = round(min_value, 1)), vjust = 2, size = 4) +
  geom_text(aes(y = max_value, label = round(max_value, 1)), vjust = -0.8, size = 4) +

  facet_wrap(~Collection) +

  # Labels and theme
  labs(
    title = "Average Cart Times",
    subtitle = "With Min-Max Range",
    x = "Month",
    y = "Value",
    color = "Category",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold")
  )

ggsave(filename = "graph.png", width = 10, height = 10, bg = "white")