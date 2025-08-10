# Windows file path: use either forward slashes or double backslashes
file_path <- "C:/Users/USER/Downloads/CLEANEDCRYPTODATA.csv"
# or
# file_path <- "C:\\Users\\USER\\Downloads\\CLEANEDCRYPTODATA.csv"

# Import CSV into a dataframe
crypto_data <- read.csv(file_path, stringsAsFactors = FALSE)

# View first few rows to check
head(crypto_data)




# Load ggplot2
library(ggplot2)
library(dplyr)

# Make sure Year is numeric
crypto_data$Year <- as.numeric(as.character(crypto_data$Year))

# Aggregate counts by Year and Classification
trend_data <- crypto_data %>%
  group_by(Year, Classification) %>%
  summarise(Count = n(), .groups = "drop")

# Plot line graph
ggplot(trend_data, aes(x = Year, y = Count, color = Classification)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Blockchain and Crypto Policy Trends",
    x = "Year",
    y = "Number of Documents",
    color = "Classification"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(trend_data$Year)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )







library(ggplot2)
library(dplyr)

# Make sure Year is numeric
crypto_data$Year <- as.numeric(as.character(crypto_data$Year))

# Aggregate total count of documents per year
yearly_counts <- crypto_data %>%
  group_by(Year) %>%
  summarise(Count = n(), .groups = "drop")

# Plot line graph
ggplot(yearly_counts, aes(x = Year, y = Count)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "Blockchain and Crypto Policy Trends",
    x = "Year",
    y = "Number of Documents"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(yearly_counts$Year)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


library(ggplot2)
library(dplyr)

crypto_data$Year <- as.numeric(as.character(crypto_data$Year))

yearly_counts <- crypto_data %>%
  group_by(Year) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(yearly_counts, aes(x = Year)) +
  # Policy Count line and points in deep navy blue
  geom_line(aes(y = Count, color = "Policy Count"), size = 1.2) +
  geom_point(aes(y = Count, color = "Policy Count"), size = 3) +
  # Trend line in vibrant yellow dashed
  geom_smooth(aes(y = Count, color = "Trend Line"), method = "loess", se = FALSE, linetype = "dashed", size = 1) +
  
  scale_color_manual(
    name = "Legend",
    values = c("Policy Count" = "#1F3A93", "Trend Line" = "#F1C40F")
  ) +
  
  scale_x_continuous(
    breaks = seq(min(yearly_counts$Year), max(yearly_counts$Year), by = 1),
    labels = scales::label_number(accuracy = 1)
  ) +
  
  labs(
    title = "Blockchain and Crypto Policy Trends",
    subtitle = "Tracking the rise of policy documents over time in Africa",
    x = "Year",
    y = "Publications Count",
    caption = "Data source: Curated from reputable articles, academic journals, and official documents."
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15), color = "black"),
    axis.title.x = element_text(face = "bold", size = 14, color = "black"),
    axis.title.y = element_text(face = "bold", size = 14, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.title = element_text(face = "bold", size = 12, color = "black"),
    legend.text = element_text(size = 11, color = "black")
  )

library(dplyr)

# Make sure Year is numeric
crypto_data$Year <- as.numeric(as.character(crypto_data$Year))

# Summarize count per year
yearly_counts <- crypto_data %>%
  group_by(Year) %>%
  summarise(Count = n(), .groups = "drop")

# Display the table
print(yearly_counts)

