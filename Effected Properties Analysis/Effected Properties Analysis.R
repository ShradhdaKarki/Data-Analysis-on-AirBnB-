# Installs required pacman packages if needed.
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, tidyr, purrr, dplyr, readr, plyr, ggplot2)

setwd("~/Desktop/R/proj/data/")
listings_files_dir <- "listings"
listings_files <- list.files(path = listings_files_dir, pattern = "*.csv.gz", full.names = TRUE)
listings_files

df_listings <- ldply(listings_files, read_csv)

df_listings <- select(df_listings, neighbourhood, latitude, longitude, room_type, price, minimum_nights, reviews_per_month, host_id, id, last_review)
head(df_listings, 10)

c(unique(df_listings["neighbourhood"]))
c(unique(df_listings["room_type"]))
c(unique(df_listings["minimum_nights"]))

# The number of rental airbnbs in each neighbourhood and the percentage distribution between them.
# Most in Dorchester  11.45% and least in Chelsea 0.004%
freq_location <- data.frame(cbind(Frequency = table(df_listings$neighbourhood), Percent = prop.table(table(df_listings$neighbourhood)) * 100))
freq_location <- freq_location[order(freq_location$Frequency), ]
freq_location

freq_area <- data.frame(cbind(Frequency = table(df_listings$neighbourhood), Percent = prop.table(table(df_listings$neighbourhood)) * 100))
freq_area <- freq_area[order(freq_area$Frequency), ]
freq_area

# Most of the airbnb rentals are entire home/apartment with a 62.41% and private room with a 36.25%
freq_type <- data.frame(cbind(Frequency = table(df_listings$room_type), Percent = prop.table(table(df_listings$room_type)) * 100))
freq_type <- freq_type[order(freq_type$Frequency), ]
freq_type

tema <- theme(
  plot.title = element_text(size = 10, hjust = .5),
  axis.text.x = element_text(size = 10, angle = 90, face = "bold"),
  axis.text.y = element_text(size = 10, angle = 90, face = "bold"),
  axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  legend.text = element_text(size = 10, face = "bold")
)

df_listings <- data.frame(neighbourhood = row.names(tail(freq_area, 10)), Frequency = tail(freq_area, 10)$Frequency)

options(repr.plot.width = 8, repr.plot.height = 2)
ggplot(data = df_listings, mapping = aes(x = neighbourhood, y = Frequency)) +
  theme_minimal() +
  geom_point(size = 3, color = "green") +
  ggtitle("The 10 most frequent neighbourhood") +
  xlab("") +
  geom_line(color = "black", size = 2, linetype = 16, group = 1, alpha = .5) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +
  tema


df_listings <- data.frame(room_type = row.names(tail(freq_type, 4)), Frequency = tail(freq_type, 4)$Frequency)

options(repr.plot.width = 8, repr.plot.height = 2)
ggplot(data = df_listings, mapping = aes(x = room_type, y = Frequency)) +
  theme_minimal() +
  geom_point(size = 3, color = "green") +
  ggtitle("The 4 most frequent room type") +
  xlab("") +
  geom_line(color = "black", size = 2, linetype = 16, group = 1, alpha = .5) +
  geom_bar(stat = "identity", mapping = aes(fill = room_type, color = room_type), alpha = .8, size = 1.5) +
  tema
