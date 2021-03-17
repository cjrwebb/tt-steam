library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggrepel)

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

view(games)

# FINAL FANTASY XIV Online

ffxiv <- games %>%
  filter(gamename %in% c("FINAL FANTASY XIV Online")) 

ffxiv <- ffxiv %>%
  filter(avg >= 2000 & avg <= 30000) %>%
  mutate(month = as.factor(month) %>% fct_relevel(., month.name) %>% fct_relabel(~month.abb),
         date = make_date(year = year, month = month)) %>%
  group_by(month) %>%
  mutate(avg_combined = mean(avg, na.rm = TRUE)) %>%
  ungroup()

ffxiv_plot <- ffxiv %>%
  ggplot() +
  geom_area(aes(x = date, y = avg), fill = "#8FD1DA", alpha = 0.5) +
  geom_point(aes(x = date, y = avg), colour = "#21557E") +
  geom_line(aes(x = date, y = avg), colour = "#21557E") +
  geom_text(aes(x = date, y = avg + 2500, label = month), angle = 90, vjust = 0.5, hjust = 1) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  ggtitle("\nFINAL FANTASY XIV ONLINE: Average Concurrent Players\n") +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), title = element_text(size = 20, face = "bold"))

ggsave(filename = "ffxiv_plot.png", ffxiv_plot, dpi = 600, width = 18, height = 6, units = "in")


