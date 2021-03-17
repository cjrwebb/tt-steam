library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggimage)
library(showtext)

font_files()
font_add(family = "Stardew Valley", "Stardew_Valley.ttf")
font_families()

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

stardew <- games %>%
  filter(gamename %in% c("Stardew Valley")) 

stardew <- stardew %>%
  filter(avg >= 2000 & avg <= 30000) %>%
  mutate(month = as.factor(month) %>% fct_relevel(., month.name) %>% fct_relabel(~month.abb),
         date = make_date(year = year, month = month)) %>%
  group_by(month) %>%
  mutate(avg_combined = mean(avg, na.rm = TRUE)) %>%
  ungroup()

# Set chicken selection
set.seed(40) # find a seed with a nice variation
stardew_agg <- stardew %>%
  group_by(month) %>%
  summarise(avg_combined = first(avg_combined)) %>%
  rowwise() %>%
  mutate(chicken = paste0("img/chicken-", sample(1:3, 1), ".png"))

set.seed(20)
stardew <- stardew %>%
  rowwise() %>%
  mutate(chicken = paste0("img/chick-", sample(1:4, 1), ".png"),
         chicken_scaler = case_when(
           chicken == "img/chick-1.png" ~ 0.035,
           chicken == "img/chick-2.png" ~ 0.025,
           chicken == "img/chick-3.png" ~ 0.03,
           chicken == "img/chick-4.png" ~ 0.03,
           TRUE ~ 0.02
         ))

# Main calendar plot

cal_plot <- stardew %>%
  ggplot() +
  geom_hline(yintercept = 30000, alpha = 0.3, colour = "#DE7B43") +
  geom_hline(yintercept = 20000, alpha = 0.1, colour = "#DE7B43") +
  geom_hline(yintercept = 10000, alpha = 0.1, colour = "#DE7B43") +
  geom_hline(yintercept = 0, alpha = 0.3, colour = "#DE7B43") +
  geom_image(aes(x = month, y = avg, image = chicken, size = I(chicken_scaler)), 
             position = position_jitter(width = 0.2, height = 0, seed = 5)) +
  geom_image(data = stardew_agg, aes(x = month, y = avg_combined, image = chicken)) +
  annotate(geom = "segment", x = 0.50, xend = 0.52, y = 0, yend = 30000, 
           arrow = arrow(length = unit(0.1, "in"))) +
  annotate(geom = "text", x = 0.46, y = 9000, hjust = 0, vjust = 0, size = 3.5, 
           label = "More players", angle = 90, family = "Stardew Valley") +
  annotate(geom = "text", label = " 0", x = 0.54, y = 0, hjust = 0, vjust = 0, size = 4.5,
           family = "Stardew Valley") +
  annotate(geom = "text", label = " 30k", x = 0.54, y = 30000, hjust = 0, vjust = 1, size = 4.5,
           family = "Stardew Valley") +
  geom_image(x = 0.52, y = -9999, image = "img/duck.png") +
  scale_y_continuous(limits = c(-10000, 30000)) +
  coord_polar() +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(family = "Stardew Valley", size = 16, colour = "#733500"),
        plot.background = element_rect(fill = "#FFE39C", colour = "#FFE39C"),
        plot.margin=unit(c(-90,-40,-50,-70), "mm"))

# Legend plot

leg_plot <- ggplot() +
  geom_image(aes(x = 1, y = 1), size = 0.2, image = "img/chicken-1.png") +
  geom_image(aes(x = 1, y = 2), size = 0.1, image = "img/chick-3.png") +
  annotate("curve", x = 2.5, xend = 1.5, y = 1, yend = 0.75, curvature = -0.3,
           arrow = arrow(length = unit(0.05, "in"), type = "closed"), size = 1,
           colour = "#733500") +
  annotate("curve", x = 2.5, xend = 1.1, y = 2, yend = 2.25, 
           arrow = arrow(length = unit(0.05, "in"), type = "closed"), size = 1,
           colour = "#733500") +
  annotate("text", x = 2.6, y = 1, label = "Chickens show\naverage concurrent\nplayers across\nall years",
           hjust = 0, vjust = 0.6, family = "Stardew Valley", size = 4, lineheight = 0.7, colour = "#733500") +
  annotate("text", x = 2.6, y = 2, label = "Baby chicks show\naverage concurrent\nplayers entries for\neach year",
           hjust = 0, vjust = 0.6, family = "Stardew Valley", size = 4, lineheight = 0.7, colour = "#733500") +
  xlim(c(0,5)) +
  ylim(c(0,3)) +
  coord_fixed(5/3) +
  theme_nothing() +
  theme(plot.background = element_rect(fill = "#FFE39C", colour = "#FFE39C"))

# Peak players plot

stardew_full <- games %>%
  filter(gamename %in% c("Stardew Valley")) 

stardew_full <- stardew_full %>%
  mutate(month = as.factor(month) %>% fct_relevel(., month.name) %>% fct_relabel(~month.abb),
         date = make_date(year = year, month = month)) %>%
  group_by(month) %>%
  mutate(avg_combined = mean(avg, na.rm = TRUE)) %>%
  ungroup()

#stardew_full %>%
#  arrange(desc(peak)) %>% view()

# Feb 26th 2016 - Launched
# April 30th 2018 - Multiplayer Mode Goes into Beta
# November 26th 2019 - Version 1.4 is released
# March 2020 - COVID-19 Lockdowns begin
# December 21st 2020 - Version 1.5 is released

peaks_plot <- stardew_full %>%
  ggplot() +
  geom_area(aes(x = date, y = peak), fill = "#75C745") +
  annotate("curve", x = as_date("2016-07-01"), xend = as_date("2016-03-01"), yend = 70856, y = 110000,
           colour = "#733500") +
  annotate("text", x = as_date("2016-07-20"), y = 110000, 
           label = "February 26th 2016:\nStardew Valley launches.\nPeak players: 64,256",
           hjust = 0, family = "Stardew Valley", lineheight = 0.7,
           colour = "#733500") +
  geom_image(x = as_date("2016-03-01"), y = 70856, image = "img/junimo.png", size = 0.04, asp = 2.5/1) +
  annotate("curve", x = as_date("2017-09-01"), xend = as_date("2018-05-01"), yend = 65000, y = 50000, 
           curvature = 0.3, colour = "#733500") +
  annotate("text", x = as_date("2017-09-01"), y = 50000,
           label = "April 30th 2018:\nMultiplayer beta.\nPeak players: 54,212",
           hjust = 1, family = "Stardew Valley", lineheight = 0.7, colour = "#733500") +
  geom_image(x = as_date("2018-05-01"), y = 65000, image = "img/heart.png", asp = 2.5/1) +
  annotate("curve", x = as_date("2019-08-01"), xend = as_date("2019-12-01"), yend = 50000, y = 90000, 
           curvature = -0.6, colour = "#733500") +
  annotate("text", x = as_date("2019-08-01"), y = 90000, 
           label = "November 26th 2019:\nVersion 1.4 released.\nPeak players: 38,024",
           hjust = 1, family = "Stardew Valley", lineheight = 0.7, colour = "#733500") +
  geom_image(x = as_date("2019-12-01"), y = 50000, image = "img/dog.png", asp = 2.5/1) +
  annotate("curve", x = as_date("2020-06-01"), xend = as_date("2021-01-01"), yend = 110000, y = 125000, 
           curvature = -0.5, colour = "#733500") +
  annotate("text", x = as_date("2020-06-01"), y = 125000, 
           label = "December 21st 2020:\nVersion 1.5 released.\nPeak players: 94,479",
           hjust = 1, family = "Stardew Valley", lineheight = 0.7, colour = "#733500") +
  geom_image(x = as_date("2021-1-01"), y = 110000, image = "img/a-fruit.png", asp = 2.5/1) +
  ylim(c(0, 150000)) +
  xlab("\n") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  coord_fixed(1/200) +
  theme_nothing() +
  theme(axis.text.x = element_text(family = "Stardew Valley", colour = "#733500"),
        plot.background = element_rect(fill = "#FFE39C", colour = "#FFE39C"),
        axis.title.x = element_text(family = "Stardew Valley", colour = "#733500"))


(cal_plot + (plot_spacer() / 
               leg_plot + plot_layout(heights = c(1,2))) + 
               plot_layout(widths = c(5,2))) / peaks_plot & 
  theme(plot.background = element_rect(fill = "#FFE39C", colour = "#FFE39C"))

# Calendar plot only
(cal_plot + (plot_spacer() / 
               leg_plot + plot_layout(heights = c(1,2))) + 
    plot_layout(widths = c(5,2))) & 
  theme(plot.background = element_rect(fill = "#FFE39C", colour = "#FFE39C"))

# Add information

cal_plot_info <- ggplot() +
  annotate("text", x = 0.5, y = 2.4, 
           label = "Seasonal Stardew",
           vjust = 1, hjust = 0,
           size = 8, family = "Stardew Valley", colour = "#733500", fontface = "bold") +
  annotate("text", x = 0.5, y = 2, 
           label = str_wrap("What times of year do people visit Stardew Valley? The plot shows how the average number of concurrent players each month changes through the seasons (between launch and December 2020)", 25),
           vjust = 1, hjust = 0, lineheight = 0.7,
           size = 5, family = "Stardew Valley", colour = "#733500") +
  xlim(c(0, 5)) +
  ylim(c(0, 3)) +
  coord_fixed(5/3) +
  theme_nothing() +
  theme(plot.background = element_rect(fill = "#FFE39C", colour = "#FFE39C"))

# Final plot, circle calendar

stardew_1 <- (cal_plot + (cal_plot_info / 
               leg_plot + plot_layout(heights = c(1,1))) + 
    plot_layout(widths = c(4,2))) +
  plot_annotation(caption = "Data Vis by @cjrwebb. Data from SteamCharts via #TidyTuesday   ") & 
    theme(plot.background = element_rect(fill = "#FFE39C", colour = "#FFE39C"),
          plot.caption = element_text(colour = "#733500", family = "Stardew Valley"),
          plot.margin = margin(5, 2, 10, 0)) 

# Final plot, peaks

stardew_2 <- peaks_plot +
  plot_annotation(title = "Mayor Lewis's Stardew Valley Tourism Report",
                  subtitle = "How the number of visitors to Stardew Valley continues to climb to new heights!",
                  caption = "Data Vis by @cjrwebb. Data from SteamCharts via #TidyTuesday") & 
  theme(plot.background = element_rect(fill = "#FFE39C", colour = "#FFE39C"),
        plot.title = element_text(colour = "#733500", family = "Stardew Valley", size = 24),
        plot.subtitle = element_text(colour = "#733500", family = "Stardew Valley", size = 16),
        plot.caption = element_text(colour = "#733500", family = "Stardew Valley"),
        plot.margin = margin(20, 15, 10, 15)) 

# Export plots

ggsave(filename = "stardew_1.png", plot = stardew_1,
       width = 9, height = 6)

ggsave(filename = "stardew_2.png", plot = stardew_2,
       width = 9, height = 5, bg = "#FFE39C")

