library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

crpgs <- games %>%
  filter(gamename %in% c("Pillars of Eternity",
                         "Pillars of Eternity II: Deadfire",
                         "Divinity: Original Sin Enhanced Edition",
                         "Divinity: Original Sin 2",
                         "Disco Elysium",
                         "Pathfinder: Kingmaker",
                         "Wasteland 2",
                         "Baldur's Gate 3",
                         "Fallout",
                         "Fallout 2",
                         "Baldur's Gate: Enhanced Edition",
                         "Baldur's Gate II: Enhanced Edition",
                         "STAR WARS\u0099: Knights of the Old Republic\u0099",
                         "STAR WARS\u0099 Knights of the Old Republic\u0099 II: The Sith Lords\u0099",
                         "Tyranny")) 

unique(crpgs$gamename)

crpgs_tidied <- crpgs %>%
  mutate(
    classic_revival = case_when(
      gamename %in% c("Pillars of Eternity",
                      "Pillars of Eternity II: Deadfire",
                      "Divinity: Original Sin Enhanced Edition",
                      "Divinity: Original Sin 2",
                      "Disco Elysium",
                      "Pathfinder: Kingmaker",
                      "Wasteland 2",
                      "Baldur's Gate 3",
                      "Tyranny") ~ "Revival",
      TRUE ~ "Classic"
    ),
    month = as.factor(month) %>% fct_relevel(., month.name) %>% fct_relabel(~month.abb),
    date = make_date(year = year, month = month)
  ) 

crpgs_tidied %>%
  arrange(desc(avg))

crpgs_tidied %>%
  arrange(year)

crpgs_tidied <- crpgs_tidied %>%
  group_by(gamename) %>%
  mutate(sum_avg = sum(avg, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(gamename = as_factor(gamename) %>% fct_reorder(., sum_avg))

crpgs_tidied <- crpgs_tidied %>%
  mutate(gamename = str_remove_all(gamename, "\u0099"))

crpgs_tidied %>%
  filter(gamename == "Disco Elysium") # data error for Disco Elysium - tester?

crpgs_tidied %>%
  filter(gamename == "Baldur's Gate: Enhanced Edition") %>%
  arrange(avg)

crpgs_tidied %>%
  filter(avg == 0)

crpgs_tidied <- crpgs_tidied %>%
  filter(!avg == 0.12) %>%
  filter(!avg == 0)



plot1 <- crpgs_tidied %>%
  filter(classic_revival == "Revival") %>%
  ggplot() +
  geom_line(aes(x = date, y = avg, group = gamename, col = gamename)) +
  facet_wrap(~gamename, labeller = labeller(gamename = label_wrap_gen(30))) +
  ggtitle("Games from the 'CRPG Revival' have large but declining numbers of concurrent players") +
  theme_minimal() +
  scale_y_log10(breaks = c(10, 100, 1000, 10000), limits = c(5, 30000)) +
  ggeasy::easy_remove_legend() +
  ylab("") +
  xlab("") +
  theme(strip.text = element_text(face = "bold", colour = "white", size = 10), 
        plot.background = element_rect("black"),
        axis.text = element_text(colour = "white"), panel.background = element_rect("black"),
        axis.ticks = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        plot.title = element_text(color = "white", size = 12, face = "plain")
        )

plot2 <- crpgs_tidied %>%
  filter(classic_revival == "Classic") %>%
  ggplot() +
  geom_line(aes(x = date, y = avg, group = gamename, col = gamename)) +
  facet_wrap(~gamename, labeller = labeller(gamename = label_wrap_gen(30))) +
  ggtitle("Classic 'golden-era' CRPGs have a smaller but growing player-base - new users discovering the classics\nor older players revisiting them?") +
  theme_minimal() +
  scale_y_log10(breaks = c(0, 10, 100, 1000, 10000), limits = c(5, 30000)) +
  ggeasy::easy_remove_legend() +
  ylab("") +
  xlab("") +
  theme(strip.text = element_text(face = "bold", colour = "white", size = 10), 
        plot.background = element_rect("black"),
        axis.text = element_text(colour = "white"), panel.background = element_rect("black"),
        axis.ticks = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        plot.title = element_text(color = "white", size = 11, face = "plain")
  )

c_plot <- plot1 / plot2 + plot_layout(heights = c(3,2)) +
  plot_annotation(title = "\nAverage monthly concurrent players of CRPGs on Steam over time",
                  subtitle = "log scale\n",
                  caption = "Plot by @cjrwebb. Data from github.com/rfordatascience/tidytuesday.",
                  theme = theme(plot.margin = margin(20, 80, 20, 40),
                                plot.background = element_rect("black"),
                                title = element_text(color = "white", face = "bold", size = 14, hjust = 0.5),
                                plot.subtitle = element_text(color = "grey", face = "plain", size = 12),
                                plot.caption = element_text(color = "white", face = "plain", size = 8)))

ggsave(plot = c_plot, filename = "crpg_players.png",
      width = 8.3, height = 11.7, units = "in", dpi = 600, scale = 1.15
      )

