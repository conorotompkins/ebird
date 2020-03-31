library(tidyverse)
library(lubridate)
library(vroom)
library(janitor)
library(rebird)
library(hrbrthemes)
library(ggrepel)
library(gganimate)

theme_set(theme_ipsum())

Sys.setenv(EBIRD_KEY = "Your-midtdb0i0l8e")

#ebirdhistorical(loc = 'US-VA-003', date = '2019-02-14',max = 10, key = "midtdb0i0l8e")

#df_freq <- ebirdfreq(loctype = 'states', loc = 'US-PA', key = "midtdb0i0l8e")

df_freq_raw <- ebirdfreq(loctype = 'states', loc = 'US-PA', startyear = 2019,
                         endyear = 2019, startmonth = 1, endmonth = 12)

df_freq_clean <- df_freq_raw %>% 
  clean_names() %>%
  separate(month_qt, into = c("month", "week")) %>% 
  mutate(week = as.numeric(week),
         month = ymd(str_c("2019", month, "01", sep = "-")),
         month = month(month, label = TRUE, abbr = TRUE),
         state = "PA") %>% 
  rename(common_name = com_name) %>% 
  arrange(common_name, month, week)

df_freq_clean

df_month <- df_freq_clean %>% 
  group_by(common_name, month) %>% 
  summarize(sample_size = sum(sample_size),
            frequency_mean = mean(frequency)) %>%
  ungroup()




df_top_birds <- df_freq_clean %>% 
  group_by(common_name) %>% 
  summarize(sample_size = sum(sample_size),
            frequency_mean = mean(frequency)) %>% 
  ungroup() %>% 
  arrange(desc(frequency_mean)) %>% 
  select(common_name) %>% 
  slice(1:10)

df_top_birds

df_month %>% 
  semi_join(df_top_birds) %>% 
  ggplot(aes(month, frequency_mean, group = common_name)) +
    geom_line()

df_month %>% 
  semi_join(df_top_birds) %>% 
  mutate(common_name = fct_inorder(common_name)) %>% 
  ggplot(aes(month, frequency_mean)) +
  geom_polygon(data = df_month %>% rename(name = common_name),
               aes(group = name),
               color = "grey", fill = NA, size = .5) +
  geom_polygon(aes(group = common_name),
               color = "blue", fill = NA, size = 1.2) +
  coord_polar() +
  facet_wrap(~common_name) +
  scale_y_percent() +
  labs(subtitle = "Most frequently observed birds in PA (2019)",
       x = NULL,
       y = "Frequency of observation",
       caption = "Data from ebird.org. @conorotompkins") +
  theme(plot.margin = margin(2, 2, 2, 2),
        strip.text = element_text(color = "blue", hjust = .5))

plot_animated <- df_month %>% 
  semi_join(df_top_birds) %>% 
  mutate(common_name = fct_inorder(common_name)) %>% 
  ggplot(aes(month, frequency_mean)) +
  geom_polygon(data = df_month %>% rename(name = common_name),
               aes(group = name),
               color = "grey", fill = NA, size = .5) +
  geom_polygon(aes(group = common_name),
               color = "blue", fill = NA, size = 1.2) +
  coord_polar() +
  #facet_wrap(~common_name) +
  scale_y_percent() +
   labs(subtitle = "Most frequently observed birds in PA (2019)",
        x = NULL,
        y = "Frequency of observation",
        caption = "Data from ebird.org. @conorotompkins") +
  theme(plot.margin = margin(2, 2, 2, 2),
        plot.title = element_text(color = "blue"))

plot_animated +
  transition_manual(common_name) +
  ggtitle("{current_frame}")
  
plot_animated +
  transition_manual(common_name) +
  ggtitle("{current_frame}") +
  anim_save("output/monthly_radar_frequency_plot_animated.gif")