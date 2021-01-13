library(tidyverse)
library(lubridate)
library(ggtext)
library(colorspace)
library(extrafont)

# font_import(pattern = "Barlow", prompt = FALSE)
# font_import(pattern = "Chivo", prompt = FALSE)
loadfonts(quiet = TRUE)

url_vac_data <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
vac <- read_csv(url_vac_data)
glimpse(vac)

# how many countries?
count(vac, location)
# how many countries with at least 2 entries
vac %>% 
  filter(!is.na(total_vaccinations)) %>% 
  group_by(location) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  count(location)
# first and last date reported by country
vac %>% 
  filter(!is.na(total_vaccinations)) %>% 
  group_by(location) %>% 
  filter(n() > 1) %>% 
  summarize(across(date, .fns = list("earliest" = min, "latest" = max)))


label_countries <- function(x) {
  labels <- vector("character", length(x))
  for (i in seq_along(x)) {
    if (x[i] == "Germany") {
      labels[i] <- "<b style='color:green'>Germany</b>"
    } else {
      labels[i] <- x[i]
    }  
  }
  labels
}


vac %>% 
  filter(!is.na(total_vaccinations), location != "World") %>% 
  group_by(location) %>% 
  filter(n() > 2) %>% 
  ungroup() %>%
  ggplot(aes(date, total_vaccinations_per_hundred, group = location)) +
  geom_line(col = "grey70") +
  geom_line(data = . %>% filter(location == "Germany"), 
            col = "green",
            size = 1) +
  labs(title = "",
       subtitle = "",
       caption = "@4nsgarW. Data: Our World in Data.",
       x = NULL,
       y = "Vaccinations per 100") +
  facet_wrap(vars(location), labeller = as_labeller(label_countries), ncol = 3) +
  theme_minimal(base_family = "Source Sans Pro") +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey92"),
        text = element_text(color = "grey20"),
        plot.title = element_text(family = "Source Sans Pro SemiBold"),
        plot.caption = element_text(hjust = 0),
        axis.title.y = element_text(hjust = 1),
        strip.text = element_markdown(padding = margin(t = 6, b = 2, margin(t = 0)))
        )


update_geom_defaults("text", list(family = "Barlow", hjust = 0, vjust = 0.5, size = 2.25))

eu_countries <- c("Germany", "France", "Portugal", "Spain", "Italy", "Slovenia", "Slovakia", 
                  "Czechia", "Netherlands", "Belgium", "Luxembourg", "Austria", "Ireland", 
                  "Lithuania", "Hungary", "Denmark", "Sweden", "Malta", "Poland", "Cyprus",
                  "Latvia", "Estonia", "Bulgaria", "Romania", "Croatia", "Greece", "Finland")

uk_parts <- c("England", "Wales", "Scotland", "Northern Ireland")

plot_title <- "<span style='color:steelblue'>Deutschland</span> in der EU im Mittelfeld,<br>
Dänemark ist deutlich voraus"
plot_subtitle <- "Impfungen je 100 Einwohner"

vac %>% 
  filter(!is.na(total_vaccinations_per_hundred)) %>% 
  # filter(location != "World", !location %in% uk_parts) %>% 
  filter(!location %in% uk_parts) %>% 
  group_by(location) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  mutate(labelled_country = case_when(
    location == "Germany" ~ "<b style='color:steelblue'>Germany</b>",
    location == "World" ~ "<b>World</b>",
    TRUE ~ location
  )) %>% 
  ggplot(aes(reorder(labelled_country, total_vaccinations_per_hundred), 
             total_vaccinations_per_hundred)) + 
  geom_col(aes(fill = case_when(
    location == "Germany" ~ "Germany",
    location %in% eu_countries ~ "EU",
    location == "World" ~ "World",
    TRUE ~ "Other")),
    width = 0.75,
    show.legend = FALSE) +
  geom_text(data = . %>% filter(location == "Germany"),
            aes(label = scales::number(total_vaccinations_per_hundred, accuracy = 0.01, decimal.mark = ",")),
            color = "steelblue", nudge_y = 0.075) +
  geom_text(data = . %>% filter(location == "Denmark"),
            aes(label = scales::number(total_vaccinations_per_hundred, accuracy = 0.01, decimal.mark = ",")),
            color = lighten("steelblue"), nudge_y = 0.075) +
  geom_text(data = . %>% filter(location == "World"),
            aes(label = scales::number(total_vaccinations_per_hundred, accuracy = 0.01, decimal.mark = ",")),
            color = "grey20", nudge_y = 0.075) + 
  geom_text(data = . %>% filter(location == "Israel"),
            aes(label = scales::number(total_vaccinations_per_hundred, accuracy = 0.01, decimal.mark = ",")),
            color = "grey50", nudge_y = 0.075) + 
  scale_fill_manual(values = c("Other" = "grey70", 
                               "World" = "grey20",
                               "Germany" = "steelblue", 
                               "EU" = lighten("steelblue", 0.3))) +
  coord_flip() +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = "@4nsgarW. Quelle: Our World in Data (Stand 12.01.2021)",
       x = NULL, fill = NULL,
       y = NULL) +
  theme_minimal(base_family = "Barlow") +
  theme(panel.grid = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, color = "grey89"),
        text = element_text(color = "grey30", lineheight = 1.2),
        plot.title = element_markdown(family = "Source Sans Pro SemiBold"),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, size = 6),
        axis.title.x = element_text(hjust = 0),
        axis.text.y = element_markdown(size = 7),
        plot.background = element_rect(color = NA, fill = "grey98"),
        plot.margin = margin(t = 8, l = 8, r = 8, b = 8))

ggsave("plots/vaccinations_worldwide.png", type = "cairo", dpi = 200, width = 6, height = 6)

