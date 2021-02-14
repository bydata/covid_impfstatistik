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


# custom ggplot2 theme
theme_custom <- function(base_family = "Barlow", base_size = 8, ...) {
  theme_minimal(base_family = base_family, base_size = base_size, ...) +
    theme(panel.grid = element_blank(),
          panel.grid.major.x = element_line(size = 0.1, color = "grey89"),
          text = element_text(color = "grey35", lineheight = 1.2),
          plot.title = element_markdown(family = "Source Sans Pro SemiBold", color = "black"),
          plot.title.position = "plot",
          plot.subtitle = element_markdown(size = round(base_size * 0.9)),
          plot.caption = element_text(hjust = 0, size = 6, color = "grey35", margin = margin(t = 8)),
          axis.title = element_text(hjust = 0, size = round(base_size * 0.8), color = "grey45"),
          axis.title.y = element_text(hjust = 0.75),
          axis.text = element_markdown(family = "Barlow Light", color = "grey45", size = round(base_size * 0.7)),
          plot.background = element_rect(color = NA, fill = "grey98"),
          plot.margin = margin(t = 8, l = 8, r = 8, b = 8),
          legend.position = "top",
          legend.justification = "left")
}
theme_set(theme_custom())
update_geom_defaults("text", list(family = "Barlow", hjust = 0, vjust = 0.5, size = 2.25, color = "grey45"))


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
  facet_wrap(vars(location), labeller = as_labeller(label_countries), ncol = 3) 


eu_countries <- c("Germany", "France", "Portugal", "Spain", "Italy", "Slovenia", "Slovakia", 
                  "Czechia", "Netherlands", "Belgium", "Luxembourg", "Austria", "Ireland", 
                  "Lithuania", "Hungary", "Denmark", "Sweden", "Malta", "Poland", "Cyprus",
                  "Latvia", "Estonia", "Bulgaria", "Romania", "Croatia", "Greece", "Finland")

uk_parts <- c("England", "Wales", "Scotland", "Northern Ireland", "Gibraltar", "Jersey", 
              "Guernsey", "Isle of Man", "Bermuda", "Cayman Islands", "Turks and Caicos Islands")

other_non_independent_regions <- c("Faeroe Islands", "Saint Helena")

plot_title <- "Impffortschritt gegen Covid-19 weltweit<br>
<span style='color:steelblue'>Deutschland</span> im Mittelfeld"
plot_subtitle <- "Impfdosen je 100 Einwohner"

eu_color <- lighten("steelblue", 0.3)

geom_text2 <- function(...) {
  geom_text(...,
            aes(label = scales::number(total_vaccinations_per_hundred, accuracy = 0.1, decimal.mark = ",")),
            nudge_y = 0.2)
}

vac %>% 
  filter(!is.na(total_vaccinations_per_hundred)) %>%
  filter(!location %in% c(uk_parts, other_non_independent_regions)) %>% 
  group_by(location) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  mutate(labelled_country = case_when(
    location == "Germany" ~ "<b style='color:steelblue'>Germany</b>",
    location == "European Union" ~ glue::glue("<b style='color:{eu_color}'>European Union</b>") %>% 
      as.character(),
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
  
  geom_text2(data = . %>% filter(location == "Germany"), color = "steelblue") +
  geom_text2(data = . %>% filter(location == "European Union"), color = eu_color) +
  geom_text2(data = . %>% filter(location == "World"), color = "grey20") + 
  geom_text2(data = . %>% filter(location == "Israel"), color = "grey50") + 
  scale_fill_manual(values = c("Other" = "grey70", 
                               "World" = "grey20",
                               "Germany" = "steelblue", 
                               "EU" = eu_color)) +
  coord_flip() +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = "Daten: Our World in Data (Stand 12.02.2021) | Visualisierung: @_ansgar",
       x = NULL, fill = NULL,
       y = NULL)

ggsave("plots/vaccinations_worldwide.png", type = "cairo", dpi = 200, width = 4.5, height = 6.5)




## Scatterplot doses per 100 vs. fully vaccinated in mio.

vac2 <- vac %>%
  filter(!is.na(total_vaccinations_per_hundred)) %>%
  filter(!location %in% c("World", "European Union", uk_parts, other_non_independent_regions)) %>%
  group_by(location) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(people_fully_vaccinated2 = replace_na(people_fully_vaccinated, 1))

highlight_locations <- c("Germany", "Israel", "United Kingdom", "United States", "Malta", "Russia")
color_palette <- "Dynamic"
colors <- qualitative_hcl(n = length(setdiff(highlight_locations, eu_countries)) + 2, palette = color_palette)


plot_title <- "Impffortschritt gegen Covid-19 weltweit"
plot_subtitle <- "Die x-Achse zeigt die Impfdosen je 100 Einwohner,
die y-Achse die absolute Zahl der <i>vollständig</i> geimpften Personen.<br>
Nicht dargestellt: Britische Inseln, sonstige Überseegebiete sowie
<b>über 100 Länder, in denen die Impfungen nicht begonnen haben</b>."


vac2 %>% 
  ggplot(aes(total_vaccinations_per_hundred, people_fully_vaccinated2)) +
  geom_point(data = . %>% filter(location == "Germany"),
             shape = 21, col = "grey20", fill = NA, size = 3) +
  geom_point(aes(
    fill = case_when(
      location %in% setdiff(highlight_locations, eu_countries) ~ location,
      location %in% eu_countries ~ "EU",
      TRUE ~ "Other")),
    shape = 21,col = "white", size = 2,
    show.legend = FALSE) +
  # geom_point(data = . %>% filter(location == "Germany"),
  #            shape = 21, col = "white", fill = "black", size = 2) +
  ggrepel::geom_text_repel(data = . %>% filter(location %in% highlight_locations),
                           aes(label = location),
                           box.padding = unit(0.75, "mm"),
                           family = "Barlow Light", size = 1.75, color = "grey35") +
  annotate("text", x = 0, y = 2, hjust = 0,
           family = "Barlow Light", size = 2, 
           label = "In diesen Ländern sind noch keine Personen vollständig geimpft") +
  annotate("richtext", x = 13, y = 6000, hjust = 0,
           family = "Barlow Light", size = 2, color = "grey35", label.color = NA, fill = NA,
           label = glue::glue("<span style='color:{colors[1]};font-family:Barlow SemiBold'>
                              Länder der EU</span> gemessen an den<br>Impfdosen pro 100 Einwohnern eng beieinander")) +
  geom_curve(aes(x = 13.2, xend = 11, y = 6000, yend = 12000),
             stat = "unique", curvature = -0.3,
             size = 0.1, color = "grey45",
             arrow = arrow(length = unit(0.5, "mm"), type = "closed")) +
  scale_y_log10(labels = scales::number_format(decimal.mark = ",")) +
  scale_fill_discrete_qualitative(palette = color_palette) + 
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = "Daten: Our World in Data (Stand 14.02.2021 oder früher). Visualisierung: @_ansgar",
    x = "Impfdosen je 100 Einwohner",
    y = "Personen mit vollständiger Impfung (log10)",
    fill = NULL
  )

ggsave("plots/vaccinations_scatter.png", type = "cairo", dpi = 200, width = 6, height = 4)

