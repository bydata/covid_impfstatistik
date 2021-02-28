pacman::p_load("tidyverse", "ggtext", "colorspace", "extrafont", "lubridate")
loadfonts(quiet = TRUE)

# custom ggplot2 theme
theme_custom <- function(base_family = "Barlow", base_size = 8, ...) {
  theme_minimal(base_family = base_family, base_size = base_size, ...) +
    theme(panel.grid = element_blank(),
          panel.grid.major.y = element_line(size = 0.1, color = "grey89"),
          text = element_text(color = "grey35", lineheight = 1.2),
          plot.title = element_markdown(family = "Source Sans Pro SemiBold", 
                                        color = "black", size = base_size * 2,
                                        margin = margin(b = 6)),
          plot.title.position = "plot",
          plot.subtitle = element_markdown(size = round(base_size * 1.3),
                                           margin = margin(b = 12)),
          plot.caption = element_markdown(hjust = 0, size = 6, color = "grey35", margin = margin(t = 8)),
          strip.text = element_text(family = "Source Sans Pro SemiBold", color = "grey35"),
          axis.title = element_text(hjust = 0, size = round(base_size * 0.8), color = "grey45"),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_markdown(family = "Barlow Light", color = "grey45", size = round(base_size * 0.7)),
          plot.background = element_rect(color = NA, fill = "grey98"),
          plot.margin = margin(t = 8, l = 8, r = 8, b = 8),
          legend.position = "top",
          legend.justification = "left")
}
theme_set(theme_custom())
update_geom_defaults("text", list(family = "Barlow", hjust = 0, vjust = 0.5, size = 2.25, color = "grey45"))

vaccine_mapping <- c("astra" = "Astra Zeneca", "astrazeneca" = "Astra Zeneca",
                     "comirnaty" = "BioNTech", "biontech" = "BioNTech",
                     "moderna" = "Moderna")
bundesland_mapping <- c("BB" = "Brandenburg", "BE" = "Berlin", "BW" = "Baden-Württemberg", "BY" = "Bayern",
                        "HB" = "Bremen", "HE" = "Hessen", "HH" = "Hamburg", "MV" = "Mecklenburg-Vorpommern",
                        "NI" = "Niedersachsen", "NW" = "Nordrhein-Westfalen", "RP" = "Rheinland-Pfalz", "SH" = "Schleswig-Holstein",
                        "SL" = "Saarland", "SN" = "Sachsen", "ST" = "Sachsen-Anhalt", "TH" = "Thüringen")


## Download vaccinations data ==========================================================
# Source: RKI/ARD: https://github.com/ard-data/2020-rki-impf-archive
url_vaccination <- "https://raw.githubusercontent.com/ard-data/2020-rki-impf-archive/master/data/9_csv_v2/all.csv"
file_vaccination <- "vaccinations.tsv"
filepath_vaccination <- file.path("data", file_vaccination)
download.file(url_vaccination, destfile = filepath_vaccination)
vaccination <- read_csv(filepath_vaccination)
glimpse(vaccination)

metric_regex <- str_c("dosen_(", str_c(names(vaccine_mapping), collapse = "|"), ")_kumulativ")

vaccination_filtered <- 
  vaccination %>% 
  filter(region != "DE") %>% 
  filter(str_detect(metric, metric_regex)) %>% 
  mutate(impfstoff = str_extract(metric, str_c(names(vaccine_mapping), collapse = "|")),
         impfstoff = vaccine_mapping[impfstoff],
         region = bundesland_mapping[region]) %>% 
  rename(dosen_cumul = value) %>% 
  select(date, region, impfstoff, dosen_cumul) %>% 
  # there are some issues with vaccination numbers - remove those
  group_by(region, impfstoff) %>% 
  arrange(date, .by_group = TRUE) %>% 
  filter((dosen_cumul <= lead(dosen_cumul) & dosen_cumul >= lag(dosen_cumul)) | 
           is.na(lead(dosen_cumul))) %>% 
  ungroup()

# get latest date for vaccinations 
max_date_vaccinations <- max(vaccination_filtered$date)


## Download vaccine delivery data ==========================================================
# Source: https://impfdashboard.de/daten
url_delivery <- "https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv"
file_delivery <- "germany_deliveries_timeseries_v2.tsv"
filepath_delivery <- file.path("data", file_delivery)
download.file(url_delivery, destfile = filepath_delivery)
delivery <- read_tsv(filepath_delivery)
glimpse(delivery)

delivery <- delivery %>% 
  arrange(impfstoff, region, date) %>% 
  group_by(impfstoff, region) %>% 
  mutate(dosen_cumul = cumsum(dosen),
         region = str_remove(region, "DE-"),
         region = bundesland_mapping[region],
         impfstoff = vaccine_mapping[impfstoff])

# complete dataset to latest date of vaccination data
delivery_expanded <- delivery %>% 
  group_by(region, impfstoff) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  mutate(date = max_date_vaccinations) %>% 
  bind_rows(delivery) %>% 
  arrange(region, impfstoff, date)


## EDA ===================================================================================

delivery_expanded %>% 
  ggplot(aes(date, dosen_cumul)) +
  geom_step() +
  facet_grid(rows = vars(region), cols = vars(impfstoff))

vaccination_filtered %>% 
  ggplot(aes(date, dosen_cumul)) +
  geom_line() +
  facet_grid(rows = vars(region), cols = vars(impfstoff), scales = "free_y")

vaccination_filtered %>% 
  filter(impfstoff == "BioNTech") %>% 
  ggplot(aes(date, dosen_cumul)) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y")

vaccination_filtered %>% 
  filter(impfstoff == "Astra Zeneca") %>% 
  ggplot(aes(date, dosen_cumul)) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y")


## PLOTS ============================================

# plot the doses available and administered by supplier
plot_doses_vaccine <- function(vaccine) {
  
  vaccine_colors <- c("Astra Zeneca" = "steelblue",
                      "BioNTech" = "orangered",
                      "Moderna" = "darkgreen")
  
  # filter deliveries for given supplier and create data structure for stepwise area chart
  delivery_expanded2 <- delivery_expanded %>%
    filter(impfstoff == vaccine) %>% 
    group_by(region) %>% 
    mutate(dosen_cumul = lag(dosen_cumul)) %>% 
    ungroup() %>% 
    filter(!is.na(dosen_cumul)) %>% 
    bind_rows(subset(delivery_expanded, impfstoff == vaccine))
  
  # doses administered for given supplier
  vaccination_subset <- subset(vaccination_filtered, impfstoff == vaccine & dosen_cumul > 0)
  
  delivery_expanded2 %>% 
    filter(dosen_cumul > 0) %>% 
    ggplot(aes(date, dosen_cumul)) +
    # area for doses delivered
    geom_step(col = "grey50", size = 0.25) +
    geom_ribbon(aes(ymin = 0, ymax = dosen_cumul), 
                outline.type = "upper",
                alpha = 0.1) +
    # line and area for doses administered
    geom_line(data = vaccination_subset,
              size = 0.75, col = vaccine_colors[vaccine]) +
    geom_area(data = vaccination_subset,
              alpha = 0.2, fill = vaccine_colors[vaccine]) +
    scale_x_date(date_labels = "%d.%m.", expand = c(0, 0)) +
    scale_y_continuous(labels = scales::number_format(scale = 10^-3, suffix = "k",decimal.mark = ",")) +
    facet_wrap(vars(region), scales = "free_y") +
    labs(title = glue::glue("Impfungen mit {vaccine}"),
         subtitle = glue::glue("<b style='color:grey50'>Gelieferte</b> und 
         <b style='color:{vaccine_colors[vaccine]}'>verimpfte</b> Impfdosen (kumulativ)<br>
         Skalierung der y-Achsen orientiert sich an den kumulativ verfügbaren Impfdosen 
                               und unterscheidet sich je Bundesland"), 
         caption = "Quelle: RKI/ARD, Bundesgesundheitsministerium. Visualisierung: @_ansgar",
         x = NULL,
         y = "Kumulative Impfdosen")
  
}

# produce plots for each supplier
for (vaccine in c("Astra Zeneca", "BioNTech", "Moderna")) {
  plot_doses_vaccine(vaccine)
  ggsave(glue::glue("plots/vaccinations_de_{vaccine}.png"), type = "cairo", dpi = 200, width = 7.5, height = 6)
  message(glue::glue("Saved {vaccine} plot"))
}
