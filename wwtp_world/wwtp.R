
# Set Dependencies ----

pacman::p_load(tidyverse, lubridate, janitor,
               ggmap, countrycode, # maps, mapproj, viridis, ggalt,
               skimr, 
               gridExtra, scales, ggthemes,  
               DT, plotly,
               extrafont, showtext
               )


# IMPORT ----

wwtp_raw <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv")


# Tidy & Transformation ----

wwtp_raw <- 
  wwtp_raw %>% 
  clean_names()

wwtp_raw %>% 
  glimpse()

wwtp_raw %>% 
  skim()

wwtp_raw %>% 
  count(COUNTRY)

wwtp_raw %>% 
  colnames()

wwtp_cleaned <- 
  wwtp_raw %>% 
  clean_names() %>% 
  mutate(country_clean = ifelse(country == countrycode(cntry_iso, 
                                                  origin = "iso3c",
                                                  destination = "country.name"),
                           country,
                           countrycode(cntry_iso, origin = "iso3c",
                                       destination = "country.name")
                           ),
         country_clean = ifelse(is.na(country_clean),
                                country,
                                country_clean)
         ) 

wwtp_cleaned %>% 
  skim()

# EDA ---- 

## WWTP Loc ----
  
wwtp_loc_map <-  
  ggplot() + 
  borders("world", fill = "darkslategrey", colour = "grey80", size = 0.2,
          xlim = c(-175, 180), ylim = c(-60, 85)
          ) +
  geom_point(data = wwtp_cleaned,
             aes(x = lon_wwtp,
                 y = lat_wwtp,
                 text = paste0("<br> Country : ", country_clean,
                               "<br> WWTP : ", wwtp_name,
                               "<br> Dilution Factor : ", df)
                 ),
             color = "coral1",
             alpha = 0.5,
             size = 0.4
             ) + 
  rcartocolor::scale_colour_carto_c(palette = "SunsetDark") +
  rcartocolor::scale_fill_carto_c(palette = "SunsetDark") +
  theme_void() +
  theme(plot.background   = element_rect(fill = "grey15", color = "grey10")
        )

plotly_wwtp_loc <- 
  ggplotly(wwtp_loc_map, tooltip = "text") %>% 
  layout(paper_bgcolor = "#202230",
         plot_bgcolor = "#202230")

## Discharge ----

# graph settings
options(scipen=10000)
font_add_google("Montserrat", "Montserrat")
showtext_auto() 

### Barplot for discharge by countries ----

bar_wwtp_top_countries <- 
  wwtp_cleaned %>% 
  count(country_clean, waste_dis) %>% 
  group_by(country_clean) %>% 
  dplyr::summarise(tot_n = sum(n),
                   sum_waste_dis = sum(waste_dis)
                   ) %>% 
  ungroup() %>% 
  slice_max(order_by = sum_waste_dis, n = 7) %>% 
  ggplot(aes(sum_waste_dis, fct_reorder(country_clean, sum_waste_dis), 
             fill = country_clean
             ) 
         ) +
  geom_col(show.legend = FALSE) + 
  geom_text(aes(label = scales::comma(round(sum_waste_dis, digits = 0))
                ), hjust = -0.1, 
            family = "Montserrat", color = "white",
            size = 3.5) +
  scale_x_continuous(expand = c(0,0), labels = scales::comma,
                     limits = c(0, 110000000)
                     ) +
  scale_y_discrete(NULL, expand = c(0,0)
                   ) +
  rcartocolor::scale_fill_carto_d(palette = "SunsetDark") +
  #scale_fill_brewer(palette = "Paired") +
  labs(title = paste0("Wastewater Discharges from WWTP across the world",
                      "\n"),
       x = paste0("Treated wastewater discharged by WWTP (m3/d)"),
       caption = paste0("Data:  Macedo et al, 2022| ",
                        "Paper: Distribution and characteristics of",
                        "wastewater treatment plants within the global river network",
                        " (2022)")
       ) +
  theme_minimal() +
  theme(text = element_text(family = "Montserrat"),
        plot.title = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, color = "white"),
        plot.background   = element_rect(fill = "grey15", color = "grey10"),
        plot.caption = element_text(family = "Montserrat",
                                    color = "white")
        )

lollipop_wwtp_top_countries <- 
  wwtp_cleaned %>% 
  count(country_clean, waste_dis) %>% 
  group_by(country_clean) %>% 
  dplyr::summarise(tot_n = sum(n),
                   sum_waste_dis = sum(waste_dis)
                   ) %>% 
  ungroup() %>% 
  slice_max(order_by = sum_waste_dis, n = 7) %>% 
  ggplot(aes(fct_reorder(country_clean, sum_waste_dis), 
             sum_waste_dis
             )) + 
  geom_segment(aes(x = country_clean,
                   xend = country_clean,
                   y = 0,
                   yend = sum_waste_dis),
               color = "white") +
  geom_point(aes(color = country_clean),
             size = 4,
             show.legend = FALSE) +
  geom_text(aes(label = scales::comma(round(sum_waste_dis, digits = 0)
                                      )
                ), hjust = -0.2, 
            family = "Montserrat", color = "white",
            size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 100000000), expand = c(0.01,0)
                     ) +
  scale_x_discrete(NULL
                   ) +
  labs(title = paste0("Wastewater Discharges from WWTP across the world"),
       y = paste0("Treated wastewater discharged by WWTP (m3/d)"),
       caption = paste0("Data:  Macedo et al, 2022| ",
                        "Paper: Distribution and characteristics of",
                        "wastewater treatment plants within the global river network",
                        " (2022)")
       ) +
  theme_minimal() +
  rcartocolor::scale_color_carto_d(palette = "SunsetDark") +
  theme(text = element_text(family = "Montserrat"),
        plot.title = element_text(color = "white"),
        axis.title.x = element_text(color = "white"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "white", size = 11),
        panel.grid = element_blank(),
        #panel.grid.major.x = element_line(size = 0.1, color = "white"),
        plot.background   = element_rect(fill = "grey15", color = "grey10"),
        plot.caption = element_text(family = "Montserrat",
                                    color = "white")
        )


continent <- 
  raster::ccodes() %>% 
  select(NAME, ISO3, continent) %>% 
  rename(country1 = NAME,
         cntry_iso = ISO3)

wwtp_continent <- 
  wwtp_cleaned %>% 
  left_join(continent, by = c("cntry_iso")
            ) %>% 
  mutate(continent = ifelse(is.na(continent) & cntry_iso == "XKX",
                            "Europe", continent)
         ) 

wwtp_continent %>% 
  count(continent)

forplotly_dis <-  
  ggplot() + 
  borders("world", fill = "darkslategrey", colour = "grey80", size = 0.2,
          xlim = c(-175, 180), ylim = c(-60, 85)
          ) +
  geom_point(data = 
               wwtp_cleaned %>% 
               filter(waste_dis > 500000) %>% 
               mutate(bin_waste_dis = cut(waste_dis, breaks = 7,
                                          ordered_result = T)
                      ),
             aes(x = lon_wwtp,
                 y = lat_wwtp,
                 colour = waste_dis,
                 text = paste0("<br> Country : ", country1,
                               "<br> WWTP : ", wwtp_name,
                               "<br> Treated wastewater discharged : ", waste_dis, " m3/d")
                 ),
             alpha = 0.8,
             size = 1.2
             ) + 

  rcartocolor::scale_colour_carto_c(palette = "SunsetDark") +
  rcartocolor::scale_fill_carto_d(palette = "SunsetDark") +
  theme_void() +
  theme(plot.background   = element_rect(fill = "grey15", color = "grey10"),
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white")
        )

ggplotly(forplotly_dis, tooltip = "text") %>% 
  layout(paper_bgcolor = "#202230",
         plot_bgcolor = "#202230")
