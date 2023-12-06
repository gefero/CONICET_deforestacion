library(sf)
library(tidyverse)

###
preds <- read_sf('./data/preds_con_lulc.geojson') %>%
        mutate(id = paste0("test_", id)) %>%
        rename(
                zona = provincia,
                year_00 = year_0,
               year_01 = year_1,
               year_02 = year_2,
               year_03 = year_3,
               year_04 = year_4,
               year_05 = year_5,
               year_06 = year_6,
               year_07 = year_7,
               year_08 = year_8,
               year_09 = year_9) %>%
        select(id, zona, year_00, year_01, year_02, year_03,
               year_04, year_05, year_06, year_07, year_08, year_09,
               year_10, year_11, year_12, year_13, year_14, year_15,
               year_16, year_17, year_18, year_19, model, b1, b1_clust, pred)
               

preds_chaco <- read_sf('./data/preds_chaco_seco_lulc.geojson') %>%
        pivot_longer(cols = c(prdctns_l, prdctns_r, prdctns_x),
                     names_to = "model",
                     values_to = "pred") %>%
        mutate(model = case_when(
                model == "prdctns_l" ~ "Regresion logística",
                model == "prdctns_r" ~ "Random Forest",
                model == "prdctns_x" ~ "XGB")
               ) %>%
        mutate(id = paste0("train_", id),
               zona = "chaco_seco") %>%
        rename(year_00 = year_0,
               year_01 = year_1,
               year_02 = year_2,
               year_03 = year_3,
               year_04 = year_4,
               year_05 = year_5,
               year_06 = year_6,
               year_07 = year_7,
               year_08 = year_8,
               year_09 = year_9) %>%
        select(id, zona, year_00, year_01, year_02, year_03,
               year_04, year_05, year_06, year_07, year_08, year_09,
               year_10, year_11, year_12, year_13, year_14, year_15,
               year_16, year_17, year_18, year_19, model, b1, b1_clust, 
               label, pred)

final <- preds_chaco %>%
        bind_rows(preds)

final <- final %>%
        mutate(pred=case_when(
                pred == 0 ~ 'No deforest. generico',
                pred == 1 ~ 'Deforestado',
                pred == 2 ~ 'No deforest. arbustal'
        ))

final <- final %>%
        filter(model != "Regresion logística")

preds %>%
        st_set_geometry(NULL) %>%
        group_by(b1_clust) %>%
        summarise(n=n())
        
final %>%
        st_set_geometry(NULL) %>%
        group_by(zona, model, pred) %>%
        summarise(n=n()) %>%
        mutate(perc=round(100*n/sum(n),2)) %>%
        ggplot() + 
                geom_col(aes(x=model, y=perc, fill=as.factor(pred))) +
                scale_fill_viridis_d() +
                facet_wrap(~zona) +
                labs(x="Modelo", 
                     y="%",
                     fill="Clase predicha") +
                theme_minimal()
        
preds_long <- final %>%
        st_set_geometry(NULL) %>%
        pivot_longer(starts_with("year_"),
                     names_to = "year",
                     values_to = "ndvi") %>%
        mutate(year = as.integer(str_remove(year, 'year_'))) 


preds_long %>%
        filter(model == "Random Forest") %>%
        ggplot() +
                geom_boxplot(aes(x=ndvi, y=pred, fill=pred), show.legend = FALSE) +
                facet_wrap(~zona+year) +
                theme_minimal()

ggsave('./outputs/test.pdf', width = 16, height = 9.02)

preds_long %>%
        filter(model == "XGB" & year==19) %>%
        ggplot() +
        geom_boxplot(aes(x=ndvi, y=pred, fill=pred), show.legend = FALSE) +
        facet_wrap(~zona+year) +
        theme_minimal()

preds_long %>%
        filter(model == "Random Forest") %>%
        ggplot() +
        geom_line(aes(x=year, y=ndvi, color=pred, group=id),
                  alpha=0.5,
                  show.legend = TRUE) +
        facet_wrap(~zona+pred) +
        theme_minimal()


preds_long %>%
        filter(model == "Random Forest" & year==19) %>%
        ggplot() + 
                geom_boxplot(aes(y=ndvi, x=zona))

preds_long %>%
        filter(model == "Random Forest" & year==19) %>%
        group_by(zona, pred) %>%
        summarise(mean=mean(ndvi),
                  sd = sd(ndvi),
                  median = median(ndvi),
                  mad = mad(ndvi),
                  n = n()
        ) %>%
        ungroup() %>%
        mutate(cv=sd/mean*100)
preds_long %>%
        filter(year == max(year)) %>%
        group_by(zona, model, pred) %>%
        summarise(across(ndvi, 
                         list(mean = mean, 
                              sd = sd,
                              median = median,
                              mad = mad))) %>%
        mutate(cv = ndvi_sd/ndvi_mean,
               cv_mad = ndvi_median/ndvi_mad)


plotly::ggplotly(
final %>%
        ggplot() + 
                geom_sf(aes(color=pred))
)
