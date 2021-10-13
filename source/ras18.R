# Rasmussen et al. 2018
# full dataset

# data wrangling
#####
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(corrplot)
library(readxl)

# set color palettes
warm <- "#BF812D"
cool <- "#80CDC1"
cold <- "#01665E"
granite <- "#9daba9"
andesite <- "#382dbf"
basalt <- "#bf382d"

# load data
ras18.min <- read_excel("/Users/jeff/sra-ts/data/external/sra_ras_sum/sierra_data_summary_2020.xlsx", 
                        sheet = "Data_Summary_2018_paper")
ras18.lyr <- read_excel("/Users/jeff/sra-ts/data/external/sra_ras_sum/sierra_data_summary_2020.xlsx", 
                        sheet = "2009_bulk_data")
ras18.frc <- read_excel("/Users/jeff/sra-ts/data/external/sra_ras_sum/sierra_data_summary_2020.xlsx", 
                        sheet = "2009_fraction_data")

# join data
nms <- names(ras18.min)[which(names(ras18.min) %in% names(ras18.lyr))]
nms <- nms[-which(nms == "ID_sra")]
ras18.min <- ras18.min[ , -which(names(ras18.min) %in% nms)]
ras18 <- type.convert(merge(ras18.lyr, ras18.min))
#####

# correlations
#####
## correlations
ras18$pm.num <- as.numeric(ras18$PM)
mins <- c(34, 84, 47:63, 67)
elem <- c(34, 84, 69:83)
sel.dis <- c(34, 84, 38:43)
cn.dep.tex.clim <- c(34, 84, 17, 18, 20:22, 31, 36, 37, 44:46)
corrplot(cor(ras18[ , mins], use = "pairwise.complete.obs"), method = "number",
         number.cex = .8, tl.cex = .8)
corrplot(cor(ras18[ , elem], use = "pairwise.complete.obs"), method = "number",
         number.cex = .8, tl.cex = .8)
corrplot(cor(ras18[ , sel.dis], use = "pairwise.complete.obs"), method = "number")
corrplot(cor(ras18[ , cn.dep.tex.clim], use = "pairwise.complete.obs"), method = "number")
#####

# plot set up
#####
min.nms <- names(ras18)[38:43]
key.nms <- names(ras18)[which(names(ras18) %in% c("pro_name", "Biome", "MAT (*C)", "Parent_Material", "bottom mineral", "top mineral", "Δ14C"))]
ras18_2 <- data.frame(
  ras18[ , key.nms],
  conc = c(ras18$`Fed (g/kg)`,
           ras18$`Feo (g/kg)`,
           ras18$`Alo (g/kg)`,
           ras18$`Alo+1/2Feo`,
           ras18$`Feo/Fed`,
           ras18$`Alp (g/kg)`),
  mins = rep(min.nms, each = nrow(ras18))) %>%
  rename(lyr_bot = `bottom.mineral`,
         lyr_top = `top.mineral`) %>%
  mutate(Biome = factor(Biome, levels = c("PP", "WF", "RF"), labels = c("warm", "cool", "cold")))
#####

# plot min data
##### 
ras18_2 %>%
  arrange(lyr_bot) %>%
  ggplot(., aes(conc, lyr_bot, color = Parent_Material)) +
  geom_point(aes(shape = Biome), size = 3) +
  geom_path(aes(linetype = Biome)) +
  scale_color_manual(values = c("Andesite" = andesite,
                                "Basalt" = basalt,
                                "Granite" = granite)) +
  scale_shape_manual(values = c("warm" = 15,
                                "cool" = 17,
                                "cold" = 16)) +
  scale_y_reverse() +
  facet_wrap(vars(mins), scales = "free") +
  ylab("depth (cm)") +
  xlab(NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
#####

# spline fits
#####
ras18.split <- split(ras18_2, ras18_2$mins)[c(1, 3:5)]
ras18.sp <- lapply(ras18.split, function(df) {
  ls <- lapply(split(df, df$pro_name), function(x) {
    depths(x) <- pro_name ~ lyr_top + lyr_bot
    x.mps <- mpspline(x, var.name = "conc", d = t(seq(0, 100, 10)))
    return(x.mps$var.std)
  })
  names(ls) <- unique(df$pro_name)
  return(ls)
})
names(ras18.sp) <- c("Al_ox", "Al_py", "Fe_dc", "Fe_ox")
ras18.sp.df <- data.frame(reduce(lapply(seq_along(ras18.sp), function(i) {
    df <- data.frame(t(bind_rows(ras18.sp[[i]])))
    names(df) <- unique(ras18$pro_name)
    df$depth <- rownames(df)
    return(df %>%
             pivot_longer(!depth, names_to = "pro_name", values_to = names(ras18.sp)[i]))
  }),
  left_join,
  by = c("depth", "pro_name")
))
ras18.sp.df <- ras18.sp.df[-which(ras18.sp.df$depth == "soil depth"), ]
ras18.sp.df$lyr_bot <- rep(seq(10, 100, 10), each = 9)
ras18.sp.df <- ras18.sp.df[complete.cases(ras18.sp.df), ]
ras18.sp.df$PM <- substr(ras18.sp.df$pro_name, 1, 2)
ras18.sp.df$ECO <- substr(ras18.sp.df$pro_name, 3, 4)

# join w/ d14c
ras.min.14c.sp.df <- merge(sra.ts.all.raw, ras18.sp.df)
#####

# plot min by 14C
#####
min.plot.fx <- function(df, Year, type, min) {
  quo_x <- sym(min)
  df %>%
    filter(year == Year & Type == type) %>%
    ggplot(., aes(!! quo_x, d14c)) +
    geom_point(aes(color = pm, shape = eco), size = 2) +
    scale_color_manual(values = c("andesite" = andesite,
                                  "basalt" = basalt,
                                  "granite" = granite)) +
    scale_shape_manual(values = c("warm" = 15,
                                  "cool" = 17,
                                  "cold" = 16)) +
    facet_wrap(vars(eco)) +
    ylab(expression(Delta*''^14*'C (‰)')) +
    xlab(min) +
    ggtitle(type) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())
}
mins <- names(ras.min.14c.sp.df)[11:14]
# bulk, 2019
lapply(seq_along(mins), function(i) {
  min.plot.fx(ras.min.14c.sp.df, 2019, "bulk", mins[i])
})
# inc, 2019
lapply(seq_along(mins), function(i) {
  min.plot.fx(ras.min.14c.sp.df, 2019, "inc", mins[i])
})
# bulk, 2001
lapply(seq_along(mins), function(i) {
  min.plot.fx(ras.min.14c.sp.df, 2001, "bulk", mins[i])
})
#####

# stats
#####
summary(lm(d14c ~ Al_ox * eco + lyr_bot, ras.min.14c.sp.df[ras.min.14c.sp.df$year == 2019, ]))
summary(lm(d14c ~ Al_py * eco + lyr_bot, ras.min.14c.sp.df[ras.min.14c.sp.df$year == 2019, ]))
summary(lm(d14c ~ Fe_ox * eco + lyr_bot, ras.min.14c.sp.df[ras.min.14c.sp.df$year == 2019, ]))
summary(lm(d14c ~ Fe_dc * eco + lyr_bot, ras.min.14c.sp.df[ras.min.14c.sp.df$year == 2019, ]))
#####

# bulk/inc dif vs. min
#####
sra.19.inc.blk.min <- ras18.sp.df %>%
  mutate(pm = ifelse(PM == "AN", "andesite", ifelse(PM == "BS", "basalt", "granite")),
         eco = ifelse(ECO == "pp", "warm", ifelse(ECO == "wf", "cool", "cold"))) %>%
  left_join(sra.19.inc.blk2, ., by = c("pm", "eco", "lyr_bot")) %>%
  mutate(bulk.inc = d14c_mean.inc - d14c_mean.bulk,
         MAST = ifelse(ECO == "pp", 13.6, ifelse(ECO == "wf", 9.1, 6.7))) %>%
  pivot_longer(cols = c("Al_py", "Al_ox", "Fe_ox", "Fe_dc"), names_to = "min", values_to = "conc")
# plot
sra.19.inc.blk.min %>%
  # filter(lyr_bot == 30) %>%
  ggplot(., aes(conc, bulk.inc)) +
  geom_point(aes(color = pm, shape = eco), size = 2) +
  scale_color_manual(values = c("andesite" = andesite,
                                "basalt" = basalt,
                                "granite" = granite)) +
  scale_shape_manual(values = c("warm" = 15,
                                "cool" = 17,
                                "cold" = 16)) +
  facet_wrap(vars(min), scales = "free") +
  ylab(expression('Bulk - Respired '*Delta*''^14*'C (‰)')) +
  xlab(expression('Concentration (g kg'^-1*')')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
# compare min relationships to just bulk 14C
sra.19.inc.blk.min %>%
  # filter(lyr_bot == 30) %>%
  ggplot(., aes(conc, d14c_mean.bulk)) +
  geom_point(aes(color = pm, shape = eco, size = lyr_bot/10)) +
  scale_color_manual(values = c("andesite" = andesite,
                                "basalt" = basalt,
                                "granite" = granite)) +
  scale_shape_manual(values = c("warm" = 15,
                                "cool" = 17,
                                "cold" = 16)) +
  facet_wrap(vars(min), scales = "free") +
  ylab(expression('Bulk soil '*Delta*''^14*'C (‰)')) +
  xlab(expression('Concentration (g kg'^-1*')')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
# and just respired 14C
sra.19.inc.blk.min %>%
  # filter(lyr_bot == 30) %>%
  ggplot(., aes(conc, d14c_mean.inc)) +
  geom_point(aes(color = pm, shape = eco), size = 3) +
  scale_color_manual(values = c("andesite" = andesite,
                                "basalt" = basalt,
                                "granite" = granite)) +
  scale_shape_manual(values = c("warm" = 15,
                                "cool" = 17,
                                "cold" = 16)) +
  facet_wrap(vars(min), scales = "free") +
  ylab(expression('Respired '*Delta*''^14*'C (‰)')) +
  xlab(expression('Concentration (g kg'^-1*')')) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
# ggsave("sra19.resp.min.pdf", p, device = cairo_pdf)

# stats
# note: no sig interaction of eco; lyr_bot additive, but not always sig and effect very small
lapply(split(sra.19.inc.blk.min, sra.19.inc.blk.min$min), function(df) {
  summary(lm(bulk.inc ~ conc + eco + lyr_bot, df))
})
lapply(split(sra.19.inc.blk.min, sra.19.inc.blk.min$min), function(df) {
  summary(lm(bulk.inc ~ conc * MAST, df))
})
# bulk 14C, for comparison
lapply(split(sra.19.inc.blk.min, sra.19.inc.blk.min$min), function(df) {
  df <- df[df$lyr_bot == 30, ]
  return(summary(lm(d14c_mean.bulk ~ conc + eco, df)))
})


# ISRaD parallel? Nope. But if we could get selective dissolution data for these samples...
db <- ISRaD.getdata("./Desktop", extra = TRUE)
inc.lyr <- merge(db$layer, db$incubation)
inc.lyr.14c <- inc.lyr[which(!is.na(inc.lyr$inc_14c) & !is.na(inc.lyr$lyr_14c)), 
                       c(1:4, 9:10, 57, 71:105, 136)]
pro.inc.lyr.14c <- merge(db$profile, inc.lyr.14c)
latlon <- pro.inc.lyr.14c[ , c("entry_name", "pro_lat", "pro_long")]
world <- map_data("world")
ggplot() +
  geom_polygon(data = world, aes(x = .data$long, y = .data$lat, group = .data$group), fill = NA, color = "Black") +
  geom_point(data = latlon, aes(pro_long, pro_lat), color = "red", size = 2, alpha = 0.5) +
  # geom_text(data = latlon, aes(pro_long, pro_lat, label = entry_name)) +
  theme_bw(base_size = 16) +
  labs(title = "inc/bulk 14C map", x = "Longitude", y = "Latitude")

# density data
sra.frc.19 <- read_excel("/Users/jeff/sra-frc/dat/derived/sra_density_summary.xlsx", sheet = "frc")
sra.frc.19$ECO <- factor(sra.frc.19$ECO, levels = c("pp", "wf", "rf"))
sra.frc.19 <- sra.frc.19[complete.cases(sra.frc.19$frc_c_perc), ]
ggplot(sra.frc.19, aes(PM, frc_c_org)) +
  geom_boxplot(aes(fill = PM)) +
  scale_fill_manual(values = c("AN" = andesite,
                               "BS" = basalt,
                               "GR" = granite)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
ggplot(sra.frc.19, aes(ECO, frc_c_org)) +
  geom_boxplot(aes(fill = ECO)) +
  scale_fill_manual(values = c("pp" = warm,
                               "wf" = cool,
                               "rf" = cold)) +
  facet_grid(cols = vars(lyr_bot)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

ggplot(sra.frc.19, aes(PM, frc_c_perc)) +
  geom_boxplot(aes(fill = PM)) +
  scale_fill_manual(values = c("AN" = andesite,
                               "BS" = basalt,
                               "GR" = granite)) +
  facet_grid(cols = vars(lyr_bot)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
ggplot(sra.frc.19, aes(ECO, frc_c_perc)) +
  geom_boxplot(aes(fill = ECO)) +
  scale_fill_manual(values = c("pp" = warm,
                               "wf" = cool,
                               "rf" = cold)) +
  facet_grid(cols = vars(lyr_bot)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
