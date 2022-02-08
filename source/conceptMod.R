# Conceptual mod: sra-ts

# What is the effect of PM and ECO on expected change over time in bulk and respired 14C?

# cols
granite <- "#9daba9"
andesite <- "#382dbf"
basalt <- "#bf382d"

# Calculate pct C frc as proxy for slow/fast
ras18.frc.sum <- ras18.frc %>% 
  select(PM, Biome, Pit, Depth, Fraction, `C (g C/g soil)`) %>%
  pivot_wider(names_from = Fraction, values_from = `C (g C/g soil)`) %>%
  mutate(lyrC_gCgS = rowSums(select(., OF, LF, MF))) %>%
  pivot_longer(cols = c(OF, LF, MF), names_to = "Fraction", values_to = "C (g C/g soil)") %>%
  mutate(frcPctC = (`C (g C/g soil)` / lyrC_gCgS) * 100,
         eco = ifelse(Biome == "RF", "cold", ifelse(Biome == "WF", "cool", "warm")),
         pm = ifelse(PM == "AN", "andesite", ifelse(PM == "BS", "basalt", "granite")),
         Fraction = factor(Fraction, levels = c("LF", "OF", "MF"))) %>%
  group_by(pm, eco, Fraction) %>%
  summarize(frcPctC_mean = mean(frcPctC))
# plot
ggplot(ras18.frc.sum, aes(pm, frcPctC_mean)) +
  geom_col(aes(fill = Fraction)) + 
  scale_fill_manual(values = c("MF" = "#0047af",
                               "LF" = "#3f9b00",
                               "OF" = "#9b003f")) + 
  facet_grid(cols = vars(eco)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


# pool slow: "slow" = OF + LF
ras18.frc.sum2 <- ras18.frc %>% 
  select(PM, Biome, Pit, Depth, Fraction, `C (g C/g soil)`) %>%
  pivot_wider(names_from = Fraction, values_from = `C (g C/g soil)`) %>%
  mutate(lyrC_gCgS = rowSums(select(., OF, LF, MF))) %>%
  pivot_longer(cols = c(OF, LF, MF), names_to = "Fraction", values_to = "C (g C/g soil)") %>%
  mutate(frcPctC = (`C (g C/g soil)` / lyrC_gCgS) * 100,
         eco = ifelse(Biome == "RF", "cold", ifelse(Biome == "WF", "cool", "warm")),
         pm = ifelse(PM == "AN", "andesite", ifelse(PM == "BS", "basalt", "granite"))) %>%
  select(!`C (g C/g soil)`) %>%
  pivot_wider(names_from = Fraction, values_from = frcPctC) %>%
  mutate(slow = rowSums(select(., OF, MF))) %>%
  rename(fast = LF) %>%
  pivot_longer(cols = c(slow, fast), names_to = "Pool", values_to = "frcPctC") %>%
  group_by(pm, eco, Pool) %>%
  summarize(frcPctC_mean = mean(frcPctC))
# plot
ggplot(ras18.frc.sum2, aes(pm, frcPctC_mean)) +
  geom_col(aes(fill = Pool)) + 
  scale_fill_manual(values = c("slow" = "#0047af",
                               "fast" = "#3f9b00")) + 
  facet_grid(cols = vars(eco)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


### 2ps mod

## constraints
# 14C: obs.bulk.14c, obs.resp.14c
# Stocks: csoc.19.0_30

# plot stocks
csoc.19.30cm.df <- bind_rows(csoc.19.0_30) %>% 
  group_by(PMeco) %>% 
  summarize(lyr_soc_30 = sum(lyr_soc))

# inputs
# Flux estimated from Goulden et al. 2012; Tang et al. 2005; Wang et al. 2000; Gaudinski 2000
# fluxes by elevation from GPP reported in Goulden et al. Fig. 5 and approximated
# Rh percentage from Tang et al. 2005 = 0.44 (ann. mean Blodgett); cf. 0.48 Harvard Forest
# A horizon est. 0.55 from Gaudinski
# assuming A = 0-30, assume 0-10 = 50%, 10-20 = 30%, 20-30 = 20% of total A production 
hznA.Rh.kgm2 <- 0.44 * 0.55 * 10^-3
gpp.ls <- c(1800, 1600, 1400)
in.frc.ls <- c(0.5, 0.3, 0.2)

# fx for calculating inputs
in.flx.fx <- function(PMeco_depth) {
  gpp <- ifelse(grepl("pp", PMeco_depth), gpp.ls[1], ifelse(grepl("wf", PMeco_depth), gpp.ls[2], gpp.ls[3]))
  in.frc <- ifelse(grepl("0-10", PMeco_depth), in.frc.ls[1], ifelse(grepl("10-20", PMeco_depth), in.frc.ls[2], in.frc.ls[3]))
  return(gpp * in.frc * hznA.Rh.kgm2)
}

# input list
in.est <- lapply(seq_along(obs.bulk.14c), function(i) {
  PMeco_depth <- names(obs.bulk.14c)[i]
  return(in.flx.fx(PMeco_depth))
})
names(in.est) <- names(obs.bulk.14c)

## mod setup
# cold sites
#####
## ANrf_10-20
PMeco_depth <- "ANrf_10-20"

# constraint df
con.df <- con.df.fx(PMeco_depth)
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]
ks <- c(.01, .002) # fast, slow
tc <- .2 # transfer coef
pars <- c(ks, tc)
lag <- 0

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps", lag = lag)

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars

# optimize
ANrf_10_20_2ps <- mod.fits.fx(mod = "2ps",
                              pars = pars.i.2ps,
                              sub = 5,
                              lag = lag,
                              In = in.est,
                              upper = c(1, 1, 1),
                              lower = c(0, 0, 0),
                              cost = "14C + cStock")
# plot
C14.2p.plot.fx(
  par.fx(pars = ANrf_10_20_2ps[[1]][[1]][["par"]], In = In, mod = "2ps"),
  con.df, mod = "2ps", PMeco_depth = PMeco_depth)

## BSrf_10-20
PMeco_depth <- "BSrf_10-20"
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]

# constraint df
con.df <- con.df.fx(PMeco_depth)

# pars
ks <- c(.12, .005) # fast, slow
tc <- .1 # transfer coef
pars <- c(ks, tc)
lag <- 0

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps", lag = lag, PMeco_depth = PMeco_depth)

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars

# optimize
BSrf_10_20_2ps <- mod.fits.fx(mod = "2ps",
                              pars = pars.i.2ps,
                              sub = 14,
                              lag = lag,
                              In = in.est,
                              upper = c(1, 1, 1),
                              lower = c(0, 0, 0),
                              cost = "14C + cStock")

# plot
C14.2p.plot.fx(
  par.fx(pars = BSrf_10_20_2ps[[1]][[1]][["par"]], In = In, mod = "2ps"),
  con.df, mod = "2ps", PMeco_depth = PMeco_depth)

## GRrf_10-20 **BULK ONLY OPT**
PMeco_depth <- "GRrf_10-20"
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]

# constraint df
con.df <- con.df.fx(PMeco_depth)

# pars
ks <- c(.25, .003) # fast, slow
tc <- .05 # transfer coef
pars <- c(ks, tc)
lag <- 0

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps", lag = lag)

# plot
C14.2p.plot.fx(ini.2ps.C14.df,
               con.df = con.df[con.df$pool == "bulk C", ],
               mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars

# optimize
GRrf_10_20_2ps <- mod.fits.fx(mod = "2ps",
                              pars = pars.i.2ps,
                              lag = lag,
                              sub = 23,
                              In = in.est,
                              upper = c(1, 1, 1),
                              lower = c(0, 0, 0),
                              cost = "14C bulk + cStock")
#####

# COLD plot optimized fits and calculate ages
#####
# ANrf_10-20, optimized
PMeco_depth <- "ANrf_10-20"
(ANrf.p <- C14.2p.plot.fx(
  par.fx(pars = ANrf_10_20_2ps[[1]][[1]][["par"]],
         In = in.est[["ANrf_10-20"]],
         mod = "2ps"),
  con.df.fx("ANrf_10-20"),
  mod = "2ps",
  PMeco_depth = "ANrf_10-20"))

# BSrf_10-20, optimized
PMeco_depth <- "BSrf_10-20"
(BSrf.p <- C14.2p.plot.fx(
  par.fx(pars = BSwf_10_20_2ps[[1]][[1]][["par"]],
         In = in.est[["BSrf_10-20"]],
         mod = "2ps",
         PMeco_depth = "BSrf_10-20"),
  con.df.fx("BSrf_10-20"),
  mod = "2ps",
  PMeco_depth = "BSrf_10-20"))

# GRrf_10-20, optimized
PMeco_depth <- "GRrf_10-20"
(GRrf.p <- C14.2p.plot.fx(
  par.fx(pars = GRwf_10_20_2ps[[1]][[1]][["par"]],
         In = in.est[["GRrf_10-20"]],
         mod = "2ps"),
  con.df.fx("GRrf_10-20")[con.df.fx("GRrf_10-20")$pool == "bulk C", ],
  mod = "2ps",
  PMeco_depth = "GRrf_10-20"))
#####

# cool sites
#####
## ANwf_10-20
PMeco_depth <- "ANwf_10-20"

# constraint df
con.df <- con.df.fx(PMeco_depth)
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]
ks <- c(.02, .001) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)
lag <- 0

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps", lag = lag)

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars

# optimize
ANwf_10_20_2ps <- mod.fits.fx(mod = "2ps",
                              pars = pars.i.2ps, 
                              sub = 8,
                              lag = lag,
                              In = in.est,
                              upper = c(1, 1, 1),
                              lower = c(0, 0, 0),
                              cost = "14C + cStock")
# plot
C14.2p.plot.fx(
  par.fx(pars = ANwf_10_20_2ps[[1]][[1]][["par"]], In = In, mod = "2ps"), 
  con.df, mod = "2ps", PMeco_depth = PMeco_depth)

## BSwf_10-20
PMeco_depth <- "BSwf_10-20"
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]

# constraint df
con.df <- con.df.fx(PMeco_depth)

# pars
ks <- c(.3, .003) # fast, slow
tc <- .05 # transfer coef 
pars <- c(ks, tc)
lag <- 0

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps", lag = lag)

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars

# optimize
BSwf_10_20_2ps <- mod.fits.fx(mod = "2ps",
                              pars = pars.i.2ps, 
                              sub = 17,
                              lag = lag,
                              In = in.est,
                              upper = c(1, 1, 1),
                              lower = c(0, 0, 0),
                              cost = "14C + cStock")

# plot
C14.2p.plot.fx(
  par.fx(pars = BSwf_10_20_2ps[[1]][[1]][["par"]], In = In, mod = "2ps"), 
  con.df, mod = "2ps", PMeco_depth = PMeco_depth)

## GRwf_10-20
PMeco_depth <- "GRwf_10-20"
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]

# constraint df
con.df <- con.df.fx(PMeco_depth)

# pars
ks <- c(.3, .005) # fast, slow
tc <- .05 # transfer coef 
pars <- c(ks, tc)
lag <- 0

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps", lag = lag)

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars

# optimize
GRwf_10_20_2ps <- mod.fits.fx(mod = "2ps",
                              pars = pars.i.2ps, 
                              lag = lag,
                              sub = 26,
                              In = in.est,
                              upper = c(1, 1, 1),
                              lower = c(0, 0, 0),
                              cost = "14C + cStock")
#####

# COOL plot optimized fits and calculate ages
#####
# ANwf_10-20, optimized
PMeco_depth <- "ANwf_10-20"
(ANwf.p <- C14.2p.plot.fx(
  par.fx(pars = ANwf_10_20_2ps[[1]][[1]][["par"]], 
         In = in.est[["ANwf_10-20"]], 
         mod = "2ps"), 
  con.df.fx("ANwf_10-20"), 
  mod = "2ps", 
  PMeco_depth = "ANwf_10-20"))

# BSwf_10-20, optimized
PMeco_depth <- "BSwf_10-20"
(BSwf.p <- C14.2p.plot.fx(
  par.fx(pars = BSwf_10_20_2ps[[1]][[1]][["par"]], 
         In = in.est[["BSwf_10-20"]], 
         mod = "2ps"), 
  con.df.fx("BSwf_10-20"), 
  mod = "2ps", 
  PMeco_depth = "BSwf_10-20"))

# GRwf_10-20, optimized
PMeco_depth <- "GRwf_10-20"
(GRwf.p <- C14.2p.plot.fx(
  par.fx(pars = GRwf_10_20_2ps[[1]][[1]][["par"]], 
         In = in.est[["GRwf_10-20"]], 
         mod = "2ps"), 
  con.df.fx("GRwf_10-20"), 
  mod = "2ps", 
  PMeco_depth = "GRwf_10-20"))
#####

# warm sites
#####
## ANpp_10-20
PMeco_depth <- "ANpp_10-20"
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]

# constraint df
con.df <- con.df.fx(PMeco_depth)

# pars
ks <- c(.2, .003) # fast, slow
tc <- .2 # transfer coef
pars <- c(ks, tc)
lag <- 0

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps", lag = lag)

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars

# optimize
ANpp_10_20_2ps <- mod.fits.fx(mod = "2ps",
                              pars = pars.i.2ps,
                              lag = 5,
                              sub = 2,
                              In = in.est,
                              upper = c(1, 1, 1),
                              lower = c(0, 0, 0),
                              cost = "14C + cStock")
# plot
C14.2p.plot.fx(
  par.fx(pars = ANpp_10_20_2ps[[1]][[1]][["par"]], In = In, mod = "2ps"),
  con.df, mod = "2ps", PMeco_depth = PMeco_depth)

## BSpp_10-20
PMeco_depth <- "BSpp_10-20"
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]

# constraint df
con.df <- con.df.fx(PMeco_depth)

# pars
ks <- c(.04, .006) # fast, slow
tc <- .4 # transfer coef
pars <- c(ks, tc)
lag <- 0

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps", lag = lag)

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars

# optimize
BSpp_10_20_2ps <- mod.fits.fx(mod = "2ps",
                              pars = pars.i.2ps,
                              lag = lag,
                              sub = 11,
                              In = in.est,
                              upper = c(1, 1, 1),
                              lower = c(0, 0, 0),
                              cost = "14C + cStock")

# plot
C14.2p.plot.fx(
  par.fx(pars = BSpp_10_20_2ps[[1]][[1]][["par"]], In = In, mod = "2ps"),
  con.df, mod = "2ps", PMeco_depth = PMeco_depth)

## GRpp_10-20
PMeco_depth <- "GRpp_10-20"
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]

# constraint df
con.df <- con.df.fx(PMeco_depth)

# pars
ks <- c(.04, .01) # fast, slow
tc <- .8 # transfer coef
pars <- c(ks, tc)
lag <- 0

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps", lag = lag)

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars

# optimize
GRpp_10_20_2ps <- mod.fits.fx(mod = "2ps",
                              pars = pars.i.2ps,
                              lag = lag,
                              sub = 20,
                              In = in.est,
                              upper = c(1, 1, 1),
                              lower = c(0, 0, 0),
                              cost = "14C + cStock")
#####

# WARM plot optimized fits and calculate ages
#####
# ANpp_10-20, optimized
PMeco_depth <- "ANpp_10-20"
(ANpp.p <- C14.2p.plot.fx(
  par.fx(pars = ANpp_10_20_2ps[[1]][[1]][["par"]],
         In = in.est[["ANpp_10-20"]],
         mod = "2ps"),
  con.df.fx("ANpp_10-20"),
  mod = "2ps",
  PMeco_depth = "ANpp_10-20"))

# BSpp_10-20, optimized
PMeco_depth <- "BSpp_10-20"
(BSpp.p <- C14.2p.plot.fx(
  par.fx(pars = BSpp_10_20_2ps[[1]][[1]][["par"]],
         In = in.est[["BSpp_10-20"]],
         mod = "2ps"),
  con.df.fx("BSpp_10-20"),
  mod = "2ps",
  PMeco_depth = "BSpp_10-20"))

# GRpp_10-20, optimized
PMeco_depth <- "GRpp_10-20"
(GRpp.p <- C14.2p.plot.fx(
  par.fx(pars = GRpp_10_20_2ps[[1]][[1]][["par"]],
         In = in.est[["GRpp_10-20"]],
         mod = "2ps"),
  con.df.fx("GRpp_10-20"),
  mod = "2ps",
  PMeco_depth = "GRpp_10-20"))
#####

# Pars
#####
par.out.fx <- function(pars) {round(c(pars[1:2]^-1, pars[3] * 100), 1)}

# cold
par.out.fx(ANrf_10_20_2ps[[1]][[1]][["par"]])
par.out.fx(BSrf_10_20_2ps[[1]][[1]][["par"]])
par.out.fx(GRrf_10_20_2ps[[1]][[1]][["par"]])

# cool
par.out.fx(ANwf_10_20_2ps[[1]][[1]][["par"]])
par.out.fx(BSwf_10_20_2ps[[1]][[1]][["par"]])
par.out.fx(GRwf_10_20_2ps[[1]][[1]][["par"]])

# warm
par.out.fx(ANpp_10_20_2ps[[1]][[1]][["par"]])
par.out.fx(BSpp_10_20_2ps[[1]][[1]][["par"]])
par.out.fx(GRpp_10_20_2ps[[1]][[1]][["par"]])

# look at identifiability
inden.df.fx <- function(ls, mod) {
  lapply(ls, function(x) {
    df <- collin(x)
    if (mod == "2pp") {
      df$ParCombo <- factor(c("k1 + k2", "k1 + gam", "k2 + gam", "k1 + k2 + gam"))
    } else {
      df$ParCombo <- factor(c("k1 + k2", "k1 + a21", "k2 + a21", "k1 + k2 + a21"))
    }
    return(df)
  })
}

# run for wf, pp
iden.ANwf <- inden.df.fx(lapply(ANwf_10_20_2ps, function(x) x[[2]]), mod = "2ps")
iden.BSwf <- inden.df.fx(lapply(BSwf_10_20_2ps, function(x) x[[2]]), mod = "2ps")
iden.GRwf <- inden.df.fx(lapply(GRwf_10_20_2ps, function(x) x[[2]]), mod = "2ps")
iden.wf.ls <- c(iden.ANwf, iden.BSwf, iden.GRwf)
names(iden.wf.ls) <- c("ANwf_10_20", "BSwf_10_20", "GRwf_10_20")

iden.ANpp <- inden.df.fx(lapply(ANpp_10_20_2ps, function(x) x[[2]]), mod = "2ps")
iden.BSpp <- inden.df.fx(lapply(BSpp_10_20_2ps, function(x) x[[2]]), mod = "2ps")
iden.GRpp <- inden.df.fx(lapply(GRpp_10_20_2ps, function(x) x[[2]]), mod = "2ps")
iden.pp.ls <- c(iden.ANpp, iden.BSpp, iden.GRpp)
names(iden.pp.ls) <- c("ANpp_10_20", "BSpp_10_20", "GRpp_10_20")

# identifiability plot function
coll.plot.fx <- function(df, mod, PMeco_depth, col.max) {
  ggplot(df, aes(N, log(collinearity), color = ParCombo)) +
    geom_hline(yintercept = log(20)) +
    geom_point(size = 3.5, position = position_dodge(width = .1)) +
    scale_y_continuous(limits = c(0, log(col.max))) +
    scale_x_continuous(limits = c(1.5, 3.5), breaks = c(2, 3)) +
    labs(title = paste(PMeco_depth, mod)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    if (mod == "2pp" | mod == "2pp + stock") {
      scale_color_manual(
        name = "Parameter combination",
        values = c("k1 + k2" = "#EF476F",
                   "k1 + gam" = "#FFD166",
                   "k2 + gam" = "#118AB2",
                   "k1 + k2 + gam" = "073B4C"))
    } else {
      scale_color_manual(
        name = "Parameter combination",
        values = c("k1 + k2" = "#EF476F",
                   "k1 + a21" = "#FFD166",
                   "k2 + a21" = "#118AB2",
                   "k1 + k2 + a21" = "073B4C"))
    }
}

# plot
lapply(seq_along(iden.wf.ls), function(i) {
  coll.plot.fx(iden.wf.ls[[i]], mod = "2ps", names(iden.wf.ls)[i], max(iden.wf.ls[[i]]["collinearity"]))
})
lapply(seq_along(iden.pp.ls), function(i) {
  coll.plot.fx(iden.pp.ls[[i]], mod = "2ps", names(iden.pp.ls)[i], max(iden.pp.ls[[i]]["collinearity"]))
})
#####

# Calculate ages
#####
# cold
ANrf_10_20_2ps_A <- -1 * diag(ANrf_10_20_2ps[[1]][[1]][["par"]][1:2])
ANrf_10_20_2ps_A[2, 1] <- ANrf_10_20_2ps[[1]][[1]][["par"]][1] * ANrf_10_20_2ps[[1]][[1]][["par"]][3]
ANrf_10_20_2ps_SA <- systemAge(A = ANrf_10_20_2ps_A,
                               u = c(in.est[["ANrf_10-20"]], 0),
                               a = seq(0,1000))
ANrf_10_20_2ps_TT <- transitTime(A = ANrf_10_20_2ps_A,
                                 u = c(in.est[["ANrf_10-20"]], 0),
                                 a = seq(0,1000))

BSrf_10_20_2ps_A <- -1 * diag(BSrf_10_20_2ps[[1]][[1]][["par"]][1:2])
BSrf_10_20_2ps_A[2, 1] <- BSrf_10_20_2ps[[1]][[1]][["par"]][1] * BSrf_10_20_2ps[[1]][[1]][["par"]][3]
BSrf_10_20_2ps_SA <- systemAge(A = BSrf_10_20_2ps_A,
                               u = c(in.est[["BSrf_10-20"]], 0),
                               a = seq(0,1000))
BSrf_10_20_2ps_TT <- transitTime(A = BSrf_10_20_2ps_A,
                                 u = c(in.est[["BSrf_10-20"]], 0),
                                 a = seq(0,1000))

GRrf_10_20_2ps_A <- -1 * diag(GRrf_10_20_2ps[[1]][[1]][["par"]][1:2])
GRrf_10_20_2ps_A[2, 1] <- GRrf_10_20_2ps[[1]][[1]][["par"]][1] * GRrf_10_20_2ps[[1]][[1]][["par"]][3]
GRrf_10_20_2ps_SA <- systemAge(A = GRrf_10_20_2ps_A,
                               u = c(in.est[["GRrf_10-20"]], 0),
                               a = seq(0,1000))
GRrf_10_20_2ps_TT <- transitTime(A = GRrf_10_20_2ps_A,
                                 u = c(in.est[["GRrf_10-20"]], 0),
                                 a = seq(0,1000))

# cool
ANwf_10_20_2ps_A <- -1 * diag(ANwf_10_20_2ps[[1]][[1]][["par"]][1:2])
ANwf_10_20_2ps_A[2, 1] <- ANwf_10_20_2ps[[1]][[1]][["par"]][1] * ANwf_10_20_2ps[[1]][[1]][["par"]][3] 
ANwf_10_20_2ps_SA <- systemAge(A = ANwf_10_20_2ps_A, 
                               u = c(in.est[["ANwf_10-20"]], 0),
                               a = seq(0,1000))
ANwf_10_20_2ps_TT <- transitTime(A = ANwf_10_20_2ps_A, 
                                 u = c(in.est[["ANwf_10-20"]], 0),
                                 a = seq(0,1000))

BSwf_10_20_2ps_A <- -1 * diag(BSwf_10_20_2ps[[1]][[1]][["par"]][1:2])
BSwf_10_20_2ps_A[2, 1] <- BSwf_10_20_2ps[[1]][[1]][["par"]][1] * BSwf_10_20_2ps[[1]][[1]][["par"]][3] 
BSwf_10_20_2ps_SA <- systemAge(A = BSwf_10_20_2ps_A, 
                               u = c(in.est[["BSwf_10-20"]], 0),
                               a = seq(0,1000))
BSwf_10_20_2ps_TT <- transitTime(A = BSwf_10_20_2ps_A, 
                                 u = c(in.est[["BSwf_10-20"]], 0),
                                 a = seq(0,1000))

GRwf_10_20_2ps_A <- -1 * diag(GRwf_10_20_2ps[[1]][[1]][["par"]][1:2])
GRwf_10_20_2ps_A[2, 1] <- GRwf_10_20_2ps[[1]][[1]][["par"]][1] * GRwf_10_20_2ps[[1]][[1]][["par"]][3] 
GRwf_10_20_2ps_SA <- systemAge(A = GRwf_10_20_2ps_A, 
                               u = c(in.est[["GRwf_10-20"]], 0),
                               a = seq(0,1000))
GRwf_10_20_2ps_TT <- transitTime(A = GRwf_10_20_2ps_A, 
                                 u = c(in.est[["GRwf_10-20"]], 0),
                                 a = seq(0,1000))

# warm
ANpp_10_20_2ps_A <- -1 * diag(ANpp_10_20_2ps[[1]][[1]][["par"]][1:2])
ANpp_10_20_2ps_A[2, 1] <- ANpp_10_20_2ps[[1]][[1]][["par"]][1] * ANpp_10_20_2ps[[1]][[1]][["par"]][3]
ANpp_10_20_2ps_SA <- systemAge(A = ANpp_10_20_2ps_A,
                               u = c(in.est[["ANpp_10-20"]], 0),
                               a = seq(0,1000))
ANpp_10_20_2ps_TT <- transitTime(A = ANpp_10_20_2ps_A,
                                 u = c(in.est[["ANpp_10-20"]], 0),
                                 a = seq(0,1000))

BSpp_10_20_2ps_A <- -1 * diag(BSpp_10_20_2ps[[1]][[1]][["par"]][1:2])
BSpp_10_20_2ps_A[2, 1] <- BSpp_10_20_2ps[[1]][[1]][["par"]][1] * BSpp_10_20_2ps[[1]][[1]][["par"]][3]
BSpp_10_20_2ps_SA <- systemAge(A = BSpp_10_20_2ps_A,
                               u = c(in.est[["BSwf_10-20"]], 0),
                               a = seq(0,1000))
BSpp_10_20_2ps_TT <- transitTime(A = BSpp_10_20_2ps_A,
                                 u = c(in.est[["BSpp_10-20"]], 0),
                                 a = seq(0,1000))

GRpp_10_20_2ps_A <- -1 * diag(GRpp_10_20_2ps[[1]][[1]][["par"]][1:2])
GRpp_10_20_2ps_A[2, 1] <- GRpp_10_20_2ps[[1]][[1]][["par"]][1] * GRpp_10_20_2ps[[1]][[1]][["par"]][3]
GRpp_10_20_2ps_SA <- systemAge(A = GRpp_10_20_2ps_A,
                               u = c(in.est[["GRpp_10-20"]], 0),
                               a = seq(0,1000))
GRpp_10_20_2ps_TT <- transitTime(A = GRpp_10_20_2ps_A,
                                 u = c(in.est[["GRpp_10-20"]], 0),
                                 a = seq(0,1000))

## SA and TT
# make lists
sa.ls <- list(ANrf_10_20_2ps_SA,
              BSrf_10_20_2ps_SA,
              GRrf_10_20_2ps_SA,
              ANwf_10_20_2ps_SA,
              BSwf_10_20_2ps_SA,
              GRwf_10_20_2ps_SA,
              ANpp_10_20_2ps_SA,
              BSpp_10_20_2ps_SA,
              GRpp_10_20_2ps_SA)
tt.ls <- list(ANrf_10_20_2ps_TT,
              BSrf_10_20_2ps_TT,
              GRrf_10_20_2ps_TT,
              ANwf_10_20_2ps_TT,
              BSwf_10_20_2ps_TT,
              GRwf_10_20_2ps_TT,
              ANpp_10_20_2ps_TT,
              BSpp_10_20_2ps_TT,
              GRpp_10_20_2ps_TT)

# name sites
nms <- c("ANrf_10-20",
         "BSrf_10-20",
         "GRrf_10-20",
         "ANwf_10-20",
         "BSwf_10-20",
         "GRwf_10-20",
         "ANpp_10-20",
         "BSpp_10-20",
         "GRpp_10-20")
names(sa.ls) <- nms
names(tt.ls) <- nms

# bind into df for plotting
sa.tt.df.fx <- function(ls, metric, pool = NULL) {
  bind_rows(
    lapply(seq_along(ls), function(i) {
      if (grepl("Density", metric)) {
        if (is.null(pool)) {
          df <- data.frame(density = ls[[i]][[metric]],
                           years = seq(1, length(ls[[i]][[metric]]))) 
        } else {
          df <- data.frame(density = ls[[i]][[metric]][ , pool],
                           years = seq(1, nrow(ls[[i]][[metric]])),
                           pool = ifelse(pool == 1, "fast", "slow"))
        }
      } else {
        df <- data.frame(years = ls[[i]][[metric]])
      }
      site <- names(ls)[i]
      df <- cbind(df,
                  metric = ifelse(grepl("Age", metric), "age", "transit time"),
                  ECO = factor(substr(site, 3, 4), levels = c("pp", "wf", "rf")),
                  PM = substr(site, 1, 2))
      return(df)
    }))
}
sa.tt.df <- rbind(sa.tt.df.fx(sa.ls, "meanSystemAge"), sa.tt.df.fx(tt.ls, "meanTransitTime"))
#####

# Plot ages
#####
# Plot ages
sa.tt.df %>%
  filter(metric == "age") %>%
  ggplot(., aes(PM, years, fill = PM)) +
  geom_col(position = "dodge") +
  scale_fill_manual(name = "Parent material",
                     values = c("AN" = andesite, 
                                "BS" = basalt, 
                                "GR" = granite)) +
  facet_grid(cols = vars(ECO)) +  
  ggtitle("System age") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Plot transit time
sa.tt.df %>%
  filter(metric != "system age") %>%
  ggplot(., aes(PM, years, fill = PM)) +
  geom_col(position = "dodge") +
  scale_fill_manual(name = "Parent material",
                    values = c("AN" = andesite, 
                               "BS" = basalt, 
                               "GR" = granite)) +
  facet_grid(cols = vars(ECO)) +
  ggtitle("Transit time") +
  theme_bw() +
  theme(panel.grid = element_blank())

# plot densities
sa.tt.dens.df <- rbind(sa.tt.df.fx(sa.ls, "systemAgeDensity"),
                       sa.tt.df.fx(tt.ls, "transitTimeDensity"))
sa.tt.dens.df %>%
  filter(metric == "age") %>%
  # filter(metric == "transit time") %>%
  ggplot(., aes(years, density, color = PM)) +
  # geom_vline(data = sa.tt.df, 
  #            aes(xintercept = years, color = site), linetype = 2, show.legend = FALSE) +
  geom_line(size = 1) +
  scale_color_manual(name = "Parent material",
                     values = c("AN" = andesite,
                                "BS" = basalt,
                                "GR" = granite),
                     labels = c("AN" = "Andesite",
                                "BS" = "Basalt",
                                "GR" = "Granite")) +
  facet_wrap(vars(ECO)) +
  coord_cartesian(xlim = c(0, 400), ylim = c(0, .04)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# pool ages
pa.dens.df <- bind_rows(lapply(seq_along(1:2), function(i) sa.tt.df.fx(sa.ls, "poolAgeDensity", pool = i)))
pa.mean.df <- data.frame(meanAge = c(ANwf_10_20_2ps_SA$meanPoolAge,
                                     BSwf_10_20_2ps_SA$meanPoolAge,
                                     GRwf_10_20_2ps_SA$meanPoolAge),
                         pool = rep(c("fast", "slow")),
                         site = rep(c("ANwf_10-20", "BSwf_10-20", "GRwf_10-20"), each = 2))
# fast & slow @ cool sites
pa.dens.df %>% 
  filter(pool == "slow" | (pool == "fast" & years < 25)) %>%
  filter(ECO == "wf") %>%
  ggplot(., aes(years, density, color = PM)) +
  # geom_vline(data = pa.mean.df, 
  #            aes(xintercept = meanAge, color = site), linetype = 2, show.legend = FALSE) +
  geom_line(size = 1) +
  scale_color_manual(name = "Parent material",
                     values = c("AN" = andesite,
                                "BS" = basalt,
                                "GR" = granite),
                     labels = c("AN" = "Andesite",
                                "BS" = "Basalt",
                                "GR" = "Granite")) +
  facet_wrap(vars(pool), scales = "free") +
  theme_bw() +
  theme(panel.grid = element_blank())

# fast C, all sites
pa.dens.df %>% 
  filter(pool == "fast" & years < 25) %>%
  ggplot(., aes(years, density, color = PM)) +
  # geom_vline(data = pa.mean.df, 
  #            aes(xintercept = meanAge, color = site), linetype = 2, show.legend = FALSE) +
  geom_line(size = 1) +
  scale_color_manual(name = "Parent material",
                     values = c("AN" = andesite,
                                "BS" = basalt,
                                "GR" = granite),
                     labels = c("AN" = "Andesite",
                                "BS" = "Basalt",
                                "GR" = "Granite")) +
  ggtitle("Fast pool age distributions") +
  facet_wrap(vars(ECO)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# slow C, all sites
pa.dens.df %>% 
  filter(pool == "slow") %>%
  ggplot(., aes(years, density, color = PM)) +
  # geom_vline(data = pa.mean.df, 
  #            aes(xintercept = meanAge, color = site), linetype = 2, show.legend = FALSE) +
  geom_line(size = 1) +
  scale_color_manual(name = "Parent material",
                     values = c("AN" = andesite,
                                "BS" = basalt,
                                "GR" = granite),
                     labels = c("AN" = "Andesite",
                                "BS" = "Basalt",
                                "GR" = "Granite")) +
  ggtitle("Slow pool age distributions") +
  facet_wrap(vars(ECO)) +
  theme_bw() +
  theme(panel.grid = element_blank())
#####

# Thought experiment for hypothesis testing 
#####
# fxs
dif.fx <- function(df) {dif <- function(x, pool) {
    round(x[which(x$years == 2001.5 & x$pool == pool), "d14C"] -
    x[which(x$years == 2019.5 & x$pool == pool), "d14C"], 0)}
  cat(paste("resp dif =", dif(df, "respiration"), "\n", "bulk dif =", dif(df, "bulk C")))
} # return resp, bulk difs
plot.fx <- function(df) {
  df %>%
    filter(pool != "fast" & pool != "slow") %>%
    ggplot(., aes(years, d14C, color = pool)) +
    geom_line() +
    scale_color_manual(
      name = "Pool",
      values = c("atm" = 8,
                 "bulk C" = "black",
                 "respiration" = "#FFC107")) +
    scale_x_continuous(limits = c(1950, 2022)) +
    theme_bw() +
    theme(panel.grid = element_blank())
} # plot d14C w/ time
dif.fx2 <- function(yr1 = 2019.5, yr2 = 2001.5, pars, mod, ix, pool.1 = "respiration", pool.2 = "bulk C") {
  if (is.null(yr2)) {
    dif <- function(df, pool) {
      round(df[which(df$years == yr1 & df$pool == pool), "d14C"] -
              df[which(df$years == yr1 & df$pool == "atm"), "d14C"])
    }
  } else {
    dif <- function(df, pool) {
      round(df[which(df$years == yr2 & df$pool == pool), "d14C"] -
              df[which(df$years == yr1 & df$pool == pool), "d14C"], 0)}
  }
  if (ix == 2) {
    v <- seq(.0005, .05, by = .001)
  } else {
    v <- seq(.02, 1, by = .02)
  }
  return(bind_rows(lapply(seq_along(v), function(i) {
    pars[ix] <- v[i]
    df <- modFun_2p(pars = pars, In = In, out = "out", mod = mod)
    dif.df <- data.frame(nm = pars[ix],
                         dif = c(dif(df, pool.1),
                                 dif(df, pool.2)),
                         pool = c(pool.1, pool.2),
                         row.names = NULL)
    names(dif.df)[1] <- names(pars)[ix]
    return(dif.df)})))
} 

# return df of difs varying one parameter 
plot.fx2 <- function(df, par, title) {
  ggplot(df, aes(!! sym(par), dif, color = pool)) +
    geom_line() +
    scale_color_manual(
      name = "Pool",
      values = c("atm" = 8,
                 "bulk C" = "black",
                 "respiration" = "#FFC107")) +
    ggtitle(title) +
    theme_bw() +
    theme(panel.grid = element_blank())
} # plot difs as function of varying parameter

# initial par set
pars <- c(k1 = .1, k2 = .005, tc = .1)

# k1.varying
k1.varying.2ps <- dif.fx2(pars = pars, mod = "2ps", ix = 1)
plot.fx2(k1.varying.2ps, "k1", "k1 varying, k2 = .005, tc = 0.1")
k1.varying.2pp <- dif.fx2(pars = pars, mod = "2pp", ix = 1)
plot.fx2(k1.varying.2pp, "k1", "k1 varying, k2 = .005, gam = 0.1")

# dd
k1.varying.2ps.dd <- dif.fx2(yr1 = 2019.5, yr2 = NULL, pars, mod = "2ps", ix = 1)
plot.fx2(k1.varying.2ps.dd, "k1", "2ps ∆∆14C 2019: k1 varying, k2 = .005, tc = 0.1")
k1.varying.2pp.dd <- dif.fx2(yr1 = 2019.5, yr2 = NULL, pars, mod = "2pp", ix = 1)
plot.fx2(k1.varying.2pp, "k1", "2pp ∆∆14C 2019: k1 varying, k2 = .005, tc = 0.1")

# k2 varying
k2.varying <- dif.fx2(pars = pars, mod = "2ps", ix = 2)
plot.fx2(k2.varying, "k2", "k2 varying, k1 = .1, tc = 0.1")

# tc varying
tc.varying <- dif.fx2(pars = pars, mod = "2ps", ix = 3)
plot.fx2(tc.varying, "tc", "tc varying, k1 = .1, k2 = .005")

## vary set pars
# par set k1 lower
pars.k1.01_tc.1 <- c(k1 = .01, k2 = .005, tc = .1)
pars.k1.01_tc.8 <- c(k1 = .01, k2 = .005, tc = .8)
# par set k1 upper
pars.k1.5_tc.1 <- c(k1 = .5, k2 = .005, tc = .1)
pars.k1.5_tc.8 <- c(k1 = .5, k2 = .005, tc = .8)

# par set k2 upper
pars.k2.01_tc.1 <- c(k1 = .1, k2 = .01, tc = .1)
pars.k2.01_tc.8 <- c(k1 = .1, k2 = .01, tc = .8)
# par set k2 lower
pars.k2.001_tc.1 <- c(k1 = .1, k2 = .001, tc = .1)
pars.k2.001_tc.8 <- c(k1 = .1, k2 = .001, tc = .8)

# par set tc upper
pars.tc.8 <- c(k1 = .1, k2 = .005, tc = .8)

# k1k2 combinations
pars.k1.1_k2.001 <- c(k1 = .1, k2 = .001, tc = .1)
pars.k1.1_k2.01 <- c(k1 = .1, k2 = .01, tc = .1)
pars.k1.01_k2.005 <- c(k1 = .01, k2 = .005, tc = .1)
pars.k1.5_k2.005 <- c(k1 = .5, k2 = .005, tc = .1)
pars.k1.01_k2.001 <- c(k1 = .01, k2 = .001, tc = .1)
pars.k1.5_k2.001 <- c(k1 = .5, k2 = .001, tc = .1)
pars.k1.01_k2.01 <- c(k1 = .01, k2 = .01, tc = .1)
pars.k1.5_k2.01 <- c(k1 = .5, k2 = .01, tc = .1)

# k1 varying
k1.varying.tc.8 <- dif.fx2(pars = pars.tc.8, mod = "2ps", ix = 1)
k1.varying.k2.001_tc.1 <- dif.fx2(pars = pars.k1.01_tc.1, mod = "2ps", ix = 1)
k1.varying.k2.001_tc.8 <- dif.fx2(pars = pars.k2_L_tc.8, mod = "2ps", ix = 1)
k1.varying.k2.01_tc.1 <- dif.fx2(pars = pars.k2_U_tc.1, mod = "2ps", ix = 1)
k1.varying.k2.01_tc.8 <- dif.fx2(pars = pars.k2_U_tc.8, mod = "2ps", ix = 1)

k1.varying.tc.8.p <- dif.fx2(pars = pars.tc.8, mod = "2pp", ix = 1)
k1.varying.k2.001_tc.1.p <- dif.fx2(pars = pars.k1.01_tc.1, mod = "2pp", ix = 1)
k1.varying.k2.001_tc.8.p <- dif.fx2(pars = pars.k2_L_tc.8, mod = "2pp", ix = 1)
k1.varying.k2.01_tc.1.p <- dif.fx2(pars = pars.k2_U_tc.1, mod = "2pp", ix = 1)
k1.varying.k2.01_tc.8.p <- dif.fx2(pars = pars.k2_U_tc.8, mod = "2pp", ix = 1)

k1.varying.2ps.df <- rbind(k1.varying.2ps,
                           k1.varying.tc.8,
                           k1.varying.k2.001_tc.1, 
                           k1.varying.k2.001_tc.8,
                           k1.varying.k2.01_tc.1,
                           k1.varying.k2.01_tc.8)
k1.varying.2pp.df <- rbind(k1.varying.2pp,
                        k1.varying.tc.8.p,
                        k1.varying.k2.001_tc.1.p, 
                        k1.varying.k2.001_tc.8.p,
                        k1.varying.k2.01_tc.1.p,
                        k1.varying.k2.01_tc.8.p)

k1.varying.2pp.plot <- k1.varying.2pp.df %>%
  mutate(scenario = factor(rep(c("k2.005, g.1",
                                 "k2.005, g.8",
                                 "k2.001, g.1", 
                                 "k2.001, g.8", 
                                 "k2.01, g.1",
                                 "k2.01, g.8"),
                               each = 100),
                           levels = c("k2.001, g.8",
                                      "k2.005, g.8",
                                      "k2.01, g.8", 
                                      "k2.001, g.1", 
                                      "k2.005, g.1",
                                      "k2.01, g.1"))) %>%
  ggplot(., aes(k1, dif, color = pool)) +
  geom_line() +
  scale_color_manual(
    name = "Pool",
    values = c("atm" = 8,
               "bulk C" = "black",
               "respiration" = "#FFC107")) +
  facet_wrap(vars(scenario), ncol = 3) +
  scale_x_continuous(breaks = seq(.1, .9, by = .2)) +
  ylab(expression(''*Delta*''^14*'C difference, 2001 - 2019 (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())

k1.varying.2ps.plot <- k1.varying.2ps.df %>%
  mutate(scenario = factor(rep(c("k2.005, tc.1",
                                 "k2.005, tc.8",
                                 "k2.001, tc.1", 
                                 "k2.001, tc.8", 
                                 "k2.01, tc.1",
                                 "k2.01, tc.8"),
                               each = 100),
                           levels = c("k2.001, tc.8",
                                      "k2.005, tc.8",
                                      "k2.01, tc.8", 
                                      "k2.001, tc.1", 
                                      "k2.005, tc.1",
                                      "k2.01, tc.1"))) %>%
  ggplot(., aes(k1, dif, color = pool)) +
  geom_line() +
  scale_color_manual(
    name = "Pool",
    values = c("atm" = 8,
               "bulk C" = "black",
               "respiration" = "#FFC107")) +
  facet_wrap(vars(scenario), ncol = 3) +
  scale_x_continuous(breaks = seq(.1, .9, by = .2)) +
  ylab(expression(''*Delta*''^14*'C difference, 2001 - 2019 (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(
  "k1.varying.2ps.png",
  plot = k1.varying.2ps.plot,
  dpi = "print")
  
# k2 varying
k2.varying.2ps <- dif.fx2(pars = pars, mod = "2ps", ix = 2)
k2.varying.tc.8 <- dif.fx2(pars = pars.tc.8, mod = "2ps", ix = 2)
k2.varying.k1.01_tc.1 <- dif.fx2(pars = pars.k1.01_tc.1, mod = "2ps", ix = 2)
k2.varying.k1.01_tc.8 <- dif.fx2(pars = pars.k1.01_tc.8, mod = "2ps", ix = 2)
k2.varying.k1.5_tc.1 <- dif.fx2(pars = pars.k1.5_tc.1, mod = "2ps", ix = 2)
k2.varying.k1.5_tc.8 <- dif.fx2(pars = pars.k1.5_tc.8, mod = "2ps", ix = 2)

k2.varying.2pp <- dif.fx2(pars = pars, mod = "2pp", ix = 2)
k2.varying.tc.8.2pp <- dif.fx2(pars = pars.tc.8, mod = "2pp", ix = 2)
k2.varying.k1.01_tc.1.2pp <- dif.fx2(pars = pars.k1.01_tc.1, mod = "2pp", ix = 2)
k2.varying.k1.01_tc.8.2pp <- dif.fx2(pars = pars.k1.01_tc.8, mod = "2pp", ix = 2)
k2.varying.k1.5_tc.1.2pp <- dif.fx2(pars = pars.k1.5_tc.1, mod = "2pp", ix = 2)
k2.varying.k1.5_tc.8.2pp <- dif.fx2(pars = pars.k1.5_tc.8, mod = "2pp", ix = 2)

k2.varying.2pp.df <- rbind(k2.varying.2pp,
                           k2.varying.tc.8.2pp,
                           k2.varying.k1.01_tc.1.2pp, 
                           k2.varying.k1.01_tc.8.2pp,
                           k2.varying.k1.5_tc.1.2pp,
                           k2.varying.k1.5_tc.8.2pp)
k2.varying.2pp.plot <- k2.varying.2pp.df %>%
  mutate(scenario = factor(rep(c("k1.1, g.1",
                                 "k1.1, g.8",
                                 "k1.01, g.1", 
                                 "k1.01, g.8", 
                                 "k1.5, g.1",
                                 "k1.5, g.8"),
                               each = 100),
                           levels = c("k1.01, g.8",
                                      "k1.1, g.8",
                                      "k1.5, g.8", 
                                      "k1.01, g.1", 
                                      "k1.1, g.1",
                                      "k1.5, g.1"))) %>%
  ggplot(., aes(k2, dif, color = pool)) +
  geom_line() +
  scale_color_manual(
    name = "Pool",
    values = c("bulk C" = "black",
               "respiration" = "#FFC107")) +
  facet_wrap(vars(scenario), ncol = 3, scales = "free") +
  scale_x_continuous(breaks = seq(.005, .045, by = .02)) +
  ylab(expression(''*Delta*''^14*'C difference, 2001 - 2019 (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())

k2.varying.2ps.df <- rbind(k2.varying.2ps,
                         k2.varying.tc.8,
                         k2.varying.k1.01_tc.1, 
                         k2.varying.k1.01_tc.8,
                         k2.varying.k1.5_tc.1,
                         k2.varying.k1.5_tc.8) 
k2.varying.2ps.plot <- k2.varying.2ps.df %>%
  mutate(scenario = factor(rep(c("k1.1, tc.1",
                                 "k1.1, tc.8",
                                 "k1.01, tc.1", 
                                 "k1.01, tc.8", 
                                 "k1.5, tc.1",
                                 "k1.5, tc.8"),
                               each = 100),
                           levels = c("k1.01, tc.8",
                                      "k1.1, tc.8",
                                      "k1.5, tc.8", 
                                      "k1.01, tc.1", 
                                      "k1.1, tc.1",
                                      "k1.5, tc.1"))) %>%
  ggplot(., aes(k2, dif, color = pool)) +
  geom_line() +
  scale_color_manual(
    name = "Pool",
    values = c("bulk C" = "black",
               "respiration" = "#FFC107")) +
  facet_wrap(vars(scenario), ncol = 3, scales = "free") +
  scale_x_continuous(breaks = seq(.005, .045, by = .02)) +
  ylab(expression(''*Delta*''^14*'C difference, 2001 - 2019 (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(
  "k2.varying.2ps.png",
  plot = k2.varying.2ps.plot,
  dpi = "print")

# tc varying
tc.varying.2ps <- dif.fx2(pars = pars, mod = "2ps", ix = 3) # 0, 0 
tc.varying.k1.1_k2.001 <- dif.fx2(pars = pars.k1.1_k2.001, mod = "2ps", ix = 3) # 0, -
tc.varying.k1.1_k2.01 <- dif.fx2(pars = pars.k1.1_k2.01, mod = "2ps", ix = 3) # 0, +
tc.varying.k1.01_k2.005 <- dif.fx2(pars = pars.k1.01_k2.005, mod = "2ps", ix = 3) # -, 0
tc.varying.k1.5_k2.005 <- dif.fx2(pars = pars.k1.5_k2.005, mod = "2ps", ix = 3) # +, 0
tc.varying.k1.01_k2.001 <- dif.fx2(pars = pars.k1.01_k2.001, mod = "2ps", ix = 3) # -, -
tc.varying.k1.5_k2.001 <- dif.fx2(pars = pars.k1.5_k2.001, mod = "2ps", ix = 3) # +, -
tc.varying.k1.01_k2.01 <- dif.fx2(pars = pars.k1.01_k2.01, mod = "2ps", ix = 3) # -, +
tc.varying.k1.5_k2.01 <- dif.fx2(pars = pars.k1.5_k2.01, mod = "2ps", ix = 3) # +, +

tc.varying.2pp <- dif.fx2(pars = pars, "2pp", 3) # 0, 0 
tc.varying.k1.1_k2.001.2pp <- dif.fx2(pars = pars.k1.1_k2.001, mod = "2pp", ix = 3) # 0, -
tc.varying.k1.1_k2.01.2pp <- dif.fx2(pars = pars.k1.1_k2.01, mod = "2pp", ix = 3) # 0, +
tc.varying.k1.01_k2.005.2pp <- dif.fx2(pars = pars.k1.01_k2.005, mod = "2pp", ix = 3) # -, 0
tc.varying.k1.5_k2.005.2pp <- dif.fx2(pars = pars.k1.5_k2.005, mod = "2pp", ix = 3) # +, 0
tc.varying.k1.01_k2.001.2pp <- dif.fx2(pars = pars.k1.01_k2.001, mod = "2pp", ix = 3) # -, -
tc.varying.k1.5_k2.001.2pp <- dif.fx2(pars = pars.k1.5_k2.001, mod = "2pp", ix = 3) # +, -
tc.varying.k1.01_k2.01.2pp <- dif.fx2(pars = pars.k1.01_k2.01, mod = "2pp", ix = 3) # -, +
tc.varying.k1.5_k2.01.2pp <- dif.fx2(pars = pars.k1.5_k2.01, mod = "2pp", ix = 3) # +, +

tc.varying.2pp.df <- rbind(tc.varying.2pp, 
                         tc.varying.k1.1_k2.001.2pp, 
                         tc.varying.k1.1_k2.01.2pp, 
                         tc.varying.k1.01_k2.005.2pp, 
                         tc.varying.k1.5_k2.005.2pp,
                         tc.varying.k1.01_k2.001.2pp,
                         tc.varying.k1.5_k2.001.2pp,
                         tc.varying.k1.01_k2.01.2pp,
                         tc.varying.k1.5_k2.01.2pp) 
tc.varying.2pp.plot <-  tc.varying.2pp.df %>% 
  mutate(scenario = factor(
    rep(c("k1.1, k2.005",
          "k1.1, k2.001",
          "k1.1, k2.01",
          "k1.01, k2.005", 
          "k1.5, k2.005",
          "k1.01, k2.001",
          "k1.5, k2.001",
          "k1.01, k2.01",
          "k1.5, k2.01"), each = 100))) %>%
  ggplot(., aes(tc, dif, color = pool)) +
  geom_line() +
  scale_color_manual(
    name = "Pool",
    values = c("atm" = 8,
               "bulk C" = "black",
               "respiration" = "#FFC107")) +
  facet_wrap(vars(scenario), scales = "free") +
  scale_x_continuous(breaks = c(.1, .5, .9)) +
  ylab(expression(''*Delta*''^14*'C difference, 2001 - 2019 (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(
  "tc.varying.2pp.png",
  plot = tc.varying.plot,
  dpi = "print")

tc.varying.2ps.df <- rbind(tc.varying.2ps, 
                           tc.varying.k1.1_k2.001, 
                           tc.varying.k1.1_k2.01, 
                           tc.varying.k1.01_k2.005, 
                           tc.varying.k1.5_k2.005,
                           tc.varying.k1.01_k2.001,
                           tc.varying.k1.5_k2.001,
                           tc.varying.k1.01_k2.01,
                           tc.varying.k1.5_k2.01) 
tc.varying.2ps.plot <-  tc.varying.2ps.df %>% 
  mutate(scenario = factor(
    rep(c("k1.1, k2.005",
          "k1.1, k2.001",
          "k1.1, k2.01",
          "k1.01, k2.005", 
          "k1.5, k2.005",
          "k1.01, k2.001",
          "k1.5, k2.001",
          "k1.01, k2.01",
          "k1.5, k2.01"), each = 100))) %>%
  ggplot(., aes(tc, dif, color = pool)) +
  geom_line() +
  scale_color_manual(
    name = "Pool",
    values = c("atm" = 8,
               "bulk C" = "black",
               "respiration" = "#FFC107")) +
  facet_wrap(vars(scenario), scales = "free") +
  # scale_x_continuous(breaks = c(.1, .5, .9)) +
  scale_x_continuous(limits = c(.01, .21, by = .5)) +
  ylab(expression(''*Delta*''^14*'C difference, 2001 - 2019 (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(
    "tc.varying.2ps.png",
    plot = tc.varying.plot,
    dpi = "print")

#####
# misc
#####
# Ti:Zr ratio
sra.09.sum %>%
  mutate(TiZr = (Ti * 100) / `Zr (ppm)`,
         SiFe_dc = Fe / Si) %>%
  ggplot(., aes(TiZr, lyr_bot)) +
  geom_point(aes(color = pm, shape = eco), size = 3) +
  scale_color_manual(name = "parent material",
                     values = c("andesite" = "blue", 
                                "basalt" = "red", 
                                "granite" = "darkgray")) +
  scale_shape_manual(name = "ecosystem",
                     values = c("warm" = 15, 
                                "cool" = 17, 
                                "cold" = 16)) +
  scale_y_reverse() +
  facet_grid(cols = vars(pm)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

