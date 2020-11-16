## 2-pool series model initial parameter search script
# Requires the following functions: con.df.fx; par.fx; C14.2p.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c
# These functions and inputs are in Rmd file "sra-ts.RMD" (this directory)

# Initialize list (9 sites by 3 depths = 27 elements)
pars.i.2ps <- lapply(seq_len(27), function(df) {
  df <- data.frame(kfast = NA,
                   kslow = NA,
                   gam = NA)
})
names(pars.i.2ps) <- names(obs.bulk.14c)

# ANpp_0-10
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.2, .02) # fast, slow
a21 <- .0009 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21)
#####

# ANpp-10-20
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.15, .009) # fast, slow
a21 <- .001 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# ANpp-20-30
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.2, .002) # fast, slow
a21 <- .01 

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# ANrf-0-10
#####
# Site/depth info
PMeco <- "ANrf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.14, .012) # fast, slow
a21 <- .01 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# ANrf-10-20
#####
# Site/depth info
PMeco <- "ANrf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.15, .006) # fast, slow
a21 <- .01 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# ANrf-20-30
#####
# Site/depth info
PMeco <- "ANrf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.08, .0026) # fast, slow
a21 <- .01 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# ANwf-0-10
#####
# Site/depth info
PMeco <- "ANwf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.08, .004) # fast, slow
a21 <- .01 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# ANwf-10-20
#####
# Site/depth info
PMeco <- "ANwf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.08, .002) # fast, slow
a21 <- .001 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# ANwf-20-30
#####
# Site/depth info
PMeco <- "ANwf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.07, .002) # fast, slow
a21 <- .008 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# BSpp-0-10
#####
# Site/depth info
PMeco <- "BSpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.07, .011) # fast, slow
a21 <- .01 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# BSpp-10-20
#####
# Site/depth info
PMeco <- "BSpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.1, .01) # fast, slow
a21 <- .01 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# BSpp-20-30
#####
# Site/depth info
PMeco <- "BSpp"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .0025) # fast, slow
a21 <- .7 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# BSrf-0-10
#####
# Site/depth info
PMeco <- "BSrf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.12, .0065) # fast, slow
a21 <- .03 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# BSrf-10-20
#####
# Site/depth info
PMeco <- "BSrf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.16, .004) # fast, slow
a21 <- .85 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# BSrf-20-30
#####
# Site/depth info
PMeco <- "BSrf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.14, .002) # fast, slow
a21 <- .85 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# BSwf-0-10
#####
# Site/depth info
PMeco <- "BSwf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.18, .013) # fast, slow
a21 <- .002 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# BSwf-10-20
#####
# Site/depth info
PMeco <- "BSwf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .0023) # fast, slow
a21 <- .95 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# BSwf-20-30
#####
# Site/depth info
PMeco <- "BSwf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .001) # fast, slow
a21 <- .95 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# GRpp-0-10
#####
# Site/depth info
PMeco <- "GRpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.11, .019) # fast, slow
a21 <- .002 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# GRpp-10-20
#####
# Site/depth info
PMeco <- "GRpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.08, .006) # fast, slow
a21 <- .7 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# GRpp-20-30
#####
# Site/depth info
PMeco <- "GRpp"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.2, .0044) # fast, slow
a21 <- .8 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# GRrf-0-10
#####
# Site/depth info
PMeco <- "GRrf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.11, .009) # fast, slow
a21 <- .01 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# GRrf-10-20
#####
# Site/depth info
PMeco <- "GRrf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.12, .0042) # fast, slow
a21 <- .8 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21) 
#####

# GRrf-20-30
#####
# Site/depth info
PMeco <- "GRrf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .0022) # fast, slow
a21 <- .7 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21)
#####

# GRwf-0-10
#####
# Site/depth info
PMeco <- "GRwf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.11, .015) # fast, slow
a21 <- .002 # transfer coef

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21)
#####

# GRwf-10-20
#####
# Site/depth info
PMeco <- "GRwf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.2, .003) # fast, slow
a21 <- .95 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21)
#####

# GRwf-20-30
#####
# Site/depth info
PMeco <- "GRwf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .0025) # fast, slow
a21 <- .95 # transfer coef, fast to slow

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, a21), In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2ps")

# save pars
pars.i.2ps[[PMeco_depth]] <- c(ks, a21)


#####

# Save output
save(pars.i.2ps, file = paste0("~/sra-ts/data/derived/modFit_pars/", "pars.i.2ps", "_", Sys.Date(), ".Rdata"))
