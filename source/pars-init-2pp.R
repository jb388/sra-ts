## 2-pool parallel model initial parameter search script
# Requires the following functions: con.df.fx; par.fx; C14.2p.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c
# These functions and inputs are in Rmd file "sra-ts.RMD" (this directory)

# Notes:
# 1) gam parameter affects both shape of the bulk 14C curve and carbon stocks
# 2) k1 parameter fits the respiration
# 3) k2 fits the bulk 14C

# changed to constant inputs by eco/depth on 10-Dec-2020

# Initialize list (9 sites by 3 depths = 27 elements)
pars.i.2pp <- lapply(seq_len(27), function(df) {
  df <- data.frame(kfast = NA,
                   kslow = NA,
                   gam = NA)
})
names(pars.i.2pp) <- names(obs.bulk.14c)

# ANpp_0-10
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.18, .007) # fast, slow
gam <- .95 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam)
#####

# ANpp-10-20
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.35, .0025) # fast, slow
gam <- .7 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# ANpp-20-30
#####
# Site/depth info
PMeco <- "ANpp"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.01, .002) # fast, slow
gam <- .5 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# ANrf-0-10
#####
# Site/depth info
PMeco <- "ANrf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.32, .0065) # fast, slow
gam <- .92 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# ANrf-10-20
#####
# Site/depth info
PMeco <- "ANrf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.1, .003) # fast, slow
gam <- .4 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# ANrf-20-30
#####
# Site/depth info
PMeco <- "ANrf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.015, .0017) # fast, slow
gam <- .4 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# ANwf-0-10
#####
# Site/depth info
PMeco <- "ANwf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.04, .0021) # fast, slow
gam <- .5 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# ANwf-10-20
#####
# Site/depth info
PMeco <- "ANwf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.02, .0015) # fast, slow
gam <- .7 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# ANwf-20-30
#####
# Site/depth info
PMeco <- "ANwf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.02, .0011) # fast, slow
gam <- .7 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# BSpp-0-10
#####
# Site/depth info
PMeco <- "BSpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.025, .005) # fast, slow
gam <- .6 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# BSpp-10-20
#####
# Site/depth info
PMeco <- "BSpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.015, .0044) # fast, slow
gam <- .5 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# BSpp-20-30
#####
# Site/depth info
PMeco <- "BSpp"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.008, .0025) # fast, slow
gam <- .5 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# BSrf-0-10
#####
# Site/depth info
PMeco <- "BSrf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.25, .005) # fast, slow
gam <- .7 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# BSrf-10-20
#####
# Site/depth info
PMeco <- "BSrf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.18, .004) # fast, slow
gam <- .75 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# BSrf-20-30
#####
# Site/depth info
PMeco <- "BSrf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.15, .003) # fast, slow
gam <- .7 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# BSwf-0-10
#####
# Site/depth info
PMeco <- "BSwf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .005) # fast, slow
gam <- .8 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# BSwf-10-20
#####
# Site/depth info
PMeco <- "BSwf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.35, .004) # fast, slow
gam <- .7 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# BSwf-20-30
#####
# Site/depth info
PMeco <- "BSwf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.4, .002) # fast, slow
gam <- .8 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# GRpp-0-10
#####
# Site/depth info
PMeco <- "GRpp"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.09, .009) # fast, slow
gam <- .75 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# GRpp-10-20
#####
# Site/depth info
PMeco <- "GRpp"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.01, .008) # fast, slow
gam <- .5 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# GRpp-20-30
#####
# Site/depth info
PMeco <- "GRpp"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.01, .0044) # fast, slow
gam <- .5 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# GRrf-0-10
#####
# Site/depth info
PMeco <- "GRrf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.008, .0042) # fast, slow
gam <- .8 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# GRrf-10-20
#####
# Site/depth info
PMeco <- "GRrf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.12, .0042) # fast, slow
gam <- .8 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam) 
#####

# GRrf-20-30
#####
# Site/depth info
PMeco <- "GRrf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .0022) # fast, slow
gam <- .7 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam)
#####

# GRwf-0-10
#####
# Site/depth info
PMeco <- "GRwf"
lyr_bot <- 10
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.02, .003) # fast, slow
gam <- .9 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam)
#####

# GRwf-10-20
#####
# Site/depth info
PMeco <- "GRwf"
lyr_bot <- 20
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.01, .003) # fast, slow
gam <- .8 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam)
#####

# GRwf-20-30
#####
# Site/depth info
PMeco <- "GRwf"
lyr_bot <- 30
lyr_top <- ifelse(lyr_bot == 10, 0, ifelse(lyr_bot == 20, 10, 20))
PMeco_depth <- paste0(PMeco, "_", lyr_top, "-", lyr_bot)

# 14C constraints
con.df <- con.df.fx(PMeco_depth)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.01, .003) # fast, slow
gam <- .5 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam)


#####

# Save output
save(pars.i.2pp, file = paste0("~/sra-ts/data/derived/modFit_pars/", "pars.i.2pp", "_", Sys.Date(), ".Rdata"))
