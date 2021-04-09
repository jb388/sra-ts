## 2-pool series model initial parameter search script
# Requires the following functions: con.df.fx; par.fx; C14.2p.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c
# These functions and inputs are in Rmd file "sra-ts.RMD" (this directory)

# Notes:
# 1) a21 parameter affects both shape of the bulk 14C curve and carbon stocks
# 2) k1 parameter fits the respiration
# 3) k2 fits the bulk 14C

# changed to constant inputs by eco/depth on 10-Dec-2020

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
con.df <- con.df.fx(PMeco_depth)

# initial pars
soc <- csoc.19.0_30[[PMeco_depth]][ , "lyr_soc"]
In <- in.est[[PMeco_depth]]
ks <- c(.18, .008)
tc <- .1 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.01, .0005) # fast, slow
tc <- .02 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .003) # fast, slow
tc <- .6 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.15, .005) # fast, slow
tc <- .3 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .003) # fast, slow
tc <- .5 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .0015) # fast, slow
tc <- .4 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .001) # fast, slow
tc <- .1 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .001) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .001) # fast, slow
tc <- .25 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.1, .008) # fast, slow
tc <- .55 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .006) # fast, slow
tc <- .5 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .003) # fast, slow
tc <- .7 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.24, .0045) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.2, .0045) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.24, .0034) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.3, .0045) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.4, .004) # fast, slow
tc <- .15 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.4, .004) # fast, slow
tc <- .1 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.1, .01) # fast, slow
tc <- .18 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.03, .01) # fast, slow
tc <- .8 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .0035) # fast, slow
tc <- .6 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .005) # fast, slow
tc <- .7 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars 
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
In <- in.est[[PMeco_depth]]
ks <- c(.008, .002) # fast, slow
tc <- .2 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.02, .002) # fast, slow
tc <- .3 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.15, .008) # fast, slow
tc <- .1 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.5, .004) # fast, slow
tc <- .02 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars
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
In <- in.est[[PMeco_depth]]
ks <- c(.4, .004) # fast, slow
tc <- .9 # transfer coef 
pars <- c(ks, tc)

# evaluate pars
ini.2ps.C14.df <- par.fx(pars = pars, In = In, mod = "2ps")

# plot
C14.2p.plot.fx(ini.2ps.C14.df, con.df, mod = "2ps", PMeco_depth = PMeco_depth)

# save pars
pars.i.2ps[[PMeco_depth]] <- pars


#####

# Save output
save(pars.i.2ps, file = paste0("~/sra-ts/data/derived/modFit_pars/", "pars.i.2ps", "_", Sys.Date(), ".Rdata"))
