## 2-pool parallel model initial parameter search script
# Requires the following functions: con.df.fx; par.fx; C14.2p.plot.fx
# Requires the following input: obs.bulk.14c, obs.resp.14c
# These functions and inputs are in Rmd file "sra-ts.RMD" (this directory)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.18, .0045) # fast, slow
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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.18, .0025) # fast, slow
gam <- .9 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.2, .002) # fast, slow
gam <- .8 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.14, .004) # fast, slow
gam <- .95 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.25, .0025) # fast, slow
gam <- .85 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.11, .0017) # fast, slow
gam <- .75 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.13, .0021) # fast, slow
gam <- .8 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.06, .0015) # fast, slow
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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.075, .0011) # fast, slow
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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.08, .0045) # fast, slow
gam <- .9 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.12, .0044) # fast, slow
gam <- .8 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .0025) # fast, slow
gam <- .7 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.25, .0044) # fast, slow
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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.16, .004) # fast, slow
gam <- .85 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.14, .002) # fast, slow
gam <- .85 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.26, .0036) # fast, slow
gam <- .95 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .0023) # fast, slow
gam <- .95 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .001) # fast, slow
gam <- .95 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.09, .007) # fast, slow
gam <- .9 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.08, .006) # fast, slow
gam <- .7 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.2, .0044) # fast, slow
gam <- .8 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

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
con.df <- con.df.fx(PMeco, lyr_bot)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.11, .0045) # fast, slow
gam <- .95 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.2, .003) # fast, slow
gam <- .95 # partitioning coef (constrain to 0.7 to 0.95)

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
con.df <- con.df.fx(PMeco, lyr_bot)

# initial pars
In <- in.i[[PMeco_depth]]
ks <- c(.3, .0025) # fast, slow
gam <- .95 # partitioning coef (constrain to 0.7 to 0.95)

# evaluate pars
ini.2pp.C14.df <- par.fx(pars = c(ks, gam), In = In, mod = "2pp")

# plot
C14.2p.plot.fx(ini.2pp.C14.df, con.df, mod = "2pp")

# save pars
pars.i.2pp[[PMeco_depth]] <- c(ks, gam)


#####

# Save output
save(pars.i.2pp, file = paste0("~/sra-ts/data/derived/modFit_pars/", "pars.i.2pp", "_", Sys.Date(), ".Rdata"))