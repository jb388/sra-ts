## 1-pool model initial parameter search script
# Requires the following functions: con.df.fx; par.fx; C14.plot.fx; modFun_1p
# Requires the following input: obs.bulk.14c, obs.resp.14c, obs.Cstock
# These functions and inputs are in Rmd file "sra-ts.RMD" (this directory)

# Notes:
# 1) bulk 14C fit to a one pool model, which represents the mean age of the bulk soil
# 2) respired 14C is also fit to a one pool model, which represents the median transit time for the bulk soil

# Initialize lists (9 sites by 3 depths = 27 elements)
# bulk
pars.i.1pb <- lapply(seq_len(27), function(df) {
  data.frame(k = NA)
})
names(pars.i.1pb) <- names(obs.bulk.14c)
# resp
pars.i.1pr <- pars.i.1pb

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
In <- in.est[[PMeco_depth]]
kb <- .01
kr <- .1

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .0025
kr <- .009

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .0025
kr <- .005

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .006
kr <- .012

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .006
kr <- .012

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .006
kr <- .012

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .006
kr <- .012

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .006
kr <- .012

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .006
kr <- .012

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .009
kr <- .012

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .006
kr <- .0085

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .004
kr <- .0045

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = Inb, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = Inr, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .004
kr <- .0045

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .004
kr <- .0045

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .004
kr <- .0045

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .004
kr <- .0045

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .004
kr <- .0045

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .004
kr <- .0045

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .015
kr <- .028

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
kb <- .009
kr <- .009

# evaluate pars
ini.1pb.C14.df <- par.fx(pars = kb, In = In, mod = "1p")
ini.1pr.C14.df <- par.fx(pars = kr, In = In, mod = "1p")

ini.1p.C14.df <- rbind(ini.1pb.C14.df, 
                       ini.1pr.C14.df[which(ini.1pr.C14.df$pool == "bulk C"), ] %>%
                         mutate(pool = "respiration"))

# plot
C14.1p.plot.fx(ini.1p.C14.df, con.df, PMeco_depth = PMeco_depth)

# save pars
pars.i.1pb[[PMeco_depth]] <- kb
pars.i.1pr[[PMeco_depth]] <- kr
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
In <- in.est[[PMeco_depth]]
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
In <- in.est[[PMeco_depth]]
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
In <- in.est[[PMeco_depth]]
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
In <- in.est[[PMeco_depth]]
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
In <- in.est[[PMeco_depth]]
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
In <- in.est[[PMeco_depth]]
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
