# Flux estimates for Sierra transect sites

# load Wang 2000 data
wang_flx <- read.csv("/Users/jeff/sra-ts/data/external/sra_wang_flx/Wang_2000_flx.csv")

# Musick site == GRpp analog (~1300 m)
m.flx <- wang_flx[wang_flx$site_name=="Trumbore Musick",]
m.flx <- m.flx[which(m.flx$flx_pathway_note!="snow-covered"),]
m.flx$flx_source <- ifelse(m.flx$flx_pathway_note == "litter-free surface", "soil", "LitterSoil")
m.flx$flx_source <- ifelse(is.na(m.flx$flx_source), "atm", m.flx$flx_source)
m.flx.mean <- tapply(m.flx$flx_rate, m.flx$flx_source, mean, na.rm=T)

# Chiquito site == GRrf analog (~2300 m)
c.flx <- wang_flx[wang_flx$site_name=="Trumbore Chiquito",]
c.flx <- c.flx[which(c.flx$flx_pathway_note!="snow-covered"),]
c.flx$flx_source <- ifelse(c.flx$flx_pathway_note == "litter-free surface", "soil", "LitterSoil")
c.flx$flx_source <- ifelse(is.na(c.flx$flx_source), "atm", c.flx$flx_source)
c.flx.mean <- tapply(c.flx$flx_rate, c.flx$flx_source, mean, na.rm=T)

# conversion factor for micromol CO2 m-2 s-1 to kgC m-2 y-1
umolCO2s.kgCyr <- 60*60*24*365*(10^-6)*(12/44)*12*(10^-3) # umol CO2 m-2 s-1 to kgC m-2 yr-1

# Calculate Musick/GRpp fluxes
m.ls.flx <- as.vector(m.flx.mean[1] * umolCO2s.kgCyr) # litter + soil flux
m.s.flx <- as.vector(m.flx.mean[2] * umolCO2s.kgCyr) # soil flux = 0.36
m.l.flx <- ls.flx-s.flx # litter flux = 0.07

# Calculate Chiquito/GRrf fluxes
c.ls.flx <- as.vector(c.flx.mean[1] * umolCO2s.kgCyr) # litter + soil flux
c.s.flx <- as.vector(c.flx.mean[2] * umolCO2s.kgCyr) # soil flux = 0.15
c.l.flx <- c.ls.flx - c.s.flx # litter flux = 0.04


## Compare to Rasmussen incubation data
ras06 <- data.frame(read_excel("/Users/jeff/sra-ts/data/external/sra_ras_inc/RespRates_Rasmussen2006.xlsx", 
                               sheet = "RatesSum"))

## For comparsion, cf. Gaudinski 2000
# Harvard forest: O = 20%, A = 44%, B+C = 36% (of total, including root resp)
# therefore A horizon ~ 55% of soil efflux (when O hzn not included)
# 48% of soil flux = root resp, so het. resp. factor = .55 * .48
het.resp.f <- .55 * .48

## Incubation estimated fluxes (in kg m-2 yr-1, from Rasmussen 2006 data, A horizon) vs. Wang fluxes
# GRpp
round(ras06[ras06$PM == "GR" & ras06$ECO == "pp", "kgC_m2_y"], 2) # Ras06
round(m.s.flx * het.resp.f, 2) # Wang
# GRrf
round(ras06[ras06$PM == "GR" & ras06$ECO == "rf", "kgC_m2_y"], 2) # Ras06
round(c.s.flx * het.resp.f, 2) # Wang

# Notes:
# incubation estimates are much higher, esp. GRpp, likely due to optimum inc conditions
# Closer for GRrf, perhaps because of seasonal impacts on field fluxes and colder inc. temp.?

## Use Ras06 stock to flux ratio for A horizon to estimate each 10cm increment
# GRpp
GRpp.flx.stk <- ras06[ras06$PM == "GR" & ras06$ECO == "pp", "flx_stock_ratio"] 
GRpp.in.10 <- GRpp.flx.stk * csoc.19.0_30[["GRpp_0-10"]][["lyr_soc"]]
in.fit.2pp.p3.5.95[["GRpp_0-10"]]
# GRrf
GRrf.flx.stk <- ras06[ras06$PM == "GR" & ras06$ECO == "rf", "flx_stock_ratio"] 
GRrf.in.10 <- GRrf.flx.stk * csoc.19.0_30[["GRrf_0-10"]][["lyr_soc"]]
in.fit.2pp.p3.5.95[["GRrf_0-10"]]

### Test optimization

## 2pp
modFun_2p <- function(pars) {
  
  # initial 14C
  F0_Delta14C <- unlist(lapply(pars.i[1:2], function(x) Delta14C_from_AbsoluteFractionModern(fm(x))))
  
  # initial stocks
  A <- -1 * diag(pars.i[1:2])
  ss.cstock <- (-1 * solve(A) %*% c(In * pars.i[3], In * (1 - pars.i[3])))
  
  # model
  mod <- TwopParallelModel14(t = Datm$Date,
                             ks = pars[1:2],
                             C0 = c(ss.cstock[1], ss.cstock[2]),
                             F0_Delta14C = F0_Delta14C,
                             In = In,
                             gam = pars[3],
                             inputFc = Datm)
  
  # get mod values
  C14m <- getF14C(mod)
  C14p <- getF14(mod)
  C14r <- getF14R(mod)
  Ctot <- getC(mod)
  
  return(data.frame(
      time = Datm$Date,
      bulkC = C14m, 
      resp = C14r,
      flx.stock = In/sum(Ctot[1, ])))
}


# GRpp
pars.i <- c(0.098, 0.005, 0.58)
# In <- in.fit.2pp.p3.5.95[["GRpp_0-10"]]
In <- GRrf.in.10
A <- -1 * diag(pars.i[1:2])
ss.cstock <- (-1 * solve(A) %*% c(In * pars.i[3], In * (1 - pars.i[3])))
F0_Delta14C <- unlist(lapply(pars.i[1:2], function(x) Delta14C_from_AbsoluteFractionModern(fm(x))))
upper <- c(1, .05, .95)
lower <- c(.05, .0002, .5)

# flx.stock cost
obs.flx.stock <- data.frame(time = 2001.5,
                            flx.stock = In/csoc.19.0_30[["GRrf_0-10"]][["lyr_soc"]])
GRpp.flx.stk <- ras06[ras06$PM == "GR" & ras06$ECO == "pp", "flx_stock_ratio"] 
GRpp.in.10 <- GRpp.flx.stk * csoc.19.0_30[["GRpp_0-10"]][["lyr_soc"]]

mod.Cost.2pp2 <- function(pars) {
  modelOutput <- modFun_2pp2(pars)
  cost1 <- modCost(model = modelOutput, obs = obs.bulk.14c[["GRrf_0-10"]], scaleVar = TRUE)
  cost2 <- modCost(model = modelOutput, obs = obs.resp.14c[["GRrf_0-10"]], cost = cost1) 
  return(modCost(model = modelOutput, obs = obs.flx.stock, cost = cost2))
}
    
# fit pars
fit.2pp <- tryCatch(
  modFit(f = mod.Cost.2pp2,
         p = pars.i,
         method = 'Nelder-Mead',
         upper = upper, 
         lower = lower),
         error = function (e) {cat("ERROR :", conditionMessage(e), "\n")}
  )

# compare
fit.2pp$par
mod.fits.2pp.p3.5.95$`GRrf_0-10`$par
fit.2pp$ssr
mod.fits.2pp.p3.5.95$`GRrf_0-10`$ssr
fit.2pp$var_ms
mod.fits.2pp.p3.5.95$`GRrf_0-10`$var_ms

# save
# GRpp_0_10.fit.2pp <- fit.2pp
GRrf_0_10.fit.2pp <- fit.2pp


## Run for all sites
# Obs cost
obs.flx.stock <- lapply(split(ras06, ras06$PMeco), function(x) {
  data.frame(time = 2001.5, # arbitrary
             flx.stock = x[ , "flx_stock_ratio"])
})

# calculate new inputs using flx/stock ratio
in.flx.stock <- lapply(seq_along(obs.flx.stock), function(i) {
  obs.flx.stock[[i]][["flx.stock"]]/csoc.19.0_30[ix.10][[i]][["lyr_soc"]]
})

mod.fits.fx2 <- function(mod, pars, sub, upper, lower) {
  # start loop
  lapply(seq_along(pars[sub]), function(i) {
    # start timer and print PMeco_depth
    start <- Sys.time()
    cat(paste0(names(pars)[sub][i], " parameter fitting\n"))
    
    # define pars
    pars <- list(pars[sub][[i]])
    
    # define cost function and set par names for current iteration
    if (mod == "2pp") {
      mod.Cost <- function(pars) {
        modelOutput <- modFun_2p(pars, in.flx.stock[[i]], mod = "2pp")
        cost1 <- modCost(model = modelOutput, obs = obs.bulk.14c[sub][[i]], scaleVar = TRUE)
        cost2 <- modCost(model = modelOutput, obs = obs.resp.14c[sub][[i]], cost = cost1) 
        return(modCost(model = modelOutput, obs = obs.flx.stock[[i]], cost = cost2))
      }
      names(pars) <- c("k1", "k2", "gam")
    } else {
      mod.Cost <- function(pars) {
        modelOutput <- modFun_2p(pars, in.flx.stock[[i]], mod = "2ps")
        cost1 <- modCost(model = modelOutput, obs = obs.bulk.14c[sub][[i]], scaleVar = TRUE)
        cost2 <- modCost(model = modelOutput, obs = obs.resp.14c[sub][[i]], cost = cost1) 
        return(modCost(model = modelOutput, obs = obs.flx.stock[[i]], cost = cost2))
      }
      names(pars) <- c("k1", "k2", "a21")
    }
    
    # fit pars
    fit <- tryCatch(
      modFit(f = mod.Cost,
             p = pars,
             method = 'Nelder-Mead',
             upper = upper, 
             lower = lower),
      error = function (e) {cat("ERROR :", conditionMessage(e), "\n")})
    
    # End timer and print elapsed time
    end <- Sys.time()
    cat(paste0("time: ", end - start, "\n"))
    
    # Return fitted parameters
    return(fit)
  }) 
}

# Run
# Start with just 0-10 cm depth increment
ix.10 <- seq(1, 27, 3)

## 2pp
mod.fits.2pp2 <- mod.fits.fx2(mod = "2pp",
                              pars = pars.i.2pp, 
                              sub = ix.10,
                              upper = c(1, .02, .951),
                              lower = c(.04, 0, .5))
names(mod.fits.2pp2) <- names(pars.i.2pp)[ix.10]
save(mod.fits.2pp2, file = paste0("/Users/jeff/sra-ts/data/derived/modFit_pars/", "mod.fits.2pp.flx.stock", "_", Sys.Date(), ".Rdata"))

## 2ps
mod.fits.2ps2 <- mod.fits.fx2(mod = "2ps",
                              pars = pars.i.2ps, 
                              sub = ix.10,
                              upper = c(1, .02, .1),
                              lower = c(.04, 0, 0))
names(mod.fits.2ps2) <- names(pars.i.2ps)[ix.10]
save(mod.fits.2ps2, file = paste0("/Users/jeff/sra-ts/data/derived/modFit_pars/", "mod.fits.2ps.flx.stock", "_", Sys.Date(), ".Rdata"))

## Best fit pars
# 2pp2
pars.fit.2pp2 <- lapply(mod.fits.2pp2, "[[", 1)
names(pars.fit.2pp2) <- names(pars.i.2pp)[ix.10]
# 2ps2
pars.fit.2ps2 <- lapply(mod.fits.2ps2, "[[", 1)
names(pars.fit.2ps2) <- names(pars.i.2ps)[ix.10]

## Summary of errors
# best par set (ssr)
ssr.2pp2.df <- data.frame(bind_rows(lapply(mod.fits.2pp2, "[", "ssr"), .id = "PMeco_depth"))
ssr.2ps2.df <- data.frame(bind_rows(lapply(mod.fits.2ps2, "[", "ssr"), .id = "PMeco_depth"))
# mean residuals, by var (var_ms)
var_ms.2pp2.df <- var_ms.df.fx(mod.fits.2pp2, c("bulkC", "resp", "flx.stock"))
var_ms.2ps2.df <- var_ms.df.fx(mod.fits.2ps2, c("bulkC", "resp", "flx.stock"))

### Look at inputs & stocks
## 2pp2
# in
in.fit.2pp2 <- lapply(seq_along(pars.fit.2pp2), function(i) {
  PMeco_depth <- names(pars.fit.2pp2)[i]
  SOC <- csoc.19.0_30[[PMeco_depth]][ ,"lyr_soc"]
  return(in.fit.fx("2pp", pars.fit.2pp2[[i]], in.flx.stock[[i]], SOC))
})
names(in.fit.2ps2) <- names(mod.fits.2ps2)
# stocks
mod.socs.2pp2.ls <- lapply(seq_along(pars.fit.2pp2), function(i) {
  soc.fx("2pp", pars.fit.2pp2[[i]], in.fit.2pp2[[i]])
})
names(mod.socs.2pp2.ls) <- names(pars.fit.2pp2)
## 2ps2
# in
in.fit.2ps2 <- lapply(seq_along(pars.fit.2ps2), function(i) {
  PMeco_depth <- names(pars.fit.2ps2)[i]
  SOC <- csoc.19.0_30[[PMeco_depth]][ ,"lyr_soc"]
  return(in.fit.fx("2ps", pars.fit.2ps2[[i]], in.i[ix.10][[i]], SOC))
})
names(in.fit.2ps2) <- names(mod.fits.2ps2)
# stocks
mod.socs.2ps2.ls <- lapply(seq_along(pars.fit.2ps2), function(i) {
  soc.fx("2ps", pars.fit.2ps2[[i]], in.fit.2ps2[[i]])
})
names(mod.socs.2ps2.ls) <- names(pars.fit.2ps2)


# plot pars
par.plot.fx(mod = "2pp (gam = [0.5, 0.95])",
            depth = "0-10",
            par.df = pars.fit.2pp.p3.5.95.df,
            initial = FALSE)


# test
pars <- list(gmax = 0.5, eff = 0.5,
             ks = 0.5, rB = 0.01, dB = 0.01)

solveBact <- function(pars) {
  derivs <- function(t, state, pars) { # returns rate of change
    with (as.list(c(state, pars)), {
      dBact <-  gmax * eff * Sub/(Sub + ks) * Bact - dB * Bact - rB * Bact
      dSub  <- -gmax       * Sub/(Sub + ks) * Bact + dB * Bact
      return(list(c(dBact, dSub)))
    })
  }
  state   <- c(Bact = 0.1, Sub = 100)
  tout    <- seq(0, 50, by = 0.5)
  ## ode solves the model by integration ...
  return(as.data.frame(ode(y = state, times = tout, func = derivs,
                           parms = pars)))
}

SnsBact <- sensFun(func = solveBact, parms = pars,
                   sensvar = "Bact", varscale = 1)
