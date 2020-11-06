# use 2019 estimated soc for steady-state C stocks
BSwf.sum <- data.frame(sra.2019.sp.ls$BSwf %>%
                         filter(lyr_bot < 31 & lyr_bot > 0) %>%
                         select(lyr_top, lyr_bot, lyr_soc) %>%
                         group_by(lyr_top, lyr_bot) %>%
                         summarize(lyr_soc = mean(lyr_soc)))


# initial pars
cstock <- lapply(sra.2019.sp.ls, function(df) {
  df[df$lyr_bot < 31 , c("lyr_bot", "lyr_soc")]
})
In <- .16

# gamma: input partitioning coefficient (proportion to fast pool, function of ks?) *target var*
gam <- .9 # initial value
ks <- c(.12, .004) # fast, slow

# solve for steady-state C stocks (should equal WS-bc)
A <- -1*diag(ks)
ss.cstock <- (-1*solve(A)%*%c(In*gam, In*(1-gam)))
round(sum(-1*solve(A)%*%c(In*gam, In*(1-gam))), 1)

# determine initial d14C using k values and a starting year of 1900
F0_Delta14C <- c(Delta14C_from_AbsoluteFractionModern(fm(ks[1])),
                 Delta14C_from_AbsoluteFractionModern(fm(ks[2])))

# 2pool parallel model
BSwf.0_10.2pp <- TwopParallelModel14(t = Year,
                                     ks = ks,
                                     C0 = c(ss.cstock[1], ss.cstock[2]),
                                     F0_Delta14C = F0_Delta14C,
                                     In = In,
                                     gam = gam,
                                     inputFc = Datm)

BSwf.0_10.2pp.C14m <- getF14C(BSwf.0_10.2pp) 
BSwf.0_10.2pp.C14 <- getF14(BSwf.0_10.2pp)
BSwf.0_10.2pp.HR <- getF14R(BSwf.0_10.2pp)
BSwf.0_10.2pp.Ctot <- getC(BSwf.0_10.2pp)

BSwf.0_10.2pp.C14.df <- data.frame(
  years = rep(Datm$Date, 5),
  d14C = c(BSwf.0_10.2pp.C14[,1], 
           BSwf.0_10.2pp.C14[,2], 
           BSwf.0_10.2pp.C14m,
           BSwf.0_10.2pp.HR,
           Datm$NHc14),
  pool = rep(c("fast", "slow", "bulk C", "respiration", "atm"), each = nrow(BSwf.0_10.2pp.C14))
)

# plot model
BSwf.0_10.2pp.C14.df %>%
  filter(pool == "bulk C" | pool == "respiration" | pool == "atm") %>%
  ggplot(., aes(years, d14C, color = pool)) +
  geom_path() +
  geom_point(data = BSwf_d14, aes(Year, d14c, color = pool), size = 3) +
  geom_errorbar(data = BSwf_d14,
                aes(x = Year,
                    ymin = d14c_l, 
                    ymax = d14c_u, 
                    color = pool), 
                width = .1,
                inherit.aes = FALSE) +
  scale_color_manual(
    name = "Model pool",
    values = c("atm" = 8,
               "bulk C" = "black",
               "fast" = "#D81B60",
               "slow" = "#1E88E5",
               "respiration" = "#FFC107")) +
  scale_x_continuous(limits = c(1950, 2022)) +
  ggtitle("BSwf 2pp") +
  xlab("Year") +
  ylab(expression(''*Delta*''^14*'C (‰)')) +
  theme_bw() +
  theme(panel.grid = element_blank())


# pars = 9x3 element list (sites by kfast, kslow, gamma)
pars.i.ls <- list(pars.i.BSwf,
                  pars.i.BSpp,
                  pars.i.BSrf,
                  pars.i.ANpp,
                  pars.i.ANwf,
                  pars.i.ANrf,
                  pars.i.GRpp,
                  pars.i.GRwf,
                  pars.i.GRrf)

# Initial model run with base parameters
mod.i.ls <- lapply(pars.i.ls, modFun_2pp, pass = FALSE)

# observational constraints
# bulk
obs.bulk.14c <- data.frame(time = c(rep(2001.5, 3), 2009.5, rep(2019.5, 3)),
                           bulkC = c(unlist(lapply(sra.19.01.rep.ls[["BSwf"]], "[[", 1)),
                                     sra.19.01.09[sra.19.01.09$PMeco == "BSwf" & sra.19.01.09$lyr_bot == 10 & sra.19.01.09$Year == 2009, "fm"],
                                     sra.2019.ls$BSwf[sra.2019.ls$BSwf$lyr_bot == 10, "fm"]))
obs.bulk.14c$bulkC <- Delta14C_from_AbsoluteFractionModern(obs.bulk.14c$bulkC)
# resp
obs.resp.14c <- data.frame(time = 2019.5,
                           resp = BSwf_resp_d14$d14c)
# SOC stocks
obs.bulk.soc <- data.frame(time = Year,
                           socs = rep(cstock, length(Year)))

# Cost function (evaluates error as model vs. obsv, per FME req)
# note that we have to set "pass" to TRUE
mod.Cost <- function(pars){
  modelOutput <- modFun_2pp(pars, pass = TRUE)
  cost1 <- modCost(model = modelOutput, obs = obs.bulk.14c, scaleVar = TRUE)
  cost2 <- modCost(model = modelOutput, obs = obs.resp.14c, cost = cost1)
  cost3 <- modCost(model = modelOutput, obs = obs.bulk.soc, cost = cost2)
  return(cost3)
}

# optimize model pars; note that upper values must be defined to prevent negative resp
mod.fit <- modFit(f = mod.Cost,
                  p = pars.i, 
                  method = 'Nelder-Mead', 
                  upper = c(1, .1, 1), 
                  lower = c(0.01, 0, .5))

# look at estimates
mod.fit$par
pars.i # slight decrease in kfast and very slight increase in kslow and gam

# Rerun model and plot results against manually fit parameters
F0_Delta14C.fit <- unlist(lapply(mod.fit$par[1:2], function(x) Delta14C_from_AbsoluteFractionModern(fm(x))))
ss.cstock.fit <- (-1*solve(-1*diag(mod.fit$par[1:2]))%*%c(In*mod.fit$par[3], In*(1-mod.fit$par[3])))
Twopp.fit <- TwopParallelModel14(
  t = Year,
  ks = mod.fit$par[1:2],
  C0 = c(ss.cstock.fit[1], ss.cstock.fit[2]),
  F0_Delta14C = F0_Delta14C.fit,
  In = In,
  gam = mod.fit$par[3],
  inputFc = Datm)

Twopp.C14m.fit <- getF14C(Twopp.fit)  # bulk
# Twopp.C14.fit <- getF14(Twopp.fit)
Twopp.HR.fit <- getF14R(Twopp.fit) # resp
# Twopp.Ctot.fit <- getC(Twopp.fit)

BSwf.0_10.2pp.C14.df.fit <- data.frame(
  years = rep(Year, 3),
  d14C = c(Twopp.C14m.fit, Twopp.HR.fit, Datm$NHc14),
  pool = rep(c("bulk C", "respiration", "atm"), each = length(Twopp.C14m.fit)))
