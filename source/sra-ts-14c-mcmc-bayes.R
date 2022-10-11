# MCMC script for Bayesian parameter optimization
# aut: J. Beem-Miller
# date: 16-Aug-2022

# script requires running all code chunks prior to "Bayesian parameter estimation" section of "sra-ts.Rmd"
# # 2pp
# pars.fit <- pars.fit.2pp
# pars.fit.sum <- pars.fit.2pp.sum
# mod.fit <- mod.fits.2pp
# mod <- "2pp"
# In.fit <- in.fit.2pp
  
# set date for saving files
date <- Sys.Date()

## Markov Chain Monte Carlo parameter optimization
# Note that the model uses prior variance distribution from 'modFit' optimization

# Model iterations
iter <- 5000 # start with 5000

# for saving script output
save.iter <- paste0(iter, "iter", ".RData")
save.dir <- file.path(paste0("~/sra-ts/data/derived/bayes-par-fit-", date))
if(!dir.exists(save.dir)) {dir.create(file.path(paste0("~/sra-ts/data/derived/bayes-par-fit-", date)))}  

## Create list of MCMC fits (delayed rejection option, no adaptation)
# Notes: uses modFit var and covariance matrix, e.g.
# var0 = mod.fit[[i]][["var_ms_unweighted"]]
# jump = pars.fit.2ps.sum[[i]][["cov.unscaled"]]
# same upper/lower limits as in modFit, same cost fx
ix.10 <- seq(1, 27, 3) # start with just 0-10 cm depth increment subset
bayes.fit.fx <- function(mod, pars.fit, In.fit, var0, jump, sub, upper, lower) {
  lapply(seq_along(pars.fit[sub]), function(i) {
  
    start <- Sys.time()
    cat(paste0(names(pars.fit)[i], " parameter fitting\n"))
    
    # define cost fx for current iteration
    # 14C costs only for now...
    mod.Cost <- function(pars) {
      modelOutput <- modFun(pars, In = In.fit[sub][[i]], pass = TRUE, mod = mod)
      cost1 <- modCost(model = modelOutput, obs = obs.bulk.14c[sub][[i]], scaleVar = TRUE)
      return(modCost(model = modelOutput, obs = obs.resp.14c[sub][[i]], scaleVar = TRUE, cost = cost1))
    }
    
    # run MCMC
    fit <- tryCatch(
      modMCMC(f = mod.Cost, 
              p = pars.fit[sub][[i]], 
              var0 = var0[sub][[i]][["var_ms_unweighted"]],
              jump = jump[sub][[i]][["cov.unscaled"]],
              upper = upper, 
              lower = lower,
              niter = iter,
              ntrydr = 2),
      error = function (e) {cat("ERROR :", conditionMessage(e), "\n")})
    end <- Sys.time()
    cat(paste0("time: ", end - start, "\n"))
    return(fit)
  })
}

## Run function
# 2pp
bayes_fit_2pp_0_10 <- bayes.fit.fx(mod = "2pp",
                                   pars.fit = pars.fit.2pp,
                                   In.fit = in.fit.2pp,
                                   var0 <- mod.fits.2pp,
                                   jump = pars.fit.2pp.sum,
                                   sub = ix.10,
                                   upper = c(1, 1, 1),
                                   lower = c(0, 0 ,0))
names(bayes_fit_2pp_0_10) <- names(mod.fits.2pp)

# test fits for 
# a) well-fit site w/ standard optimization
bayes_fit_2pp_ANpp_0_10 <- bayes.fit.fx(mod = "2pp",
                                   pars.fit = pars.fit.2pp,
                                   In.fit = in.1.ls,
                                   var0 <- mod.fits.2pp,
                                   jump = pars.fit.2pp.sum,
                                   sub = 1,
                                   upper = c(1, 1, 1),
                                   lower = c(0, 0 ,0))
names(bayes_fit_2pp_ANpp_0_10) <- "ANpp_0-10"
# b) poorly-fit site w/ standard optimization
bayes_fit_2pp_ANrf_0_10 <- bayes.fit.fx(mod = "2pp",
                                        pars.fit = pars.fit.2pp,
                                        In.fit = in.1.ls,
                                        var0 <- mod.fits.2pp,
                                        jump = pars.fit.2pp.sum,
                                        sub = 4,
                                        upper = c(1, 1, 1),
                                        lower = c(0, 0 ,0))
names(bayes_fit_2pp_ANrf_0_10) <- "ANrf_0_10"
C14.plot.fx(modFun(bayes_fit_2pp_ANrf_0_10$ANrf_0_10$bestpar, mod = "2pp", In = 1, out = ""), con.df = con.df.fx("ANrf_0-10"), mod, PMeco_depth = "ANrf_0-10")

# b) poorly-fit site w/ fm optimization
bayes_fit_2pp_ANrf_0_10 <- bayes.fit.fx(mod = "2pp",
                                        pars.fit = pars.fit.2pp,
                                        In.fit = in.1.ls,
                                        var0 <- mod.fits.2pp,
                                        jump = pars.fit.2pp.sum,
                                        sub = 4,
                                        upper = c(1, 1, 1),
                                        lower = c(0, 0 ,0))
names(bayes_fit_2pp_ANrf_0_10) <- "ANrf_0_10"

# 2ps
bayes_fit_2ps_0_10 <- bayes.fit.fx(mod = "2ps",
                                   pars.fit = pars.fit.2ps,
                                   In.fit = in.fit.2ps,
                                   var0 <- mod.fits.2ps,
                                   jump = pars.fit.2ps.sum,
                                   sub = ix.10,
                                   upper = c(1, 1, 1),
                                   lower = c(0, 0 ,0))
names(bayes_fit_2ps_0_10) <- names(mod.fits.2ps)

# 'ntrydr' can improve poor acceptance rate by adjusting the delayed rejection parameter
# e.g. ntrydr = 2 means upon first rejection, the next parameter candidate is tried
# (ntrydr = 1 means no delayed rejection steps)

# 'dr_steps' denotes the number of delayed rejection steps;
# 'Alfasteps' is the number of times the algorithm has entered the acceptance function for delayed rejection

# save output
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_2pp_0_10, file = paste0(save.dir, "/bayes_fit_", "2pp_", "0-10_", save.iter))
save(bayes_fit_2ps_0_10, file = paste0(save.dir, "/bayes_fit_", "2ps_", "0-10_", save.iter))

## SA and TT uncertainty
# Function to calculate system age, pool ages, and transit time for all bayesian parameter combinations
sa.tt.fx <- function(mod, pars, input, iter) {
  
  # define parameters
  In <- c(input * pars[3], input * (1 - pars[3]))
  
  # initialize list
  ls.nms <- c("SA.ls", "TT.ls", "fast.age.ls", "slow.age.ls")
  SA.TT.ls <- lapply(ls.nms, function(ls) {
    ls <- vector(mode = "list", length = iter)
  })
  names(SA.TT.ls) <- ls.nms
  
  # set progress bar
  pb <- txtProgressBar(min = 0, max = iter, style = 3)
  
  for (i in 1:iter) {
    
    # model matrix
    A <- -1 * diag(pars[[1:2]])
    if (mod == "2ps") {
      A[2, 1] <- pars[[3]]
    }
    
    # System ages and transit times
    SA <- systemAge(A = A, u = In)
    TT <- transitTime(A = A, u = In)
    
    # Append to list
    SA.TT.ls[["SA.ls"]][[i]] <- as.numeric(SA$meanSystemAge)
    SA.TT.ls[["TT.ls"]][[i]] <- as.numeric(TT$meanTransitTime)
    SA.TT.ls[["fast.age.ls"]][[i]] <- as.numeric(SA$meanPoolAge[1])
    SA.TT.ls[["slow.age.ls"]][[i]] <- as.numeric(SA$meanPoolAge[2])
    
    # tracker
    setTxtProgressBar(pb, i)
  }
  return(SA.TT.ls)
}

# note that the following function call is very time consuming...
# 2pp
SA.TT.2pp.ls <- lapply(seq_along(bayes_fit_2pp_0_10), function(j) {
  sa.tt.fx(mod = "2pp", bayes_fit_2pp_0_10[[j]][["bestpar"]], in.fit.2pp[[j]], iter)
})
# 2ps
SA.TT.2ps.ls <- lapply(seq_along(bayes_fit_2ps_0_10), function(j) {
  sa.tt.fx(mod = "2ps", bayes_fit_2ps_0_10[[j]][["bestpar"]], in.fit.2ps[[j]], iter)
})

# save output
# *WARNING* will overwrite if files from current date exist!
save(SA.TT.2pp.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2pp_", save.iter))
save(SA.TT.2ps.ls, file = paste0(save.dir, "/bayes_fit_SA_TT_", "2ps_", save.iter))