# MCMC script for Bayesian parameter optimization
# aut: J. Beem-Miller
# date: 09-Nov-2020

# script requires running all code chunks prior to "Bayesian parameter estimation" section of "sra-ts.Rmd"

# set date for saving files
date <- Sys.Date()

## Markov Chain Monte Carlo parameter optimization
# Note that the model uses prior variance distribution from 'modFit' optimization

# Model iterations
iter <- 5000 # start with 5000

# for saving script output
save.iter <- paste0(iter, "iter", ".RData")
save.dir <- file.path(paste0("sra-ts/data/derived/bayes-par-fit-", date))
if(!dir.exists(save.dir)) {dir.create(file.path(paste0("sra-ts/data/derived/bayes-par-fit-", date)))}  

## Create list of MCMC fits (adaptive algorithm with delayed rejection)
# same upper/lower limits as in modFit, same cost fx
test.ix <- seq(1, 27, 3) # start with just 0-10 cm depth increments
bayes_fit_2pp <- lapply(test.ix, function(i) {
  
    start <- Sys.time()
    cat(paste0(names(pars.fit)[i], " parameter fitting\n"))
    
    # define cost fx for current iteration
    mod.Cost <- function(pars){
      modelOutput <- modFun_2pp(pars, pass = TRUE)
      cost1 <- modCost(model = modelOutput, obs = obs.bulk.14c[[i]], scaleVar = TRUE)
      return(modCost(model = modelOutput, obs = obs.resp.14c[[i]], cost = cost1))
    }
    
    # use modFit var and covariance matrix
    var0 <- mod.fits[[i]][["var_ms_unweighted"]]
    jump <- pars.fit.sum[[i]][["cov.unscaled"]]
    
    # run MCMC
    fit <- modMCMC(f = mod.Cost, 
                   p = unlist(pars.fit[[i]][1, 1:3]), 
                   var0 = var0,
                   jump = jump,
                   upper = c(1, 1, 1), 
                   lower = c(0, 0, 0),
                   niter = iter,
                   ntrydr = 2)
    end <- Sys.time()
    cat(paste0("time: ", end - start, "\n"))
    return(fit)
})

# 'ntrydr' can improve poor acceptance rate by adjusting the delayed rejection parameter
# e.g. ntrydr = 2 means upon first rejection, the next parameter candidate is tried
# (ntrydr = 1 means no delayed rejection steps)

# 'dr_steps' denotes the number of delayed rejection steps;
# 'Alfasteps' is the number of times the algorithm has entered the acceptance function for delayed rejection

# save output
# *WARNING* will overwrite if file from current date exist!
save(bayes_fit_2pp, file = paste0(save.dir, "/bayes_fit_2pp-", save.iter))


## SA and TT uncertainty
# Function to calculate system age, pool ages, and transit time for all bayesian parameter combinations
sa.tt.fx <- function(ks, gam, In, iter) {
  
  # initialize list
  ls.nms <- c("SA.ls", "TT.ls", "fast.age.ls", "slow.age.ls", "stock.ls")
  SA.TT.ls <- lapply(ls.nms, function(ls) {
    ls <- vector(mode = "list", length = iter)
  })
  names(SA.TT.ls) <- ls.nms
  
  # set progress bar
  pb <- txtProgressBar(min = 0, max = iter, style = 3)
  
  for (i in 1:iter) {
    
    # model matrix
    A <- -1 * diag(ks[[i]])
    
    # System ages and transit times
    SA <- systemAge(A = A, u = In[[i]], a = ages)
    TT <- transitTime(A = A, u = In[[i]], a = ages)
    
    # Append to list
    SA.TT.ls[["SA.ls"]][[i]] <- as.numeric(SA$meanSystemAge)
    SA.TT.ls[["TT.ls"]][[i]] <- as.numeric(TT$meanTransitTime)
    SA.TT.ls[["fast.age.ls"]][[i]] <- as.numeric(SA$meanPoolAge[1])
    SA.TT.ls[["intm.age.ls"]][[i]] <- as.numeric(SA$meanPoolAge[2])
    SA.TT.ls[["slow.age.ls"]][[i]] <- as.numeric(SA$meanPoolAge[3])
    
    # tracker
    setTxtProgressBar(pb, i)
  }
  return(SA.TT.ls)
}

# note that the following function call is very time consuming...
SA.TT.2pp.ls <- sa.tt.fx(pars = bayes_fit_2pp, iter = nrow(bayes_fit_2pp$pars), a31 = TRUE)

# save output
# *WARNING* will overwrite if files from current date exist!
save(s.3p.SA.TT.ls, file = paste0(save.dir, "/bayes_fit_s_3p.SA.TT.", save.iter))
save(z.3p.SA.TT.ls, file = paste0(save.dir, "/bayes_fit_z_3p.SA.TT.", save.iter))

# test
#####
start <- Sys.time()
cat(paste0(names(pars.fit)[1], " parameter fitting\n"))

# define cost fx for current iteration
mod.Cost <- function(pars){
  modelOutput <- modFun_2pp(pars, pass = TRUE)
  cost1 <- modCost(model = modelOutput, obs = obs.bulk.14c[[i]], scaleVar = TRUE)
  return(modCost(model = modelOutput, obs = obs.resp.14c[[i]], cost = cost1))
}

# use modFit var and covariance matrix
var0 <- mod.fits.0_10[[1]][["var_ms_unweighted"]]
jump <- pars.fit.sum[[1]][["cov.unscaled"]]

# run MCMC
fit <- modMCMC(f = mod.Cost, 
               p = unlist(pars.fit[[1]][1, 1:3]), 
               var0 = var0,
               jump = jump,
               upper = c(1, 1, 1), 
               lower = c(0, 0, 0),
               niter = iter,
               # updatecov = updatecov,
               ntrydr = 2)
end <- Sys.time()
cat(paste0("time: ", end - start, "\n"))

# check fit
plot(fit)

# cov = updatecov, jmp = jump (set to cov.unscaled), bounds = 0,1
# fit_cov10_ntry2_jmpCov <- fit # 30 mins
# fit_covNULL_ntry2_jmpCov <- fit # 37 mins
round(fit_cov10_ntry2_jmpCov$bestpar,4)
round(fit_covNULL_ntry2_jmpCov$bestpar,4)
