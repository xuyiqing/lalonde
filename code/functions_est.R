# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Functions for Estimation

#############################
# Function for estimation
#############################


## be quiet!
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 


#                       $$\     $$\                          $$\                                   
#                       $$ |    \__|                         $$ |                                  
#  $$$$$$\   $$$$$$$\ $$$$$$\   $$\ $$$$$$\$$$$\   $$$$$$\ $$$$$$\    $$$$$$\   $$$$$$\   $$$$$$$\ 
# $$  __$$\ $$  _____|\_$$  _|  $$ |$$  _$$  _$$\  \____$$\\_$$  _|  $$  __$$\ $$  __$$\ $$  _____|
# $$$$$$$$ |\$$$$$$\    $$ |    $$ |$$ / $$ / $$ | $$$$$$$ | $$ |    $$ /  $$ |$$ |  \__|\$$$$$$\  
# $$   ____| \____$$\   $$ |$$\ $$ |$$ | $$ | $$ |$$  __$$ | $$ |$$\ $$ |  $$ |$$ |       \____$$\ 
# \$$$$$$$\ $$$$$$$  |  \$$$$  |$$ |$$ | $$ | $$ |\$$$$$$$ | \$$$$  |\$$$$$$  |$$ |      $$$$$$$  |
#  \_______|\_______/    \____/ \__|\__| \__| \__| \_______|  \____/  \______/ \__|      \_______/ 
                                                                                                 
                                                                                                 
library(estimatr)


# difference in means 
diff <- function(data, Y, treat) {
    fml <- as.formula(paste(Y, "~", treat))
    out <- summary(lm_robust(fml, data = data, se_type = "stata"))$coefficients[treat, c(1, 2, 5, 6)]
    return(out) # extract coef, se, ci.lower, ci.upper
}


# regression adjustment
reg <- function(data, Y, treat, covar) {
    fml <- as.formula(paste(Y, "~", treat, "+", paste(covar, collapse = " + ")))
    out <- summary(lm_robust(fml, data = data, se_type = "stata"))$coefficients[treat, c(1, 2, 5, 6)]
    # extract coef, se, ci.lower, ci.upper
    return(out) 
}

# matching
library(Matching)
matching <- function(data, Y, treat, covar) {
  m.out <- Match(Y = data[, Y], Tr = data[, treat], X = data[, covar], Z = data[, covar], 
    estimand = "ATT", M = 5, replace = TRUE, ties = TRUE, BiasAdjust = TRUE)
  out <- c(m.out$est[1], m.out$se[1], m.out$est[1] - 1.96 * m.out$se[1], 
    m.out$est[1] + 1.96 * m.out$se[1])
  return(out)
}

psm <- function(data, Y, treat, covar) {
  ps <- probability_forest(X = data[, covar], 
    Y = as.factor(data[,treat]), seed = 1234, num.trees = 4000)$predictions[,2]
  m.out <- Match(Y = data[, Y], Tr = data[, treat], X = matrix(ps, nrow(data), 1), 
    estimand = "ATT", M = 5, replace = TRUE, ties = FALSE, BiasAdjust = FALSE)
  if (is.null(m.out$se)==FALSE) {
    se <- m.out$se[1]
  } else {
    se <- m.out$se.standard[1] 
  }
  out <- c(m.out$est[1], se, m.out$est[1] - 1.96 * se, 
    m.out$est[1] + 1.96 * se)
  return(out)
}


# OM (reg)
om.reg <- function(data, Y, treat, covar) {
  tr <- which(data[, treat] == 1)
  co <- which(data[, treat] == 0)
  fml <- as.formula(paste(Y, "~", paste(covar, collapse = " + ")))
  out.co <- lm(fml, data = data[co, ])
  Y.tr.hat <- predict(out.co, newdata = data[tr, covar, drop = FALSE])
  newdata <- cbind.data.frame(Y = c(data[tr, Y], Y.tr.hat), treat = rep(c(1, 0), each = length(tr)))
  out <- summary(lm_robust(Y ~ treat, data = newdata, se_type = "stata"))$coefficients["treat", c(1, 2, 5, 6)]
  return(out)
}

# OM (grf)
library(grf)
om.grf <- function(data, Y, treat, covar) {
  tr <- which(data[, treat] == 1)
  co <- which(data[, treat] == 0)
  out.co <- regression_forest(X = data[co, covar, drop = FALSE], Y = as.vector(data[co, Y]) )
  Y.tr.hat <- as.vector(unlist(predict(out.co, newdata = data[tr, covar, drop = FALSE])))
  newdata <- cbind.data.frame(Y = c(data[tr, Y], Y.tr.hat), treat = rep(c(1, 0), each = length(tr)))
  out <- summary(lm_robust(Y ~ treat, data = newdata, se_type = "stata"))$coefficients["treat", c(1, 2, 5, 6)]
  return(out)
}


# IPW
ipw <- function(data, Y, treat, covar) {
  ps <- probability_forest(X = data[, covar, drop = FALSE], Y = as.factor(data[, treat]), seed = 1234)$predictions[,2]
  fml <- as.formula(paste(Y, "~", treat))
  weights <- rep(1, nrow(data))
  co <- which(data[, treat] == 0)
  weights[co] <- ps[co]/(1-ps[co])
  out <- summary(lm_robust(fml, data = data, weights = weights, se_type = "stata"))$coefficients[treat, c(1, 2, 5, 6)]
  # extract coef, se, ci.lower, ci.upper
  return(out) 
}

# CBPS
library("CBPS")
cbps <- function(data, Y, treat, covar) {
  fml <- as.formula(paste(treat, "~", paste(covar, collapse = " + ")))
  ps <- quiet(CBPS(fml, data = data, standardize = TRUE)$fitted.values)
  fml <- as.formula(paste(Y, "~", treat))
  weights <- rep(1, nrow(data))
  co <- which(data[, treat] == 0)
  weights[co] <- ps[co]/(1-ps[co])
  out <- summary(lm_robust(fml, data = data, weights = weights, se_type = "stata"))$coefficients[treat, c(1, 2, 5, 6)]
  return(out)
}

# ebal
library(hbal)
ebal <- function(data, Y, treat, covar) {
  ebal.out <- hbal::hbal(Y = Y, Treat = treat, X = covar,  data = data, expand.degree = 1)
  out <- hbal::att(ebal.out, dr = FALSE)[1, c(1, 2, 5, 6)]
  return(out)
}

# hbal
hbal <- function(data, Y, treat, covar) {
  hbal.out <- hbal::hbal(Y = Y, Treat = treat, X = covar,  data = data, expand.degree = 2, cv = TRUE)
  out <- hbal::att(hbal.out, dr = FALSE)[1, c(1, 2, 5, 6)]
  return(out)
}


# AIPW-GRF
aipw <- function(data, Y, treat, covar) {
  library("grf")
  for (var in c(Y, treat, covar)) {
    data[, var] <- as.vector(data[, var])
  }
  c.forest <- causal_forest(X = data[, covar, drop = FALSE], Y = data[, Y], 
    W = data[, treat], seed = 1234)
  att <- average_treatment_effect(c.forest, target.sample = "treated", method = "AIPW")
  att <- c(att, att[1] - 1.96 * att[2], att[1] + 1.96 * att[2])
  return(att)
}

aipw.match <- function(data, Y, treat, covar) {
  # match on ps
  ps <- probability_forest(X = data[, covar], Y = as.factor(data[, treat]), seed = 1234)$predictions[,2]
  m.out <- Match(Y = data[, Y], Tr = data[, treat], X = ps, 
    estimand = "ATT", M = 1, replace = FALSE, ties = FALSE, BiasAdjust = FALSE)
  mb <- quiet(MatchBalance(treat ~ ps, data = data, match.out = m.out, nboots= 0))
  ks <- mb$AfterMatching[[1]]$ks$ks$statistic
  s <- data[c(m.out$index.treated, m.out$index.control), ]
  out <- aipw(s, Y, treat, covar)
  #return(out)
  return(c(out, ks))
}

                                 
                                 
#       $$\               $$\ 
#       $$ |              $$ |
#  $$$$$$$ |$$$$$$\$$$$\  $$ |
# $$  __$$ |$$  _$$  _$$\ $$ |
# $$ /  $$ |$$ / $$ / $$ |$$ |
# $$ |  $$ |$$ | $$ | $$ |$$ |
# \$$$$$$$ |$$ | $$ | $$ |$$ |
#  \_______|\__| \__| \__|\__|
                         
                                 

### This script checks for robustness by estimating original model
### using double/debiased machine learning using DoubleML package
dml <-function(data, Y = NULL, treat = NULL, covar = NULL, clust_var = NULL){
  
  if(is.null(covar)){
    stop("No controls in specification.")
  }
  
  require(DoubleML)
  require(mlr3learners)
  require(fixest)
  require(ggplot2)

  n_obs = nrow(data)
  n_vars = length(covar)

  learner = lrn("regr.glmnet", lambda = sqrt(log(n_vars)/(n_obs)))
  ml_l = learner$clone()
  ml_m = learner$clone()
  
  if(is.null(clust_var) == TRUE){
    
    dat = data[,c(Y,treat,covar)]
    dat = na.omit(dat)
    
    dml_dat = DoubleMLData$new(dat,
                               y_col = Y,
                               d_cols = treat,
                               use_other_treat_as_covariate = FALSE,
                               x_cols = covar)
    
  }else{
    
    dat = data[,c(Y, treat, covar, clust_var)]
    dat[,clust_var] = as.numeric(factor(dat[,clust_var]))
    dat = dat[is.na(dat[,Y]) == FALSE,]
    dat = dat[is.na(dat[,D]) == FALSE,]
    features = data.frame(model.matrix(formula(paste(c('~ 1',treat,covar), collapse="+")), dat))
    dat = cbind(dat[,c(Y,clust_var)],features)
    
    dml_dat = DoubleMLClusterData$new(dat,
                                      y_col = Y,
                                      d_cols = treat,
                                      cluster_cols = clust_var,
                                      use_other_treat_as_covariate = FALSE,
                                      x_cols = covar)
  }
  
  # Set active treatment treatment
  dml_dat$set_data_model(treat)
  
  # Estimate with DML
  set.seed(pi)
  dml_mod = DoubleMLPLR$new(dml_dat, ml_l=ml_l, ml_m=ml_m)
  quiet(dml_mod$fit())
  out = c(dml_mod$coef[treat], dml_mod$se[treat], dml_mod$confint()[treat,])
  
  return(out)
  
}


                                                                         
                                                                         
# $$\  $$\  $$\  $$$$$$\  $$$$$$\   $$$$$$\   $$$$$$\   $$$$$$\   $$$$$$\  
# $$ | $$ | $$ |$$  __$$\ \____$$\ $$  __$$\ $$  __$$\ $$  __$$\ $$  __$$\ 
# $$ | $$ | $$ |$$ |  \__|$$$$$$$ |$$ /  $$ |$$ /  $$ |$$$$$$$$ |$$ |  \__|
# $$ | $$ | $$ |$$ |     $$  __$$ |$$ |  $$ |$$ |  $$ |$$   ____|$$ |      
# \$$$$$\$$$$  |$$ |     \$$$$$$$ |$$$$$$$  |$$$$$$$  |\$$$$$$$\ $$ |      
#  \_____\____/ \__|      \_______|$$  ____/ $$  ____/  \_______|\__|      
#                                  $$ |      $$ |                          
#                                  $$ |      $$ |                          
#                                  \__|      \__|                          
                                                                         
                                                                                                                                          
                                                                 
## estimate all
estimate_all <- function(data, Y, treat, covar, 
    methods = c("diff", "reg", "om.reg", "om.grf",
      "matching", "psm", "ipw", "cbps", "ebal", 
      "dml", "aipw_grf")) {
  
  results <- as.data.frame(matrix(NA, length(methods), 4))
  rownames(results) <- methods
  colnames(results) <- c("Estimate", "SE", "CI_lower", "CI_upper")
  m <- 1
  if ("diff" %in% methods) {
    results[m, ] <- diff(data, Y, treat) 
    m <- m + 1
  }
  if ("reg" %in% methods) {
    results[m, ] <- reg(data, Y, treat, covar) 
    m <- m + 1
  }
  if ("om.reg" %in% methods) {
    results[m, ] <- om.reg(data, Y, treat, covar) 
    m <- m + 1
  }
  if ("om.grf" %in% methods) {
    results[m, ] <- om.grf(data, Y, treat, covar) 
    m <- m + 1
  } 
  if ("matching" %in% methods) {
    results[m, ] <- matching(data, Y, treat, covar) 
    m <- m + 1
  }
  if ("psm" %in% methods) {
    results[m, ] <- psm(data, Y, treat, covar) 
    m <- m + 1
  }  
  if ("ipw" %in% methods) {
    results[m, ] <- ipw(data, Y, treat, covar) 
    m <- m + 1
  }
  if ("cbps" %in% methods) {
    results[m, ] <- cbps(data, Y, treat, covar) 
    m <- m + 1
  }
  if ("ebal" %in% methods) {
    results[m, ] <- quiet(ebal(data, Y, treat, covar))
    m <- m + 1
  }
  # if ("hbal" %in% methods) {
  #   results[m, ] <- quiet(hbal(data, Y, treat, covar))
  #   m <- m + 1
  # }
  if ("dml" %in% methods) {
    results[m, ] <-dml(data, Y, treat, covar) 
    m <- m + 1
  }
  if ("aipw_grf" %in% methods) {
    results[m, ] <- aipw(data, Y, treat, covar) 
    m <- m + 1
  }
  return(results)
}


#                     $$\     $$\     
#                     $$ |    $$ |    
#  $$$$$$$\ $$$$$$\ $$$$$$\ $$$$$$\   
# $$  _____|\____$$\\_$$  _|\_$$  _|  
# $$ /      $$$$$$$ | $$ |    $$ |    
# $$ |     $$  __$$ | $$ |$$\ $$ |$$\ 
# \$$$$$$$\\$$$$$$$ | \$$$$  |\$$$$  |
#  \_______|\_______|  \____/  \____/ 
                                    
                                    
                                    

library(grf)
catt <- function(data, Y, treat, covar){
    tau.forest <- causal_forest(X = data[, covar], Y = data[, Y], 
        W = data[, treat], num.trees = 4000)
    tau0 <- average_treatment_effect(tau.forest, 
      target.sample = "treated", method = "AIPW")    
    tau <- tau.forest$predictions
    tau.tr <- tau[which(data[, treat]==1)] 
    return(list(catt = tau.tr, att = tau0))
}

                                                                  
#            $$\               
#            $$ |              
#  $$$$$$\ $$$$$$\    $$$$$$\  
# $$  __$$\\_$$  _|  $$  __$$\ 
# $$ /  $$ | $$ |    $$$$$$$$ |
# $$ |  $$ | $$ |$$\ $$   ____|
# \$$$$$$$ | \$$$$  |\$$$$$$$\ 
#  \____$$ |  \____/  \_______|
#       $$ |                   
#       $$ |                   
#       \__|                   





library("qte")
est_qte <- function(Y, treat, covar, data, ps = TRUE,
  probs = seq(0.05,0.95,0.05), cores = 20,
  ylim = NULL) {
    # Set up
    if (is.null(covar)) {
      formla <- as.formula(paste(Y, "~", treat))
    } else {
       formla <- as.formula(paste(Y, "~", treat, "+", paste(covar, collapse = "+")))
    }
    if (is.null(covar) | ps == FALSE) {
      mod <- ci.qtet(formla, xformla = NULL, data = data, 
        probs = probs, se = TRUE, iters = 1000, pl = TRUE, cores = cores)
    } else {
      xformla <- as.formula(paste("~", paste(covar, collapse = "+")))
      mod <- ci.qtet(formla, xformla = xformla, data = data, 
        probs = probs, se = TRUE, iters = 1000, pl = TRUE, cores = cores)      
    }
    return(mod)
}                                                                      
     

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                      
#                                                         $$\ 
#                                                         $$ |
#  $$$$$$\  $$\   $$\  $$$$$$\   $$$$$$\  $$$$$$$\   $$$$$$$ |
# $$  __$$\ \$$\ $$  |$$  __$$\  \____$$\ $$  __$$\ $$  __$$ |
# $$$$$$$$ | \$$$$  / $$ /  $$ | $$$$$$$ |$$ |  $$ |$$ /  $$ |
# $$   ____| $$  $$<  $$ |  $$ |$$  __$$ |$$ |  $$ |$$ |  $$ |
# \$$$$$$$\ $$  /\$$\ $$$$$$$  |\$$$$$$$ |$$ |  $$ |\$$$$$$$ |
#  \_______|\__/  \__|$$  ____/  \_______|\__|  \__| \_______|
#                     $$ |                                    
#                     $$ |                                    
#                     \__|                                    
                                                            
                                                            
                                                                                                                        
# expand covariates for sample selection
expand <- function(data) {
  data$married0 <- ifelse(data$married == 0, 1, 0)
  data$married1 <- ifelse(data$married == 1, 1, 0)
  data$race0 <- ifelse(data$black == 0 & data$hispanic == 0, 1, 0)
  data$race1 <- ifelse(data$black == 1, 1, 0)
  data$race2 <- ifelse(data$hispanic == 1, 1, 0)
  data$age0 <- ifelse(data$age <= 20, 1, 0)
  data$age1 <- ifelse(data$age > 20 & data$age <= 24, 1, 0)
  data$age2 <- ifelse(data$age > 24 & data$age <= 28, 1, 0)
  data$age3 <- ifelse(data$age > 28, 1, 0)
  data$education0 <- ifelse(data$education <= 9, 1, 0)
  data$education1 <- ifelse(data$education == 10, 1, 0)
  data$education2 <- ifelse(data$education == 11, 1, 0)
  data$education3 <- ifelse(data$education >= 12, 1, 0)
  data$re740 <- ifelse(data$u74 == 1, 1, 0)
  data$re741 <- ifelse(data$re74 > 0 & data$re74 <= 5605, 1, 0)
  data$re742 <- ifelse(data$re74 > 5605, 1, 0)
  data$re750 <- ifelse(data$u75 == 1, 1, 0)
  data$re751 <- ifelse(data$re75 > 0 & data$re75 <= 2613, 1, 0)
  data$re752 <- ifelse(data$re75 > 2613, 1, 0)
  return(data)
}

# quantile(nsw_dw$age, probs = c(0.25, 0.5, 0.75))
