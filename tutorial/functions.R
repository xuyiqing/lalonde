# 1.1 Installation

# required packages
packages <- c("haven", "labelled", "Matching", "grf", "sensemakr", "qte",
              "estimatr", "CBPS", "hbal", "DoubleML", "mlr3learners", "mlr3","fixest", "ggplot2")

# install packages
install_all <- function(packages) {
  installed_pkgs <- installed.packages()[, "Package"]
  for (pkg in packages) {
    if (!pkg %in% installed_pkgs) {
      install.packages(pkg)
    }
  }
}

install_all(packages)

# load packages
library(haven)
library(labelled)
library(grf)
library(Matching)
library(estimatr)
library(hbal)
library(CBPS)
library(DoubleML)
library(mlr3learners)
library(mlr3)
library(fixest)
library(ggplot2)
library(qte)
library(sensemakr)

# 1.2.2 plot_hist(), love.plot() & assess_overlap()

plot_hist <- function(data, var, treat, main = NULL, odds = FALSE,
                      breaks = 40, density = TRUE, xlim = NULL, ylim = NULL,
                      xlab = NULL, text.size = 0.8) {
  ntr <- sum(data[, treat] == 1)
  nco <- sum(data[, treat] == 0)
  if (odds == TRUE) {
    data[, var] <- log(data[, var]/(1-data[, var]))
    if (is.null(xlab) == TRUE) {xlab <- "Log Odds"}
  } else {
    if (is.null(xlab) == TRUE) {xlab <- "Propensity Score"}
  }
  if (is.null(xlim)) {
    if (odds == TRUE) {
      xlim <- range(data[, var])
      cat(xlim)
    } else {
      xlim <- c(0,1)
    }
  }
  intervals <- seq(xlim[1], xlim[2], length.out = breaks + 1)
  h0 <- as.numeric(table(cut(data[data[,treat]==0, var],
                             breaks = intervals, include.lowest = TRUE)))
  h1 <- as.numeric(table(cut(data[data[,treat]==1, var],
                             breaks = intervals, include.lowest = TRUE)))
  if (density == TRUE) {
    h0 <- h0/sum(h0); h1 <- h1/sum(h1)
  }
  s <- cbind.data.frame(h0, h1)
  if (is.null(ylim)) {
    ylim.max <- max(s$h0, s$h1) * 1.2
    ylim <- c(-ylim.max, ylim.max)
  }
  par(mar = c(4, 4, 1, 1))
  barplot(s$h0 * -1, names.arg = sprintf("%.2f", intervals[-1]),
          col = "#AAAAAA80", main = main, cex.lab = 1.3,
          ylim = ylim, xlab = xlab, cex.axis = 1.2, cex.names = 1.2,
          ylab = "Density", border = NA, axes = TRUE)
  barplot(s$h1, col = "#ff000080", add = TRUE,
          border = NA, cex.axis = 1.2)
  abline(h = 0, col = "gray60", lty = 2, lwd = 1.5)
  axis(1, at = seq(1, 60, length.out = breaks/2), labels = FALSE)
  usr <- par("usr")
  user_x <- usr[1] + 0.03 * (usr[2] - usr[1])
  user_y <- usr[3] + 0.92 * (usr[4] - usr[3])
  text(user_x, user_y, paste("Ntr = ", ntr), pos = 4, cex = text.size)
  text(user_x, user_y - 0.05 * (usr[4] - usr[3]), paste("Nco = ", nco),
       pos = 4, cex = text.size)
  box()
}

assess_overlap <- function(data, treat, cov, odds = TRUE, num.trees = NULL, seed = 1234, breaks = 50, xlim = NULL, ylim = NULL) {
  if(is.null(num.trees))
  {
    p.forest1 <- probability_forest(X = data[, cov],
                                    Y = as.factor(data[,treat]), seed = seed)
  }
  else
  {
    p.forest1 <- probability_forest(X = data[, cov],
                                    Y = as.factor(data[,treat]), seed = seed, num.trees = num.trees)
  }
  data$ps_assoverlap <- p.forest1$predictions[,2]
  #range(lcps.plus$ps)
  data$ps_assoverlap[which(abs(data$ps_assoverlap) <= 1e-7)] <- 1e-7
  #range(lcps.plus$ps)
  if(odds == TRUE)
  {
    plot_hist(data, "ps_assoverlap", treat, odds = TRUE, breaks = breaks,
              density = TRUE, main = "", xlim = xlim, ylim = ylim)
  }
  else
  {
    plot_hist(data, "ps_assoverlap", treat, odds = FALSE, breaks = breaks,
              density = TRUE, main = "", xlim = c(0, 1), ylim = ylim)
  }
  return(data)
}

love.plot <- function(data_pre, data_post, treat, covar, threshold = 0.1, title = "Love.Plot") {
  
  standardized_diff <- function(data, treat, covar) {
    treated <- data[data[[treat]] == 1, ]
    control <- data[data[[treat]] == 0, ]
    
    std_diff <- sapply(covar, function(var) {
      mean_treated <- mean(treated[[var]], na.rm = TRUE)
      mean_control <- mean(control[[var]], na.rm = TRUE)
      sd_pooled <- sqrt((var(treated[[var]], na.rm = TRUE) + var(control[[var]], na.rm = TRUE)) / 2)
      (mean_treated - mean_control) / sd_pooled
    })
    
    return(std_diff)
  }
  
  std_diff_pre <- standardized_diff(data_pre, treat, covar)
  std_diff_post <- standardized_diff(data_post, treat, covar)
  
  love_data <- data.frame(
    Variable = rep(covar, 2),
    Std_Diff = c(std_diff_pre, std_diff_post),
    Matching = rep(c("Pre-Matching", "Post-Matching"), each = length(covar))
  )
  
  p <- ggplot(love_data, aes(x = Variable, y = Std_Diff, color = Matching)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
    geom_hline(yintercept = -threshold, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(title = title, x = "Covariates", y = "Standardized Mean Differences") +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1)
    )
  
  return(p)
}

# 1.2.3 psmatch()

psmatch <- function(data, Y, treat, cov, num.trees = 4000, seed = 1234, replace = FALSE, estimand = "ATT")
{
  set.seed(seed) # need to set seed b/c tie-breaking is random
  data$psmatch <- probability_forest(X = data[, cov],
                                     Y = as.factor(data[, treat]), seed = seed, num.trees = num.trees)$predictions[,2]
  mout <- Match(Y = data[,Y], Tr = data[,treat], X = data$psmatch, estimand = estimand, M = 1,
                BiasAdjust = FALSE, replace=replace, ties = FALSE)
  data <- data[c(mout$index.treated, mout$index.control), ]
  return(data)
}

# 1.2.4 estimate_all() & plot_coef()

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

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
#library(Matching)
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
#library(grf)
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
#library("CBPS")
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
#library(hbal)
ebal <- function(data, Y, treat, covar) {
  ebal.out <- hbal::hbal(Y = Y, Treat = treat, X = covar,  data = data, expand.degree = 1)
  out <- hbal::att(ebal.out, dr = FALSE)[1, c(1, 2, 5, 6)]
  return(out)
}

# hbal
# hbal <- function(data, Y, treat, covar) {
#   hbal.out <- hbal::hbal(Y = Y, Treat = treat, X = covar,  data = data, expand.degree = 2, # cv = TRUE)
#   out <- hbal::att(hbal.out, dr = FALSE)[1, c(1, 2, 5, 6)]
#   return(out)
# }


# AIPW
aipw <- function(data, Y, treat, covar) {
  #library("grf")
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

### This script checks for robustness by estimating original model
### using double/debiased machine learning using DoubleML package
dml <-function(data, Y = NULL, treat = NULL, covar = NULL, clust_var = NULL, ml_l = lrn("regr.lm"), ml_m = lrn("regr.lm")){
  
  if(is.null(covar)){
    stop("No controls in specification.")
  }
  
  #require(DoubleML)
  #require(mlr3learners)
  #require(fixest)
  #require(ggplot2)
  
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

# execute all estimators
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

plot_coef <- function(out, 
                      methods = c("diff", "reg", "om.reg", "om.grf", 
                                  "matching", "psm", "ipw", "cbps", "ebal", 
                                  "dml", "aipw_grf"),
                      labels = c("Diff-in-Means", "Reg", "OM: Reg", "OM: GRF",
                                 "NN\nMatching", "PS\nMatching",
                                 "IPW", "CBPS", "Ebal", "DML\nElasnet", "AIPW-GRF"),
                      main = NULL,
                      ylab = "Estimate",
                      band = NULL,
                      line = NULL,
                      grid = TRUE,
                      main.pos = 1,
                      main.line = -2,
                      ylim = NULL,
                      textsize = 1
) {
  
  if (is.null(methods) == TRUE) {
    methods <- rownames(out)
  }
  
  if (is.null(labels) == TRUE) {
    labels <- methods
  }
  
  # # check
  # if (is.null(out)==FALSE) {
  #   if (inherits(out, "ivDiag") == FALSE) {stop("\"out\" needs to be a \"ltz\" object.")}
  # }
  # 
  # # title
  # if (is.null(main)==TRUE) {
  #   main <- "Estimates with 95% CIs"
  # }
  
  
  # Data for the plot
  data <- out
  rg <- range(data[,c(3,4)], na.rm = TRUE)
  adj <- rg[2] - rg[1]
  if (is.null(ylim) == TRUE) {
    ylim  <- c(min(0, rg[1] - 0.3*adj), max(0, rg[2] + 0.35*adj))
  }
  adj2 <- ylim[2] - ylim[1] 
  
  # Set up the plot
  ncoefs <- length(methods)
  par(mar = c(2.5, 4, 1, 2))
  plot(1: ncoefs, data[, 1], xlim = c(0.5, ncoefs + 0.5), ylim = ylim,
       ylab = "", xlab = "", main = "", 
       axes = FALSE, xaxt = "n", yaxt = "n", type = "n")
  axis(1, at = 1: ncoefs, labels =  labels, las = 1, cex.axis = 0.8)
  axis(2, cex.axis = 0.7)
  mtext(main, main.pos, line = main.line, cex = textsize)
  mtext(ylab, 2, line = 2.5)
  if (is.null(band) == FALSE) {
    rect(-0.5, band[1], ncoefs + 1, band[2], col = "#ff000030", border = "white") # label at bottom
  }
  if (is.null(line) == FALSE) {
    abline(h = line, col = "red", lty = 2)
  }
  if (grid == TRUE) {
    abline(h = axTicks(2), lty = "dotted", col = "gray50")
    abline(v = c(0.5, c(1: ncoefs) + 0.5), lty = "dotted", col = "gray50") # horizontal grid
  }
  abline(h = 0, col = "red", lwd = 2, lty = "solid")
  segments(y0 = data[, 3], x0 = c(1: ncoefs), y1 = data[, 4], x1 = c(1: ncoefs), lwd = 2) #CI
  points(1: ncoefs, data[, 1], pch = 16, col = 1, cex = 1.2) #point coefs
  box()
}

# 1.2.5 catt() & plot_catt()

catt <- function(data, Y, treat, covar){
  tau.forest <- causal_forest(X = data[, covar], Y = data[, Y],
                              W = data[, treat], num.trees = 4000)
  tau0 <- average_treatment_effect(tau.forest,
                                   target.sample = "treated", method = "AIPW")
  tau <- tau.forest$predictions
  tau.tr <- tau[which(data[, treat]==1)]
  return(list(catt = tau.tr, att = tau0))
}

plot_catt <- function(catt1, catt2, att1, att2,
                      xlab = NULL, ylab = NULL, main = NULL, axes.range = NULL,
                      file = NULL, width = 7, height = 7) {
  
  if (is.null(axes.range)==TRUE) {
    axes.range <- range(c(catt1,catt2))
  }
  drange <- axes.range[2] - axes.range[1]
  axes.range <- axes.range + c(-0.1, 0.1) * drange
  den1 <- density(catt1)
  den2 <- density(catt2)
  max.den <- max(c(den1$y, den2$y))
  adj <- drange * 0.15 / max.den
  if (!is.null(file)) {
    pdf(file = file, width = width, height = height)
    par(mar = c(4, 4, 3, 2))
  }
  plot(1, xlim = axes.range, ylim = axes.range, type = "n",
       xlab = xlab, ylab = ylab, main = main)
  abline(h = 0, v = 0, col = "gray", lty = 3)
  abline(0, 1, col = "red", lty = 2)
  y1 <- adj * den1$y + axes.range[1] - drange*0.03
  polygon(den1$x,  y1, col="#AAAAAA50", border = NA)
  lines(den1$x, y1, col = "gray", lwd = 1)
  y2 <- adj * den2$y + axes.range[1] - drange*0.03
  polygon(y2, den2$x, col="#AAAAAA50", border = NA)
  lines(y2, den2$x, col = "gray", lwd = 1)
  points(catt1, catt2, cex = 0.5, col = "#AAAAAA80", pch = 16)
  points(catt1, catt2, cex = 0.5, col = "#777777", pch = 1, lwd = 0.5)
  if (is.null(att1) == FALSE) {
    points(att1, att2, cex = 2, col = 2, pch = 3, lwd = 2)
  }
  box()
  if (!is.null(file)) {graphics.off()}
}

# 1.2.6 est_qte() & plot_qte()

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

plot_qte <- function(mod, mod2 = NULL, bm = NULL, main= "", ylim = NULL,
                     col = NULL) {
  # ylim
  if (is.null(ylim)) {
    ylim <- range(c(mod$qte.lower, mod$qte.upper))
  }
  # Plot
  par(mar = c(3, 3, 1, 1))
  plot(1, type = "n", xlab = "", ylab = "",
       xlim = c(0, 1), ylim = ylim, axes = FALSE)
  box(); axis(1, at = seq(0.1, 0.9, 0.2)); axis(2)
  mtext("QTET", 2, line = 2)
  mtext("Quantile", 1, line = 2)
  abline(h = 0, lty = 1, lwd = 2, col = "gray")
  title(main, line = -1.5)
  # model 2
  if (is.null(mod2) == FALSE) {
    polygon(c(mod2$probs, rev(mod2$probs)), c(mod2$qte.lower, rev(mod2$qte.upper)),
            col = "#FC94AF50", border = NA)
  }
  # benchmark
  if (is.null(bm) == FALSE) {
    polygon(c(bm$probs, rev(bm$probs)), c(bm$qte.lower, rev(bm$qte.upper)),
            col = "#ADD8E680", border = NA)
  }
  # main
  if (is.null(col) == TRUE) {
    col1 <- "gray30"
    col2 <- "#AAAAAA90"
  } else {
    col1 <- col[1]
    col2 <- col[2]
  }
  polygon(c(mod$probs, rev(mod$probs)), c(mod$qte.lower, rev(mod$qte.upper)),
          col = col2, border = NA)
  if (is.null(mod2) == FALSE) {
    lines(mod2$probs, mod2$qte, col = "#DC143C80", lwd = 2)
    points(mod2$probs, mod2$qte, col = "#DC143C", pch = 17, cex = 0.8)
  }
  if (is.null(bm) == FALSE) {
    lines(bm$probs, bm$qte, col = 4, lwd = 2)
    points(bm$probs, bm$qte, col = 4, pch = 16)
  }
  lines(mod$probs, mod$qte, col = col1, lwd = 2)
  lines(mod$probs, mod$qte.lower, col = col1, lty = 3, lwd = 1.5)
  lines(mod$probs, mod$qte.upper, col = col1, lty = 3, lwd = 1.5)
  points(mod$probs, mod$qte, col = col1, pch = 16)
}

# 1.2.7 sens_ana()

sens_ana <- function(data, Y, treat, covar, bm = NULL, kd = 1)
{
  p.forest <- probability_forest(X = data[, covar],
                                 Y = as.factor(data[, treat]), seed = 1234, num.trees = 4000)
  data$ps_sens <- p.forest$predictions[,2]
  data <- subset(data, ps_sens > 0.1 & ps_sens < 0.9)
  fml <- as.formula(paste(Y, "~", treat, "+", paste(covar, collapse = "+")))
  mod <- lm(fml, data = data)
  sens <- sensemakr(model = mod, treatment = treat, benchmark_covariates = bm, kd = kd, sensitivity.of = "t-value")
  plot(sens)
}