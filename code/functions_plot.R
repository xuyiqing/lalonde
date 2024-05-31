# LaLonde (1986) after Nearly Four Decades: Lessons Learned
# Guido Imbens & Yiqing Xu
# 2024

## Functions for Plotting

#############################
# Function for plotting
#############################


#                                         $$\                     
#                                         $$ |                    
#  $$$$$$\ $$\    $$\  $$$$$$\   $$$$$$\  $$ | $$$$$$\   $$$$$$\  
# $$  __$$\\$$\  $$  |$$  __$$\ $$  __$$\ $$ | \____$$\ $$  __$$\ 
# $$ /  $$ |\$$\$$  / $$$$$$$$ |$$ |  \__|$$ | $$$$$$$ |$$ /  $$ |
# $$ |  $$ | \$$$  /  $$   ____|$$ |      $$ |$$  __$$ |$$ |  $$ |
# \$$$$$$  |  \$  /   \$$$$$$$\ $$ |      $$ |\$$$$$$$ |$$$$$$$  |
#  \______/    \_/     \_______|\__|      \__| \_______|$$  ____/ 
#                                                       $$ |      
#                                                       $$ |      
#                                                       \__|      

plot_hist <- function(data, var, treat, main = NULL, odds = FALSE,
    breaks = 40, density = TRUE, xlim = NULL, ylim = NULL,
    xlab = NULL, text.size = 1.5) {
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

#                                $$$$$$\  
#                               $$  __$$\ 
#  $$$$$$$\  $$$$$$\   $$$$$$\  $$ /  \__|
# $$  _____|$$  __$$\ $$  __$$\ $$$$\     
# $$ /      $$ /  $$ |$$$$$$$$ |$$  _|    
# $$ |      $$ |  $$ |$$   ____|$$ |      
# \$$$$$$$\ \$$$$$$  |\$$$$$$$\ $$ |      
#  \_______| \______/  \_______|\__|      

                                        
plot_coef <- function(out, 
    methods = c("diff", "reg", "om.reg", "om.grf", 
    "matching", "psm", "ipw", "cbps", "ebal", 
        "dml", "aipw_grf"),
    labels = c("Diff-in-Means", "Reg", "OM: Reg", "OM: GRF",
        "NN\nMatching", "PS\nMatching",
        "IPW", "CBPS", "Ebal", "DML-Elasnet", "AIPW-GRF"),
    main = NULL,
    ylab = "Estimate",
    band = NULL,
    line = NULL,
    grid = TRUE,
    main.pos = 1,
    main.line = -2,
    ylim = NULL
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
  mtext(main, main.pos, line = main.line, cex = 1.5)
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

plot_coef2 <- function(out, 
    methods = c("diff", "reg", "matching", "psm",
        "om.reg", "om.grf",
        "ipw", "cbps", "ebal", "hbal", 
        "dml", "aipw_grf"),
    labels = c("diff-in-means", "linear\nreg", "matching", "pscore\nmatching",
        "om: reg", "om: grf",
        "ipw", "cbps", "ebal", "hbal", 
        "dml_elasnet", "aipw_grf"),
    main = NULL,
    xlab = "Estimate",
    band = c(0, 3500),
    grid = TRUE,
    stats = TRUE,
    ylim = NULL
) {
  
  if (is.null(methods) == TRUE) {
    methods <- rownames(out)
  }
  
  if (is.null(labels) == TRUE) {
    labels <- methods
  }  
 
  # Data for the plot
  data <- out
  rg <- range(data[,c(3,4)], na.rm = TRUE)
  adj <- rg[2] - rg[1]
  if (is.null(xlim) == TRUE) {
    xlim  <- c(min(0, rg[1] - 0.3*adj), max(0, rg[2] + 0.35*adj))
  }
  adj2 <- xlim[2] - xlim[1] 
  
  # Set up the plot
  ncoefs <- length(methods)
  par(mar = c(4, 5, 3, 2))
  plot(data[, 1], 1: ncoefs, ylim = c(0.5, ncoefs + 0.5), xlim = xlim,
       ylab = "", xlab = "", main = main, 
       axes = FALSE, xaxt = "n", yaxt = "n", type = "n")
  axis(2, at = ncoefs: 1, labels =  labels, las = 1, cex.axis = 0.8)
  axis(1)
  mtext(xlab, 1, line = 2.5)
  # if (is.null(band) == FALSE) {
  #   rect(-0.5, band[1], ncoefs + 1, band[2], col = "#ff000030", border = "white") # label at bottom
  # }
  if (grid == TRUE) {
    abline(v = axTicks(2), lty = "dotted", col = "gray50")
    abline(h = c(0.5, c(1: ncoefs) + 0.5), lty = "dotted", col = "gray50") # horizontal grid
  }
  abline(v = 0, col = "red", lwd = 2, lty = "solid")
  segments(x0 = data[, 3], y0 = c(ncoefs: 1), x1 = data[, 4], y1 = c(ncoefs: 1), lwd = 2) #CI
  points(data[, 1], c(ncoefs: 1), pch = 16, col = 1, cex = 1.2) #point coefs
  box()
}


#                     $$\     $$\     
#                     $$ |    $$ |    
#  $$$$$$$\ $$$$$$\ $$$$$$\ $$$$$$\   
# $$  _____|\____$$\\_$$  _|\_$$  _|  
# $$ /      $$$$$$$ | $$ |    $$ |    
# $$ |     $$  __$$ | $$ |$$\ $$ |$$\ 
# \$$$$$$$\\$$$$$$$ | \$$$$  |\$$$$  |
#  \_______|\_______|  \____/  \____/ 
                                    
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
