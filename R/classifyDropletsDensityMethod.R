analyzeWellClustersDensity <- function(wellNum, plot = FALSE) {

  wellDataSingle <- private$getSingleWell(wellNum)
  
  set.seed(SEED)
  quiet(
    mixmdlF <- mixtools::normalmixEM(wellDataSingle$FAM, k = 2))
  largerComp <- which.max(mixmdlF$mu)
  clborders <- mixmdlF$mu[largerComp] +
    mixmdlF$sigma[largerComp]*PARAMS$ASSIGN_CLUSTERS$CLUSTERS_BORDERS_NUM_SD*c(-1,1)
  
  top <- subset(wellDataSingle, FAM > clborders[1] & FAM < clborders[2])
  
  bwOrig <- TRUE  # whether or not the result is found using the first attempted bandwidth
  prevAdj <- NULL # previous "adjust" value
  finalAdj <- NULL # final "adjust" value
  for (adj in seq(5, 20, 0.5)) {
    densSmooth <- density(top$HEX, bw = "sj", adjust = adj)
    inflections <- getInflectionPts(densSmooth)
    maxima <- localMaxima(densSmooth$y)
    
    # ensure the right peak/inflection points are covering the wildtype drops
    # and not a couple outliers
    while(TRUE) {
      wtborders <- densSmooth$x[tail(inflections, 2)]
      wtdrops <- top %>% dplyr::filter(HEX %btwn% wtborders)
      if (nrow(wtdrops) < nrow(top)*0.1) {
        # discard rightmost inflection points
        inflections <- head(inflections, -2)
        if (densSmooth$x[tail(maxima, 1)] %btwn% wtborders) {
          maxima <- head(maxima, -1)
        }
      } else {
        break
      }
    }
    
    numPeaks <- length(maxima)
    numIP <- length(inflections)
    if (numPeaks == 1) {
      if (!bwOrig) {
        adj <- prevAdj
        densSmooth <- density(top$HEX, bw = "sj", adjust = adj)
        inflections <- getInflectionPts(densSmooth)
        maxima <- localMaxima(densSmooth$y)
        numPeaks <- length(maxima)
        numIP <- length(inflections)
      }
      finalAdj <- adj
      break
    }
    if (numPeaks == 2 && numIP == 4) {
      finalAdj <- adj
      break
    }
    prevAdj <- adj
    bwOrig <- FALSE
  }
  if (is.null(finalAdj)) {
    stop(sprintf("Could not analyse well %s", wellNum), call. = FALSE)
  }
  
  if (numPeaks == 1) {
    mtborders <- c(0, 0)
  } else {
    # use the IP to the right of the left-most peak
    leftPeak <- densSmooth$x[maxima][1]
    IPs <- densSmooth$x[inflections]
    rightBorder <- IPs[min(which(IPs > leftPeak))]
    mtborders <- c(0, rightBorder)
  }
  
  wtborders <- densSmooth$x[tail(inflections, 2)]
  mtdrops <- top %>% dplyr::filter(HEX %btwn% mtborders)
  wtdrops <- top %>% dplyr::filter(HEX %btwn% wtborders)
  mtfreq <- round(nrow(mtdrops) / (nrow(mtdrops) + nrow(wtdrops)) * 100, 3)
  
  if (mtfreq > 1) {
    mtborders[2] <- mtborders[2] + (wtborders[1] - mtborders[2]) * 0.1
  }
  
  wtborders[1] <- mtborders[2] + 1
  mtdrops <- top %>% dplyr::filter(HEX <= mtborders[2]) %>% dplyr::mutate(cluster = CLUSTER_MT)
  wtdrops <- top %>% dplyr::filter(HEX > mtborders[2]) %>% dplyr::mutate(cluster = CLUSTER_WT)
  
  mtFreq <- signif(nrow(mtdrops) / (nrow(mtdrops) + nrow(wtdrops)) * 100, 3)
  
  if (plot) {
    graphics::plot(
      wellDataSingle,
      main = sprintf("%s (%s%%)\nadj=%s (bw: %s)", wellNum, mtFreq, finalAdj, round(densSmooth$bw)),
      xlab = paste0("max: ", paste(round(densSmooth$x[maxima]), collapse = ", "),
                    "\ninf: ", paste(round(densSmooth$x[inflections]), collapse = ", ")))
    points(mtdrops, col = "red")
    points(wtdrops, col = "green")
    abline(v = densSmooth$x[inflections])
    abline(v = densSmooth$x[maxima], col = "grey")
  }

  wellDataSingle %<>%
    dplyr::mutate(cluster = CLUSTER_RAIN) %>%
    dplyr::left_join(wtdrops, by = c("HEX", "FAM")) %>%
    dplyr::mutate(cluster = ifelse(is.na(cluster.y), cluster.x, cluster.y)) %>%
    dplyr::select(-cluster.x, -cluster.y) %>%
    dplyr::left_join(mtdrops, by = c("HEX", "FAM")) %>%
    dplyr::mutate(cluster = ifelse(is.na(cluster.y), cluster.x, cluster.y)) %>%
    dplyr::select(-cluster.x, -cluster.y)
  hasMTcluster <- (mtFreq > 5 | nrow(mtdrops) > 30)
  msg <- NA
  
  return(list(result = wellDataSingle,
              hasMTcluster = hasMTcluster,
              comment = msg))
}

localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

#+ define-getInflectionPts, include = FALSE
getInflectionPts <- function(dat) {
  spl <- smooth.spline(x = dat$x, y = dat$y)
  secondDer <- predict(spl, deriv = 2)
  secondDerSigns <- sign(secondDer$y)
  signSameRuns <- rle(secondDerSigns)$lengths
  signChangesIdx <- cumsum(signSameRuns)
  bogusIdx <- c(1, length(dat$x)-1, length(dat$x))
  signChangesIdx <- signChangesIdx[!signChangesIdx %in% bogusIdx]
  signChangesIdx
}

Plate$set("private", "analyzeWellClustersDensity", analyzeWellClustersDensity)