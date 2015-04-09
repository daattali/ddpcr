plot <- function(
  superimpose = FALSE,
  wells = NA, samples = NA,
  showPoints = TRUE, showLowvsHigh = TRUE, showFullPlate = FALSE,
  showMTFreq = TRUE, mtFreqSize = 4,
  includeEmpty = FALSE, includeFailed = TRUE,
  failedCol = "black", emptyCol = "black",
  wtCol = "green", mtCol = "purple", rainCol = "black",
  failedBg = "#111111", unusedBg = "#FFFFFF", wtBg = "green", mtBg = "purple",
  pointsAlpha = 0.1, lowmtPointsAlpha = 0.5,
  failedBgAlpha = 0.7, mtwtBgAlpha = 0.1,
  xlab = "HEX", ylab = "FAM", title = NA,
  showGrid = FALSE, labelGrid = FALSE, print = TRUE) {
  
  metadata <- self$getMeta()
  wellData <- self$getData()
  
  if (self$getStatus() < STATUS_DROPLETS_CLASSIFIED) {
    showMTFreq = FALSE
  }
  
  # prepare the data to be plotted
  rownames(metadata) <- metadata$well
  metadata$row <- as.factor(metadata$row)
  metadata$col <- as.factor(metadata$col)
  wellData %<>%
    dplyr::left_join(dplyr::select(metadata, well, row, col), by = "well")  
  
  if (!includeEmpty) {
    wellData <- dplyr::filter(wellData, cluster != CLUSTER_EMPTY)
  }
  if (!includeFailed) {
    failedWells <- private$wellsFailed()
    if (length(failedWells) > 0) {
      metadata[failedWells, , drop = FALSE]$used = FALSE
      wellData <- dplyr::filter(wellData, !(well %in% failedWells))
    }
  }
  
  # if the user wants to only show certain wells/samples, dplyr::filter the data accordingly
  if (!all(is.na(wells))) {
    wells <- dplyr::filter(metadata, used, well %in% wells)$well
    if (length(wells) == 0) {
      stop("There are no wells to show.", call. = FALSE)
    }
  } else if (!all(is.na(samples))) {
    wells <- dplyr::filter(metadata, used, sample %in% samples)$well
    if (length(wells) == 0) {
      stop("There are no wells to show.", call. = FALSE)
    }
  } else {
    wells <- unique(wellData$well)
  }
  wellData %<>%  dplyr::filter(well %in% wells)
  if (any(!(metadata$well %in% wells))) {
    metadata[!(metadata$well %in% wells), ]$used <- FALSE
  }
  metadataUsed <- dplyr::filter(metadata, used)
  
  # define the colours of the clusters
  clusterColours <- c(failedCol, emptyCol, wtCol, mtCol, rainCol)
  
  # need to remove colours corresponding to clusters that don't exist in the dataset
  # because otherwise the colour order will be messed up
  if (length(unique(wellData$cluster)) < length(clusterColours)) {
    clustersExclude <- c()
    for (i in seq_along(clusterColours)) {
      if (nrow(dplyr::filter(wellData, cluster == (i-1))) == 0) {
        clustersExclude <- c(clustersExclude, i)
      }
    }
    clusterColours <- clusterColours[-clustersExclude]
  }
  
  # define the colours of the backgrounds of wells with high/low MT freq
  if (nrow(dplyr::filter(metadata, !hasMTcluster)) > 0) {
    clusterBgColours <- c(wtBg, mtBg)
  } else {
    clusterBgColours <- c(mtBg)
  }
  
  # extract the drops that are considered mutant without a significant mutant cluster
  mtDropsLowFreq <-
    wellData %>%
    dplyr::filter(well %in% dplyr::filter(metadataUsed, !hasMTcluster)$well) %>%
    dplyr::filter(cluster == CLUSTER_MT)
  
  # remove unused rows/columns from the plate
  if (!showFullPlate) {
    metadata %<>%
      dplyr::filter(col %in% unique(metadataUsed$col),
             row %in% unique(metadataUsed$row)) %>%
      dplyr::arrange(row, col)
  }
  
  # basic plot
  p <-
    ggplot2::ggplot() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  
  # superimpose all the data from all the wells onto one plot instead of a grid
  if (superimpose) {
    showPoints = TRUE
    showMTFreq = FALSE
    showLowvsHigh = FALSE
  }
  if (!superimpose) {
    p <- p +
      ggplot2::facet_grid(row ~ col)
  }
  
  # show unused wells
  if (!superimpose) {
    if (sum(!metadata$used, na.rm = T) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = dplyr::filter(metadata, !used),
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = unusedBg)
    }
  }
  # show the points
  if (showPoints) {
    p <- p +
      ggplot2::geom_point(
                 data = wellData,
                 ggplot2::aes(x = HEX, y = FAM, color = as.factor(cluster)),
                 alpha = pointsAlpha, show_guide = FALSE) +
      ggplot2::scale_color_manual(values = clusterColours)
    if (nrow(mtDropsLowFreq) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = mtDropsLowFreq, ggplot2::aes(HEX, FAM), alpha = lowmtPointsAlpha, col = mtCol)
    }
  }
  # show high vs low mutant frequency in background colour
  if (showLowvsHigh) {
    if (sum(metadataUsed$success, na.rm = TRUE) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = dplyr::filter(metadataUsed, success),
          ggplot2::aes(fill = hasMTcluster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
          alpha = mtwtBgAlpha, show_guide = FALSE) +
        ggplot2::scale_fill_manual(values = clusterBgColours)
    }
  }
  # show the failed ddPCR runs
  if (includeFailed && !superimpose) {
    if (sum(!metadataUsed$success, na.rm = TRUE) > 0) {
      p <- p +
        ggplot2::geom_rect(
          data = dplyr::filter(metadataUsed, !success),
          ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
          alpha = failedBgAlpha, fill = failedBg)
    }
  }
  # plot visual parameters
  if (!showGrid) {
    p <- p +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  }
  if (!labelGrid) {
    p <- p +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank())
  }
  if (!is.na(title)) {
    p <- p +
      ggplot2::ggtitle(title)
  }
  
  # so that R won't complain when we don't show points
  if (!showPoints) {
    p <- p +
      ggplot2::geom_text(data = metadata, ggplot2::aes(0, 0, label = ""))
  }
  
  # ensure the aspect ratio is a square for each well
  ggb <- ggplot2::ggplot_build(p)
  p <- p +
    ggplot2::coord_fixed(
      ratio = diff(ggb$panel$ranges[[1]]$x.range) / diff(ggb$panel$ranges[[1]]$y.range))
  
  # show the mutant frequency as text on the plot
  if (showMTFreq) {
    if (sum(metadataUsed$success, na.rm = TRUE) > 0) {
      midHex <- ggb$panel$ranges[[1]]$x.range[1] + diff(ggb$panel$ranges[[1]]$x.range) * 0.95
      midFam <- ggb$panel$ranges[[1]]$y.range[1] + diff(ggb$panel$ranges[[1]]$y.range) * 0.1
      hjust = 1
      vjust = 0
      
      # if there are no points on the plot, put the text in the center
      if (!showPoints) {
        midHex <- 0
        midFam <- 0
        hjust = 0.5
        vjust = 0.5
      }
      p <- p + 
        ggplot2::geom_text(
          data = dplyr::filter(metadataUsed, success),
          ggplot2::aes(label = paste0(mtFreq, "%")),
          x = midHex, y = midFam, hjust = hjust, vjust = vjust,
          fontface = "bold", size = mtFreqSize, show_guide = FALSE)
    }
  }
  
  if (print) {
    print(p)
  }
  
  rows <- ifelse(superimpose, 5, metadata$row %>% unique %>% length)
  cols <- ifelse(superimpose, 5, metadata$col %>% unique %>% length)
  
  invisible(list(p = p, cols = cols, rows = rows))
}

Plate$set("public", "plot", plot)