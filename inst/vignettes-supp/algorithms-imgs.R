# This script generates images that are used in the algorithms vignette 

library(ggplot2)
library(grid)
library(gridExtra)
library(ddpcr)

plotWithLabel <- function(p, label) {
  labelFont <- gpar(col="black", fontsize=20, fontfamily="Times Roman", fontface="bold")
  arrangeGrob(p, top = textGrob(label,
                                x = unit(0, "npc"),
                                y = unit(1, "npc"),
                                just = c("left","top"),
                                gp = labelFont))
}

# Remove outliers step image

plate <- new_plate(sample_data_dir()) %>% subset("b6") %>%
  `plate_data<-`(plate_data(.) %>% dplyr::filter(FAM < 11000))
full <- plate_data(plate)
ndrops <- nrow(full)
top1 <- full %>% dplyr::arrange(desc(HEX)) %>% head(ndrops/100)
p <- ggplot(full, aes(HEX, FAM)) +
  geom_point(alpha = 0.2, size = 2) + theme_classic(15)
line <- quantile(full$HEX, .75) + 5*IQR(full$HEX)
pline <- p + geom_vline(xintercept = line, linetype = 2)
p1 <- pline
p2 <- ggplot(full, aes(HEX, FAM)) +
  geom_point(colour = "white") + theme_classic(15) +
  geom_point(data = top1, alpha = 0.3, size = 2)
line <- quantile(top1$HEX, .75) + 5*IQR(top1$HEX)
p2 <- p2 + geom_vline(xintercept = line, linetype = 2)
p3 <- p + geom_vline(xintercept = line, linetype = 2) +
  geom_point(data = top1 %>% dplyr::filter(HEX > 10000),
             alpha = 0.7, size = 2, colour = "red")
p1 <- plotWithLabel(p1, "A")
p2 <- plotWithLabel(p2, "B")
p3 <- plotWithLabel(p3, "C")
png(file.path("inst", "vignettes-supp", "outliers.png"), width = 1000, height = 300)
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()


# Remove empty droplets step image

plate <- new_plate(sample_data_dir()) %>% subset("b6") %>%
  `plate_data<-`(plate_data(.) %>%dplyr::filter(FAM < 11000, HEX < 10000))
full <- plate_data(plate)
quiet( mixmdl <- mixtools::normalmixEM(full$FAM, k = 2) )
smaller_comp <- mixmdl$mu %>% which.min
cutoff <- mixmdl$mu[smaller_comp] + params(plate, 'REMOVE_EMPTY', 'CUTOFF_SD') * mixmdl$sigma[smaller_comp]
p <- ggplot(full, aes(HEX, FAM)) +
  geom_point(alpha = 0.2, size = 2) + theme_classic(15)
pline <- p + geom_hline(yintercept = mixmdl$mu) +
  geom_hline(yintercept = cutoff, linetype = 2)
p1 <- ggExtra::ggMarginal(pline, margins = "y")
p2 <- ggplot(full %>% dplyr::filter(FAM > cutoff), aes(HEX, FAM)) +
  geom_point(alpha = 0.2, size = 2) + theme_classic(15) +
  geom_hline(yintercept = cutoff, linetype = 2) +
  geom_point(data = full %>% dplyr::filter(FAM < cutoff),
             colour = "red", alpha=0.2, size = 2)
p1 <- plotWithLabel(p1, "A")
p2 <- plotWithLabel(p2, "B")
png(file.path("inst", "vignettes-supp", "empty.png"), width = 700, height = 300)
grid.arrange(p1, p2, ncol = 2)
dev.off()