# ddpcr: Analysis and visaulization of Digital Droplet PCR data in R and on the web

This package provides an interface to explore, analyze, and visualize droplet digital PCR (ddPCR) data in R. An interactive tool was also created and is available online to facilitate this analysis for anyone who is not comfortable with using R.

## Background

Droplet Digital PCR (ddPCR) is a technology provided by Bio-Rad for performing digital PCR. The basic workflow of ddPCR involves partioning sample DNA into 20,000 droplets, PCR amplifying the nucleic acid in each droplet, and finally passing the droplets through a reader that detects flurescent intensities in two different wavelengths corresponding to FAM and HEX dyes. As a result, the data obtained from a ddPCR experiment can be visualized as a 2D scatterplot (one dimension is FAM intensity and the other dimension is HEX intensity) with 20,000 points (each droplet represents a point).

ddPCR experiments can be defined as singleplex, duplex, or multiplex depending on the number of dyes used (one, two, and more than two, respectively). A duplex experiment typically uses one FAM dye and one HEX dye, and consequently the droplets will be grouped into one of four clusters: double-negative (not emitting fluorescence from either dye), HEX-positive, FAM-positive, or double-positive. When plotting the droplets, each quadrant of the plot corresponds to a cluster; for example, the droplets in the lower-left quadrant are the double-negative droplets.

After running a ddPCR experiment, a key step in the analysis is gating the droplets to determine how many droplets belong to each cluster. Bio-Rad provides an analysis program called QuantaSoft which can be used to perform gating. QuantaSoft can either do the gating automatically or allow the user to set the gates manually. Most ddPCR users currently gate their data manually because QuantaSoft's automatic gating often does a poor job and there are no other tools available for gating ddPCR data.



Analysis and visualization of droplet digital PCR, designed around a wild-type negative BRAF-V600 assay. Designed to be able to analyze duplex experiments with the following properties:

- most of the droplets are double-negative (empty droplets)
- most of the non-empty droplets are double-positive
- the secondary cluster of non-empty droplets is either HEX+ or FAM+

Most of the work is an improvement from the work [here](https://github.com/jennybc/haynes)
