# ddpcr: Analysis and visaulization of Digital Droplet PCR data in R and on the web

This package provides an interface to explore, analyze, and visualize droplet digital PCR (ddPCR) data in R. An interactive tool was also created and is available online to facilitate this analysis for anyone who is not comfortable with using R.

## Background

Droplet Digital PCR (ddPCR) is a technology provided by Bio-Rad for performing digital PCR. The basic workflow of ddPCR involves three main steps: partitioning sample DNA into 20,000 droplets, PCR amplifying the nucleic acid in each droplet, and finally passing the droplets through a reader that detects flurescent intensities in two different wavelengths corresponding to FAM and HEX dyes. As a result, the data obtained from a ddPCR experiment can be visualized as a 2D scatterplot (one dimension is FAM intensity and the other dimension is HEX intensity) with 20,000 points (each droplet represents a point).

ddPCR experiments can be defined as singleplex, duplex, or multiplex depending on the number of dyes used (one, two, and more than two, respectively). A duplex experiment typically uses one FAM dye and one HEX dye, and consequently the droplets will be grouped into one of four clusters: double-negative (not emitting fluorescence from either dye), HEX-positive, FAM-positive, or double-positive. When plotting the droplets, each quadrant of the plot corresponds to a cluster; for example, the droplets in the lower-left quadrant are the double-negative droplets.

After running a ddPCR experiment, a key step in the analysis is gating the droplets to determine how many droplets belong to each cluster. Bio-Rad provides an analysis software called QuantaSoft which can be used to perform gating. QuantaSoft can either do the gating automatically or allow the user to set the gates manually. Most ddPCR users currently gate their data manually because QuantaSoft's automatic gating often does a poor job and **there are no other tools available for gating ddPCR data**.

## Overview

The `ddpcr` package allows you to upload ddPCR data, perform some basic analysis (more on that later), explore characteristic of the data, and create customizable figures.

This tool was initially developed to automatically gate data for a particular ddPCR assay (the paper for that experiment is in progress), and any assay with similar characteristics can also use this tool to automatically gate the droplets. In order to benefit from the full automatic analysis, your ddPCR experiment needs to have these characteristics:  

- The experiment is a duplex ddPCR experiment
- The majority of droplets are empty (double-negative)
- The majority of non-empty droplets are double-positive
- There can be a third cluster of either FAM+ or HEX+ droplets

In other words, the built-in automatic gating will work when there are three clusters of droplets: (1) double-negative, (2) double-positive, and (3) either FAM+ or HEX+. These types of experiments will be referred to as *(FAM+)/(FAM+HEX+)* or  *(HEX+)/(FAM+HEX+)*. Here is what a typical well could look like from each of these experiments:

![Supported experiment types](vignettes/figures/supported-exp-types.png)

