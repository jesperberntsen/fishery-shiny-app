# Fishery Data Explorer – Shiny App

This Shiny app allows users to explore marine catch data from multiple regions (Large Marine Ecosystems).  
Users can compare landings and discards over time, and examine catch composition by commercial group.

## Data Source

The data used in this app comes from [Sea Around Us](https://www.seaaroundus.org),  
a research initiative at the University of British Columbia that provides global fisheries data by region and sector.

Data was downloaded for the following Large Marine Ecosystems (LMEs):  
- North Sea  
- Norwegian Sea  
- Barents Sea  
- Baltic Sea

## Features

- Select multiple regions to compare trends
- Toggle between Landings and Discards
- Stacked area plot showing composition by commercial group
- Clean visual theme (Flatly via `bslib`)

## How to Run the App

1. Open RStudio
2. Install required packages (only needed once):

```r
install.packages(c("shiny", "bslib", "ggplot2", "tidyverse"))


## Author

Jesper Rein Berntsen  
University of Bergen – BIO302 Course Project