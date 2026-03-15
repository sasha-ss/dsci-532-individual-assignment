# Mars Pressure Dashboard

A Shiny dashboard that visualizes atmospheric pressure on Mars over time. Users can filter data by Martian month and see the average pressure alongside a line plot of pressure versus terrestrial date.

**Live App:** [Mars Pressure Dashboard](https://019ceffe-3eea-ec59-fa9e-988cbca77797.share.connect.posit.cloud)

## Purpose

This app allows users to explore Mars atmospheric pressure trends interactively. By selecting Martian months, users can identify variations in pressure and gain insights.

## Installation

**1. Clone the repository:**

``` bash
git clone https://github.com/sasha-ss/mars-pressure-dashboard.git
```

**2. Open app.R in RStudio (or any R IDE) and install required packages in R**

``` bash
install.packages(c("shiny", "dplyr", "readr", "bslib"))
```

**3. Launch the dashboard:**

Option 1: Click the Run App button in RStudio

Option 2: In the console, run:
``` bash
shiny::runApp("app.R")
```
