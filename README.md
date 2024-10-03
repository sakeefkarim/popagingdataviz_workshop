# `PopAging DataViz`: Visualizing Population Data in `R`

### A Three-Day Workshop at Florida State University

<h2 style="display: inline-block;">About the Workshop</h2>
<a href="https://popagingdataviz.com/" target="_blank">
    <img src="https://github.com/sakeefkarim/popagingdataviz_workshop/raw/main/misc/logo.png" align="right" width="150" style="margin-left: 20px;">
</a>


**PopAging DataViz** is a short course (or workshop) on visualizing population data within `R`'s graphical ecosystem. During the workshop, we will be using the [`ggplot2`](https://ggplot2.tidyverse.org/) library as our workhorse. Over the course of three days, workshop participants will be exposed to many of the principles undergirding the ***Grammar of Graphics*** framework for visualizing data --- a grammar that serves as the lifeblood of `ggplot2` and its many extensions.

Then, the course will slowly build in complexity. Concretely, participants will be exposed to modules on (1) generating population pyramids and other plots that are germane to population research (e.g., Lexis diagrams); (2) leveraging interactivity and animations to bring data visualizations to life; and (3) visualizing statistical quantities of substantive interest (e.g., average marginal effects, adjusted predictions) to clarify model results.

The workshop will draw on a range of packages---including, but not limited to, `tidycensus`, `leaflet`, `mapview` and `cansim`---as well as a series of hands-on exercises designed to concretize high-level concepts and principles related to data visualization. All workshop materials are available (and will be maintained) on this repository and the [course website](https://popagingdataviz.com/).

### Sponsors

This short course is being co-hosted by the Consortium on Analytics for Data-Driven Decision-Making ([CAnD3](https://www.mcgill.ca/cand3/)) and our partners at Florida State University, including the [Pepper Institute on Aging and Public Policy](https://pepperinstitute.fsu.edu/), the [Center for Demography and Population Health](https://coss.fsu.edu/popcenter/), and the [College of Social Sciences and Public Policy](https://coss.fsu.edu/).

CAnD3 is hosted at McGill University and supported in part by funding from the [Social Sciences and Humanities Research Council of Canada](https://www.sshrc-crsh.gc.ca/home-accueil-eng.aspx) (SSHRC).

### Inspirations

Cédric Scherer's course on [`Graphic Design with ggplot2`](https://rstudio-conf-2022.github.io/ggplot2-graphic-design/), Andrew Heiss' course on [`Data Visualization with R`](https://datavizf23.classes.andrewheiss.com/), and the third edition of [`ggplot2: Elegant Graphics for Data Analysis`](https://ggplot2-book.org/) all serve as inspirations for this three-day workshop.

## Preliminaries

### Programming Language

As noted, this short course will be centred around---or anchored to---the `R` programming language for statistical computing and visualization. If you do not have `R` on your machine, please download the open-source software by clicking [here](https://cran.r-project.org/) before following the relevant prompts.

### Integrated Development Environment

Participants are encouraged to run their code using `RStudio`,[^readme-1] a powerful open-source IDE optimized for `R`. If you do not have `RStudio` on your machine, you can download the application by clicking [here](https://posit.co/download/rstudio-desktop/).

[^readme-1]: Participants should, of course, feel free to use other IDEs, `R` environments, or source-code editors to run the material featured in this short course.

### Packages

Thanks to the wonderful `renv` package, simply running `renv::restore()` before executing any lines of code in your source-code editor should ensure that all the packages needed for this short course are automatically available to you.

### API Keys

You'll need to obtain valid API keys to use the `tidycensus` and `cancensus` libraries.

-   Instructions for obtaining a CensusMapper API key (to access `cancensus` data)---and storing it on your system---can be found [here](https://github.com/mountainMath/cancensus#api-key).

-   Instructions for obtaining an API key from the US Census Bureau---and storing it on your system---can be found [here](https://walker-data.com/tidycensus/articles/basic-usage.html).

### Fonts

Please download and install the `IBM Plex Sans` and `Inconsolata` typefaces via [Google Fonts](https://fonts.google.com/) or by downloading the compressed file embedded below:

<a href="https://github.com/sakeefkarim/popagingdataviz_workshop/raw/main/misc/fonts.zip"> `fonts.zip`</a> <i class="fa-solid fa-download"></i>
