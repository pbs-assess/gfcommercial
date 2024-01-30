# gfcommercial
A review of biological samples collected from commercial groundfish fisheries in British Columbia, 1996-2022


## Overview
This commercial biosampling compendium uses an automated workflow to generate 
figure and text summaries of groundfish commercial biosampling in British 
Columbia, Canada. Figure and text summaries are then formatted as a CSAS 
Technical Report using the R package [csasdown](https://github.com/pbs-assess/csasdown). 
The content is nearing completion in January 2024.


## Dependencies
Most packages can be installed from CRAN, but some need special installation:

```{r}
# install.packages("remotes")
remotes::install_github("pbs-assess/gfdata")
remotes::install_github("pbs-assess/gfplot")
remotes::install_github("pbs-assess/csasdown")
```


## Use
All the `.Rmd` files used to create the tech report are found in the 
report/report-rmd/ folder.

The `make.R` script (found in the report/ folder) contains all the code to create 
the species specific pages. This script pulls data from the databases, creates 
the figures (and writes them to the report/report-rmd/figs/ folder), 
and generates the `05-plot-pages.Rmd` file. 

To render the report, open `index.Rmd` in RStudio and then click the knit button.


## Output
The `csasdown` package writes the tech report to the report/report-rmd/_book/ 
folder.


## TODO

- [ ] Add Sablefish predicted lengths from 2018+

- [ ] Address Malcolm's comments about at-sea/ dockside and sorted/ unsorted issue

- [ ] Address Rowan's comments on figure clarity

- [ ] Incorporate remaining revisions once received (Chris, Philina, Robyn, Schon)

- [ ] Tweak working around representativeness

- [ ] Final read-through


## Notes and Cautions
- Please note that "sample" should refer to the collection of specimens taken in a sampling event, while "specimen" should refer to an individual fish.


