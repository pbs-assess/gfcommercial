# gfcommercial
Groundfish Commercial Biosampling Compendium

## Overview
This commercial biosampling compendium uses an R [targets](https://github.com/ropensci/targets)
workflow to generate figure and text summaries of groundfish commercial biosampling
in British Columbia, Canada. Figure and text summaries are then formatted as a CSAS Technical Report using
the R package [csasdown](https://github.com/pbs-assess/csasdown). The content
is in the early stages of development as of January, 2023.

## Dependencies
Most packages can be installed from CRAN, but some need special installation:

```{r}
remotes::install_github("pbs-assess/csasdown")

```

## Use
The R targets workflow is found in the _targets.R file. To run the workflow, first
load the targets package via

```{r}
library(targets)
```

then optionally inspect the node statuses and dependencies via

```{r}
tar_visnetwork()
```

and finally, update the outdated or errored nodes in the workflow via

```{r}
tar_make()
```

## Output
The targets workflow exports figures to the XX folder. All other nodes are
stored as R objects and can be accessed by calling the `tar_read()` function.
For example, 

```{r}
tar_read(data_cache_path)
```

## TODO

- [x] Develop figure functions

Notes: four functions have been developed that plot summaries of specimen counts, 
sample representativeness, length frequencies and age frequencies respectively.

- [ ] Develop figure layout functions

Notes: two functions are needed to take a species name and data path and save
a figure layout pdf for pages one and two respectively of a two-page species 
summary. The current plan is for the page one pdf to show the sample counts figure
and for the page two pdf to show the representativeness, length frequencies and
age frequencies figures. This work is in progress (Leah).

- [ ] Automate the generation of targets nodes for each figure layout pdf for each
species.

Notes: As currently envisioned, tarchetypes::tar_map() or tarchetypes::tar_eval()
should allow automatic generation of targets nodes by passing a species name list
and data file path to the figure layout functions. Using one call for the page one
nodes and a separate call for the page two nodes should result in the desired
one automatically generated node per species layout page.
 
- [ ] Automate the generation of .Rmd pages to typeset species summaries and 
figure layout pdfs. 

Notes: I (Luke) have not yet explored options for automating the 
generation of .Rmd pages.

- [ ] Develop species information summaries including recent assessment years,
recent assessment citations, and catch proportions by gear type.

Notes: These may be able to draw from gfsynopsis species summaries, augmented
with information from the catch data and individual assessments.

- [ ] Develop introductory material, supporting figures including a map of 
PMFC areas, figure descriptions, and discussion.

Notes: gfsynopsis may provide a useful model for framing the report and 
contextualizing the species-specific commercial sampling summaries.

## Notes and Cautions
- Please note that "sample" should refer to the collection of specimens taken in a sampling event, while "specimen" should refer to an individual fish.


