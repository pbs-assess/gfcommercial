# Script to make RMD of plot pages

library(dplyr)
lapply(list.files("R", full.names = TRUE), source)

build_dir <- "report/report-rmd"
ext <- ".png"

data_cache_path <- here::here("data-cache")
dir.create(data_cache_path, showWarnings = FALSE)

file <- here::here("data", "species.csv")
spp <- get_spp_names(file)
spp_list <- as.list(spp$spp_w_hyphens)

# pbs_spp <- gfdata::get_species()
# spp$species_common_name[!spp$species_common_name %in% pbs_spp$species_common_name]
spp$species_common_name[spp$species_common_name == "rougheye blackspotted rockfish complex"] <- "rougheye/blackspotted rockfish complex"

filenames <- file.path(data_cache_path, paste0(spp$spp_w_hyphens, ".rds"))

for (i in seq_along(spp$species_common_name)) {
  .s <- spp$species_common_name[i]
  cat(.s, "\n")
  d <- list()
  if (!file.exists(filenames[i])) {
    d[["commercial_samples"]] <-
      gfdata::get_commercial_samples(
        species = .s,
        unsorted_only = FALSE,
        return_all_lengths = TRUE
      )
    d[["catch"]] <- gfdata::get_catch(.s)
    saveRDS(d, file = filenames[i])
  }
}


missing_spp <- NULL

# Check figs folder if any spp missing
for (i in 1:length(spp_list)) {
  fig_check <- paste0(here::here("report", "figs"), "/", spp_list[i])
  fig_check1 <- paste0(fig_check, "-pg-1", ext)
  fig_check2 <- paste0(fig_check, "-pg-2", ext)

  missing <- !file.exists(fig_check1) | !file.exists(fig_check2)

  if (!missing){
    print(paste0("Figure pages for ", spp_list[i], " already exists"))
  } else {
    missing_spp <- c(missing_spp, spp_list[i])
  }
}

# Make pages for missing species
for (i in 1:length(missing_spp)) {
  plot_layout_pg_1(missing_spp[i])
  plot_layout_pg_2(missing_spp[i])
}

# Generate plot-pages.RMD
temp <- lapply(spp$species_common_name, function(x) {
  spp_title <- stringr::str_to_title(x)
  spp_hyphen <- spp$spp_w_hyphens[spp$species_common_name == x]
  out <- list()

  i <- 1
  out[[i]] <- "\\clearpage\n"
  i <- i + 1
  out[[i]] <- paste0("## ", spp_title, " {#sec:", spp_hyphen, "}\n")
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.4in]{report/figs/",
                     spp_hyphen, "-pg-1", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\includegraphics[width=6.4in]{report/figs/",
                     spp_hyphen, "-pg-2", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}\n"
  i <- i + 1
  out[[i]] <- "\n"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
temp <- c("# Plots\n", temp)
temp <- c("\\clearpage\n", temp)
temp <- c("<!-- This page has been automatically generated: do not edit by hand -->\n", temp)
con <- file(file.path(build_dir, "plot-pages.Rmd"), encoding = "UTF-8")
writeLines(temp, con = con)




