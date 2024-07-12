# Script to make RMD of plot pages

# Load packages and functions --------------------------------------------------
library(dplyr)
# Suppress summarize info
options(dplyr.summarise.inform = FALSE)
lapply(list.files("R", full.names = TRUE), source)

# Set directories and paths
build_dir <- "report/report-rmd"
ext <- ".png"
data_cache_path <- here::here("data-cache")
dir.create(data_cache_path, showWarnings = FALSE)

# Load species list ------------------------------------------------------------
file <- here::here("data", "species.csv")
spp <- get_spp_names(file)
spp_list <- as.list(spp$spp_w_hyphens)

spp$species_common_name[spp$species_common_name == "rougheye blackspotted rockfish complex"] <- "rougheye/blackspotted rockfish complex"

filenames <- file.path(data_cache_path, paste0(spp$spp_w_hyphens, ".rds"))

# Update data cache ------------------------------------------------------------
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

# Calculate proportion of data from at-sea or dockside -------------------------

# ls_data_source <- lapply(spp$spp_w_hyphens, data_source)
#
# column_names <- c("species", "dockside", "atsea")
# sources <- data.frame(matrix(nrow = 0, ncol = length(column_names)))
# colnames(sources) <- column_names
#
# for (i in seq_along(spp$spp_w_hyphens)) {
#   sources[i,] <- data.frame(
#     species = ls_data_source[[i]]$species,
#     dockside = ls_data_source[[i]]$dockside,
#     atsea = ls_data_source[[i]]$atsea
#   )
# }
#
# write.csv(sources, paste0(here::here("data"), "/", "data_sources", ".csv"), row.names = FALSE)


# Gather and arrange some metadata ---------------------------------------------

if (!file.exists(here::here("report", "itis.rds"))) {
  cls <- taxize::classification(spp$itis_tsn[!is.na(spp$itis_tsn)], db = 'itis')
  saveRDS(cls, file = here::here("report", "itis.rds"))
} else {
  cls <- readRDS(here::here("report", "itis.rds"))
}
cls <- plyr::ldply(cls) %>%
  rename(itis_tsn = .id) %>%
  filter(rank %in% c('order', 'family')) %>%
  reshape2::dcast(itis_tsn ~ rank, value.var = 'name')
spp <- left_join(spp, mutate(cls, itis_tsn = as.integer(itis_tsn)), by = "itis_tsn")

# downloaded from:
# https://species-registry.canada.ca/index-en.html#/species?ranges=1,18&taxonomyId=4&sortBy=commonNameSort&sortDirection=asc&pageSize=10
# on 2023-04-18
cos <- readr::read_csv(here::here("report/COSEWIC-species.csv"), show_col_types = FALSE)
cos <- rename(cos, species_common_name = `COSEWIC common name`,
              species_science_name = `Scientific name`,
              cosewic_status = `COSEWIC status`,
              sara_status = `Schedule status`)
# duplicate of inside YE:
cos <- dplyr::filter(cos, !grepl("Pacific Ocean outside waters population", `COSEWIC population`))
cos <- dplyr::select(cos, species_common_name, species_science_name, cosewic_status, sara_status)
cos <- dplyr::mutate(cos, species_science_name = ifelse(grepl("type I", species_science_name), "Sebastes aleutianus/melanostictus", species_science_name))
cos$species_common_name <- tolower(cos$species_common_name)
cos$species_science_name <- tolower(cos$species_science_name)
spp <- dplyr::left_join(spp, cos, by = c("species_common_name", "species_science_name"))

# Parse metadata that will be used at the top of each species page:
refs <- readr::read_csv(here::here("report/spp-refs.csv"), show_col_types = FALSE)
spp <- dplyr::left_join(spp, refs, by = "species_common_name")

spp$species_science_name <- gfplot:::firstup(spp$species_science_name)
spp$species_science_name <- gsub(" complex", "", spp$species_science_name)
spp$resdoc <- gsub(", ", ", @", spp$resdoc)
spp$resdoc <- ifelse(is.na(spp$resdoc), "", paste0("@", spp$resdoc, ""))
spp$sar <- gsub(", ", ", @", spp$sar)
spp$sar <- ifelse(is.na(spp$sar), "", paste0("@", spp$sar, ""))
spp$other_ref_cite <- ifelse(is.na(spp$other_ref), "", paste0(spp$type_other_ref, ": @", spp$other_ref, ""))
spp$other_ref_cite <- gsub(", ", ", @", spp$other_ref_cite)
spp$species_common_name[spp$species_common_name == "rougheye blackspotted rockfish complex"] <- "rougheye/blackspotted rockfish complex"
#filenames <- file.path(data_cache_path, paste0(spp$spp_w_hyphens, ".rds"))

summaries <- readr:: read_csv(here::here("report/spp_summaries.csv"), show_col_types = FALSE)

spp <- dplyr::left_join(spp, summaries, by = "species_common_name")

spp <- spp %>%
  dplyr::arrange(species_code)

# Check for and make missing species plot pages --------------------------------
missing_spp <- NULL

# Check figs folder if any species missing
for (i in 1:length(spp_list)) {
  fig_check <- paste0(here::here("report", "report-rmd", "figs"), "/", spp_list[i])
  fig_check1 <- paste0(fig_check, "-pg-1", ext)
  fig_check2 <- paste0(fig_check, "-pg-2", ext)
  fig_check3 <- paste0(fig_check, "-pg-3", ext)
  fig_check4 <- paste0(fig_check, "-pg-4", ext)

  missing <- !file.exists(fig_check1) | !file.exists(fig_check2) | !file.exists(fig_check3) | !file.exists(fig_check4)

  if (!missing){
    print(paste0("Figure pages for ", spp_list[i], " already exists"))
  } else {
    missing_spp <- c(missing_spp, spp_list[i])
  }
}

# Make pages for missing species
suppressWarnings(
for (i in 1:length(missing_spp)) {
  plot_layout_pg_1(missing_spp[i])
  plot_layout_pg_2(missing_spp[i])
  plot_layout_pg_3(missing_spp[i])
  plot_layout_pg_4(missing_spp[i])
}
)
# Generate plot-pages.RMD ------------------------------------------------------
temp <- lapply(spp$species_common_name, function(x) {

  french <- FALSE
  out <- list()

  spp_title <- stringr::str_to_title(x)
  spp_hyphen <- spp$spp_w_hyphens[spp$species_common_name == x]
  latin_name <- spp$species_science_name[spp$species_common_name == x]
  sar <- spp$sar[spp$species_common_name == x]
  resdoc <- spp$resdoc[spp$species_common_name == x]
  species_code <- spp$species_code[spp$species_common_name == x]
  other_ref <- spp$other_ref_cite[spp$species_common_name == x]
  sara_status <- spp$sara_status[spp$species_common_name == x]
  cosewic_status <- spp$cosewic_status[spp$species_common_name == x]
  cosewic_report <- spp$cosewic_status_reports[spp$species_common_name == x]
  worms_id <- spp$worms_id[spp$species_common_name == x]
  summary_pgraph <- spp$summary[spp$species_common_name == x]

  resdoc_text <- if (grepl(",", resdoc)) {
    paste0("Last Research Document", "s: ")
  } else {
    paste0("Last Research Document", ": ")
  }
  sar_text <- if (grepl(",", sar)) {
    paste0("Last Science Advisory Report", "s: ")
  } else {
    paste0("Last Science Advisory Report", ": ")
  }

  i <- 1
  out[[i]] <- "\\clearpage\n"
  i <- i + 1
  out[[i]] <- paste0("## ", spp_title, " {#sec:", spp_hyphen, "}\n")
  i <- i + 1
  out[[i]] <- paste0(emph(latin_name), " (", species_code, ")", "\\\n",
                     "Order", ": ", spp$order[spp$species_common_name == x], ", ",
                     "Family", ": ", spp$family[spp$species_common_name == x],
                     ", ")
  i <- i + 1
  if (species_code == "394") { # Sebastes aleutianus/melanostictus
    .names <- rougheye_split(gfplot:::firstup(latin_name))
    out[[i]] <- paste0(
      "[FishBase 1]",
      "(http://www.fishbase.org/summary/", .names[1], "),"
    )
    i <- i + 1
    out[[i]] <- paste0(
      "[FishBase 2]",
      "(http://www.fishbase.org/summary/", .names[2], ")"
    )
  } else {
    out[[i]] <- paste0("[FishBase]",
                       "(http://www.fishbase.org/summary/",
                       gsub(" ", "-", gfplot:::firstup(latin_name)), ")")
  }
  if (worms_id != "unknown") {
    out[[i]] <- paste0(out[[i]], ", ")
    i <- i + 1
    out[[i]] <- paste0(
      "[WoRMS]",
      "(http://www.marinespecies.org/aphia.php?p=taxdetails&id=",
      worms_id, ")")
  }
  # else if (worms_id == "unknown") {
  #   out[[i]] <- paste0(out[[i]], "\\")
  # }
  if (resdoc != "") {
    i <- i + 1
    out[[i]] <- paste0("\\")
    i <- i + 1
    out[[i]] <- paste0(resdoc_text, resdoc)
  }
  if (sar != "") {
    i <- i + 1
    out[[i]] <- paste0("\\")
    i <- i + 1
    out[[i]] <- paste0(sar_text, sar)
  }
  i <- i + 1

  if (!is.na(other_ref)) {
    if (other_ref != "") {
      out[[i]] <- paste0("\\")
      i <- i + 1
      out[[i]] <- paste0(other_ref)
      if (!is.na(cosewic_status) && cosewic_status != "") {
      }
      i <- i + 1
    }
  }
  if (!is.na(cosewic_report)) {
    if (cosewic_report != "") {
      out[[i]] <- paste0("\\")
      i <- i + 1
      out[[i]] <- paste0("COSEWIC Status Report", ": @", cosewic_report)
      i <- i + 1
    }
  }
  if (!is.na(cosewic_status)) {
    if (cosewic_status != "") {
      out[[i]] <- paste0("\\")
      i <- i + 1
      out[[i]] <- paste0("COSEWIC Status", ": ", cosewic_status)
      if (!is.na(sara_status)) {
        if (sara_status != "") {
          if (sara_status != "No Status") {
            out[[i]] <- paste0(out[[i]], ", ", "SARA Status", ": ", sara_status)
          }
        }
      }
      i <- i + 1
    }
  }
  if (species_code == "394") {
    out[[i]] <- paste0("\\")
    i <- i + 1
    out[[i]] <- paste0("COSEWIC Status", ": ", "Special Concern", ", ", "SARA Status", ": ", "Special Concern")
    i <- i + 1
  }
  out[[i]] <- "\n"
  i <- i + 1
  if (species_code == "225") {
      out[[i]] <- "Note that Pacific Hake undergoes a directed joint
      Canada-US coastwide survey and annual assessment. The most recent stock
      assessment should be consulted for details on stock status."
      # out[[i]] <- paste0(out[[i]], "\n")
      i <- i + 1
  }
  if (species_code == "614") {
      out[[i]] <- "Note that Pacific Halibut undergoes thorough assessment by the
      International Pacific Halibut Commission based on the annual
      standardized setline survey. The most recent stock assessment
      should be consulted for details on stock status."
    # out[[i]] <- paste0(out[[i]], "\n")
    i <- i + 1
  }
  if (species_code == "455") {
      out[[i]] <- "Note that Sablefish undergoes directed annual trap surveys,
      which are used for stock assessment. The most recent stock assessment should be
      consulted for details on stock status."
    # out[[i]] <- paste0(out[[i]], "\n")
    i <- i + 1
  }
  out[[i]] <- paste0("\n", summary_pgraph)
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\centering\\includegraphics[width=6.8in]{figs/",
                     spp_hyphen, "-pg-1", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\centering\\includegraphics[width=6.8in]{figs/",
                     spp_hyphen, "-pg-2", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\centering\\includegraphics[width=6.8in]{figs/",
                     spp_hyphen, "-pg-3", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}"
  i <- i + 1
  out[[i]] <- "\\clearpage"
  i <- i + 1
  out[[i]] <- "\\begin{figure}[b!]"
  i <- i + 1
  out[[i]] <- paste0("\\centering\\includegraphics[width=6.1in]{figs/",
                     spp_hyphen, "-pg-4", ext, "}")
  i <- i + 1
  out[[i]] <- "\\end{figure}\n"
  i <- i + 1
  out[[i]] <- "\n"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
temp <- c("<!-- This page has been automatically generated: do not edit by hand -->\n", temp)
con <- file(file.path(build_dir, "05-plot-pages.Rmd"), encoding = "UTF-8")
writeLines(temp, con = con)




