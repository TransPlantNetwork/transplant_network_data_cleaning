# TransPlant Network Project

This repository contains code for cleaning and loading data from a distributed network of plant community transplants along elevational gradients. 

To obtain the combined dataframe across all sites for plant community data, run the runsource_drakeplan.R script. This will run the TransPlant_DrakePlan.R in the R subfolder, which in turn runs all the scripts in the R/ImportData subfolder and merges using the merge_community.R script. The resulting dataframe 'dat' will contain plant community abundance for each site, elevation, time (year), and plot.

To find the traits dataset, run the runsource_traitplan.R. This will source the trait drakeplan in the R/WrangleTaxaTraits subfolder. In this drake plan, sitetraits and trytraits dataframes are loaded, where site traits are field-collected data from the sites with average traits per species per elevation per site (if collected), and the try traits are an average value per species that has been gapfilled from TRY. The final combined trait object alltraits contains the field-collected sites per elevation per species and then gap-fills any values where we did not have field-collected data using the try traits.

## Authors

* **Chelsea Chisholm** - chelsea.chisholm@gmail.com
* **Dagmar Egelkraut** - Dagmar.Egelkraut@uib.no

## Using targets

The pipeline is defined in `_targets.R`, which combines plan files from `R/` (e.g. `download_plan.R`, `harmonization_plan.R`, `validation_plan.R`). Custom functions live in `R/functions/` and are loaded with `tar_source()`.

To run the pipeline:

```r
source("run.R")
# or
targets::tar_make()
```

Useful checks:

```r
targets::tar_visnetwork()   # dependency graph
targets::tar_outdated()     # which targets need to run
targets::tar_load(name)     # load a built target into the session
```

Add new steps as `tar_target()` (or `tar_plan()`) entries in the relevant plan file under `R/`.

## Using renv

This project uses [renv](https://rstudio.github.io/renv/) so everyone works with the same package versions. After cloning the repo, open the project and restore the library from `renv.lock`:

```r
renv::restore()
```

When you add or update packages:

1. Add the package to `DESCRIPTION` (`Imports:`; use `Remotes:` for GitHub packages).
2. Run `renv::install()` then `renv::snapshot()`.
3. Commit the updated `DESCRIPTION` and `renv.lock`.

Do not commit `renv/library/` — it is local and ignored by git.

