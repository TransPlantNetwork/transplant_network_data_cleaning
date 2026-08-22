# TransPlant Network Project

This repository contains code for cleaning and loading data from a distributed network of plant community transplants along elevational gradients. 

This project has migrated from the old Drake pipeline to `targets` - see "Using targets" below for how to run it now. The old Drake-based workflow (`R/old_code/`) is kept for reference only until the new pipeline covers everything it did; see "Old code" at the bottom of this file.

## Authors

* **Chelsea Chisholm** - chelsea.chisholm@gmail.com
* **Dagmar Egelkraut** - Dagmar.Egelkraut@uib.no

## Using targets

The pipeline is defined in `_targets.R`, which combines plan files from `R/` (`download_plan.R`, `site_plan.R`, `harmonization_plan.R`, `validation_plan.R`, `taxonomy_plan.R`, `regression_plan.R`, `database_plan.R`). Everything the plans call is in `R/functions/` (see "R folder layout" below).

To run the pipeline:

```r
source("run.R")
# or
targets::tar_make()
```

Useful checks:

```r
targets::tar_manifest()    # list every target without running anything
targets::tar_validate()    # check the pipeline is structurally valid
targets::tar_visnetwork()  # dependency graph
targets::tar_outdated()    # which targets need to run
targets::tar_load(name)    # load a built target into the session
```

### R folder layout

- `R/*_plan.R` - the `targets` plan files only (`download_plan.R`, `site_plan.R`,
  `harmonization_plan.R`, `validation_plan.R`, `taxonomy_plan.R`, `regression_plan.R`,
  `database_plan.R`).
- `R/functions/` - every function the plans call: the general pipeline steps
  (`R/functions/pipeline/`), the site registry (`R/functions/site_registry.R`),
  schema/data-dictionary helpers (`R/functions/schema.R`), merging
  (`R/functions/merge_community.R`), the regression check
  (`R/functions/regression_check.R`), the legacy per-site recipes
  (`R/functions/sites/legacy_recipes.R`), and the original per-site import/clean
  scripts they wrap (`R/functions/ImportData/`).
- `R/old_code/` - the previous Drake-based pipeline (`TransPlant_DrakePlan.R`,
  `runsource_drakeplan.R`, `runsource_traitplan.R`) and folders it depended on
  that the new pipeline doesn't use or need (`CheckData/` manual QA plots,
  `ClimateData/` climate raster processing, `WrangleTaxaTraits/` old
  taxize-based taxonomy/trait code). Kept for reference only, not sourced by
  `_targets.R` or the tests, until we're confident the new pipeline covers
  everything the old one did.

### Adding or updating a site

Each site is one row in `site_registry` (`R/functions/site_registry.R`) - this is
the single place that documents a site's raw data format, cover unit, treatment
rule, and ID components, and it drives `tar_map()` in `R/site_plan.R` to create
that site's targets automatically (`cleaned_<site_id>`, `validated_<site_id>`).

- Fully generalized sites (currently the 3 pilot sites: `CH_Lavey`, `US_Colorado`,
  `CN_Gongga`) are cleaned entirely by the shared functions in
  `R/functions/pipeline/` (`import_raw`, `standardize_columns`, `derive_treatment`,
  `build_ids`, `split_cover_classes`, `compute_rel_cover`). Adding a new site like
  this means adding a row to `site_registry` plus an entry in `site_pipeline_config`.
- The remaining sites keep their original, trusted cleaning code from
  `R/functions/ImportData/`, wrapped by a thin `clean_recipe_*()` function in
  `R/functions/sites/legacy_recipes.R` so they plug into the same registry-driven
  pipeline. These can be migrated onto the general functions later, one at a time.

### Validation, taxonomy, regression and the output database

- **Validation** (`R/functions/pipeline/validate_site.R`, `R/validation_plan.R`): a
  small set of schema/value/referential checks, generated from `config/schema.yml`,
  run per site and combined into `validation_summary`.
- **Schema & data dictionary**: `config/schema.yml` is the single source of truth
  for the common dataset's columns; `R/functions/schema.R::generate_data_dictionary()`
  renders it to `docs/data_dictionary.md`.
- **Taxonomy** (`R/taxonomy_plan.R`): resolves species names via the
  [TNRS package](https://github.com/EnquistLab/RTNRS) after merging, separate from
  per-site cleaning.
- **Regression safety net** (`R/regression_plan.R`, `data-raw/snapshot_legacy_output.R`):
  compares the new pipeline's merged output against a saved snapshot of the old
  Drake pipeline's output, once that snapshot has been generated.
- **Database** (`R/database_plan.R`): writes the final merged, harmonized dataset
  to `data/transplant_network_clean.sqlite`.

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

