# Legacy recipe wrappers.
#
# Why this file exists: 16 of the 19 sites (plus the 4 NO_Norway gradients)
# have genuinely bespoke cleaning logic (unique treatment matrices, wide-format
# sheets, cover-class midpoint tables, multiple raw sources bound together,
# database joins, etc.) that was already written, reviewed and trusted in
# R/functions/ImportData/ImportCleanAndMakeList_*.R. Rewriting all of that "generically"
# without real raw data available to test against would risk silently changing
# the resulting dataset. Per the plan (Section 2), these sites keep their
# bespoke code as-is; this file just gives each one a uniform name so the
# registry-driven pipeline (R/functions/pipeline/clean_site.R, R/site_plan.R) can call
# them the same way it calls the general pipeline for pilot sites.
#
# Each function here simply calls the original ImportClean_<site>() function
# (sourced from R/functions/ImportData/ via tar_source()) and returns its unmodified
# result: list(meta=, community=, cover=, taxa=[, trait=]).
#
# To migrate one of these sites onto the general pipeline later: add its
# general-pipeline config to `site_pipeline_config` in R/functions/site_registry.R,
# set its `recipe_fn` to NA in `site_registry`, and remove the entry below.

clean_recipe_CH_Calanda   <- function() ImportClean_CH_Calanda()
clean_recipe_NO_Norway    <- function(g) ImportClean_NO_Norway(g = g)
clean_recipe_US_Arizona   <- function() ImportClean_US_Arizona()
clean_recipe_CN_Damxung   <- function() ImportClean_CN_Damxung()
clean_recipe_CN_Heibei    <- function() ImportClean_CN_Heibei()
clean_recipe_IN_Kashmir   <- function() ImportClean_IN_Kashmir()
clean_recipe_DE_TransAlps <- function() ImportClean_DE_TransAlps()
clean_recipe_DE_Susalps   <- function() ImportClean_DE_Susalps()
clean_recipe_FR_AlpeHuez  <- function() ImportClean_FR_AlpeHuez()
clean_recipe_FR_Lautaret  <- function() ImportClean_FR_Lautaret()
clean_recipe_IT_MatschMazia1 <- function() ImportClean_IT_MatschMazia1()
clean_recipe_IT_MatschMazia2 <- function() ImportClean_IT_MatschMazia2()
