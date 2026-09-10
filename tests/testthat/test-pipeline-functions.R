test_that("build_ids() assembles UniqueID from id_components", {
  site_cfg <- list(pipeline = list(id_components = c("Year", "originSiteID", "destSiteID", "destPlotID")))
  dat <- tibble::tibble(Year = 2020, originSiteID = "A", destSiteID = "B", destPlotID = "1")
  out <- build_ids(dat, site_cfg)
  expect_equal(out$UniqueID, "2020_A_B_1")
  expect_true(is.character(out$destBlockID))
})

test_that("compute_rel_cover() sums to 1 within a plot x year", {
  site_cfg <- list()
  dat <- tibble::tibble(
    UniqueID = c("p1", "p1", "p2"),
    SpeciesName = c("sp1", "sp2", "sp1"),
    Cover = c(30, 70, 50)
  )
  out <- compute_rel_cover(dat, site_cfg)
  totals <- out %>% dplyr::group_by(UniqueID) %>% dplyr::summarise(total = sum(Rel_Cover))
  expect_equal(totals$total, c(1, 1))
})

test_that("add_other_category() synthesizes an Other row summing percent cover to 100", {
  site_cfg <- list(cover_unit = "percent", pipeline = list(non_vascular = c("Moss")))
  dat <- tibble::tibble(
    UniqueID = c("p1", "p1"),
    SpeciesName = c("sp1", "sp2"),
    Cover = c(30, 20)
  )
  out <- add_other_category(dat, site_cfg)
  expect_true("Other" %in% out$SpeciesName)
  expect_equal(out$Cover[out$SpeciesName == "Other"], 50) # 100 - 30 - 20
})

test_that("add_other_category() is skipped when site_cfg$pipeline$add_other is FALSE", {
  site_cfg <- list(cover_unit = "percent", pipeline = list(add_other = FALSE))
  dat <- tibble::tibble(UniqueID = "p1", SpeciesName = "sp1", Cover = 30)
  out <- add_other_category(dat, site_cfg)
  expect_false("Other" %in% out$SpeciesName)
})

test_that("split_cover_classes() separates vascular species from cover classes after Rel_Cover is computed", {
  site_cfg <- list(cover_unit = "percent", pipeline = list(non_vascular = c("Moss")))
  dat <- tibble::tibble(
    UniqueID = c("p1", "p1", "p1", "p1"),
    SpeciesName = c("sp1", "sp2", "Moss", "Other"),
    Cover = c(30, 20, 10, 40)
  ) %>% compute_rel_cover(site_cfg)
  out <- split_cover_classes(dat, site_cfg)
  expect_true(all(c("sp1", "sp2") %in% out$comm$SpeciesName))
  expect_true(all(c("Moss", "Other") %in% out$cover$CoverClass))
  expect_equal(sum(out$comm$Rel_Cover) + sum(out$cover$Rel_OtherCover), 1)
})

test_that("derive_treatment() dispatches on treatment_rule", {
  site_cfg <- list(
    treatment_rule = "code_lookup",
    pipeline = list(treatment_map = c("C" = "Control", "W" = "Warm"))
  )
  site_data <- tibble::tibble(treatment_code = c("C", "W"))
  out <- derive_treatment(site_data, site_cfg)
  expect_equal(out$Treatment, c("Control", "Warm"))
})

test_that("derive_treatment() joins origin x dest treatment_matrix", {
  site_cfg <- list(
    site_id = "TEST",
    treatment_rule = "origin_dest_matrix",
    pipeline = list(
      treatment_matrix = tibble::tribble(
        ~originSiteID, ~destSiteID, ~Treatment,
        "High", "Low", "Warm",
        "High", "High", "LocalControl",
        "Low", "High", "Cold"
      )
    )
  )
  site_data <- tibble::tibble(
    originSiteID = c("High", "High", "Low", "Low"),
    destSiteID = c("Low", "High", "High", "Low"),
    Cover = 1:4
  )
  out <- derive_treatment(site_data, site_cfg)
  expect_equal(out$Treatment, c("Warm", "LocalControl", "Cold", NA_character_))
  expect_equal(out$Cover, 1:4)
})

test_that("derive_treatment() recodes HIGH/LOW turf codes with destSiteID", {
  site_cfg <- list(site_id = "TEST", treatment_rule = "turf_code_site")
  site_data <- tibble::tibble(
    treatment_code = c("low_turf", "high_turf", "HIGH_TURF", "LOW_TURF"),
    destSiteID = c("LOW", "LOW", "HIGH", "HIGH"),
    Cover = 1:4
  )
  out <- derive_treatment(site_data, site_cfg)
  expect_equal(out$originSiteID, c("LOW", "HIGH", "HIGH", "LOW"))
  expect_equal(out$Treatment, c("LocalControl", "Warm", "LocalControl", NA_character_))
  expect_null(out$treatment_code)
})

test_that("validate_site() flags Rel_Cover that does not sum to ~1", {
  skip_if_not(file.exists(file.path("..", "..", "config", "schema.yml")), "schema.yml not found relative to test dir")
  withr_wd <- setwd(file.path("..", ".."))
  on.exit(setwd(withr_wd), add = TRUE)
  site_cfg <- list(site_id = "TEST")
  cleaned <- list(
    community = tibble::tibble(
      UniqueID = c("p1", "p1"), SpeciesName = c("sp1", "sp2"), Cover = c(10, 10),
      Rel_Cover = c(0.1, 0.1), Treatment = c("Control", "Control"),
      Year = 2020, originSiteID = "A", destSiteID = "B", destPlotID = "1"
    ),
    meta = tibble::tibble(destSiteID = "B", Gradient = "TEST", Longitude = 1, Latitude = 1, Elevation = 100,
                          YearEstablished = 2019, YearMin = 2020, YearMax = 2020, YearRange = 1,
                          PlotSize_m2 = 1, Country = "X")
  )
  report <- validate_site(cleaned, site_cfg)
  rel_cover_check <- report[report$check == "rel_cover_sums", ]
  expect_equal(rel_cover_check$status, "fail")
})
