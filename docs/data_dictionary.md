# TransPlant Network - common dataset data dictionary

This file is auto-generated from [config/schema.yml](../config/schema.yml).
Do not edit by hand - edit the schema file and re-run `generate_data_dictionary()`.

## `community` table

| Column | Type | Required | Description | Unit | Allowed values | Range |
|---|---|---|---|---|---|---|
| `Year` | integer | yes | Calendar year of the observation |  |  |  |
| `originSiteID` | character | yes | Identifier of the site the turf/plot originated from |  |  |  |
| `originBlockID` | character | no | Block identifier at the origin site (NA if not recorded) |  |  |  |
| `destSiteID` | character | yes | Identifier of the destination (transplant) site |  |  |  |
| `destBlockID` | character | no | Block identifier at the destination site (NA if not recorded) |  |  |  |
| `destPlotID` | character | yes | Unique plot/turf identifier at the destination site |  |  |  |
| `Treatment` | character | yes | Experimental treatment applied to the plot |  | LocalControl, Warm, Cold, Control, NettedControl |  |
| `UniqueID` | character | yes | Unique identifier for a plot x year combination; must be unique within a site |  |  |  |
| `SpeciesName` | character | yes | Vascular plant species name (pre taxonomy harmonization) |  |  |  |
| `Cover` | numeric | yes | Percent cover (most sites) or biomass in grams (biomass-based sites, e.g. DE_Susalps) | percent or g |  | >= 0 |
| `Total_Cover` | numeric | no | Sum of Cover across all species in the same plot x year |  |  | >= 0 |
| `Rel_Cover` | numeric | yes | Cover divided by Total_Cover; expected to be between 0 and 1 |  |  | >= 0, <= 1 |

## `cover` table

| Column | Type | Required | Description | Unit | Allowed values | Range |
|---|---|---|---|---|---|---|
| `UniqueID` | character | yes | Matches community$UniqueID |  |  |  |
| `CoverClass` | character | yes | Non-vascular cover category (e.g. Bare ground, Moss, Litter, Rock, Lichen) |  |  |  |
| `OtherCover` | numeric | yes | Cover value for the non-vascular class |  |  | >= 0 |
| `Rel_OtherCover` | numeric | no | Relative cover for the non-vascular class |  |  | >= 0 |

## `meta` table

| Column | Type | Required | Description | Unit | Allowed values | Range |
|---|---|---|---|---|---|---|
| `Gradient` | character | yes | Name of the elevational gradient/experiment (usually same as site_id) |  |  |  |
| `destSiteID` | character | yes | Identifier of the destination site; must match community$destSiteID |  |  |  |
| `Longitude` | numeric | yes | Decimal degrees longitude of the destination site |  |  | >= -180, <= 180 |
| `Latitude` | numeric | yes | Decimal degrees latitude of the destination site |  |  | >= -90, <= 90 |
| `Elevation` | numeric | yes | Elevation of the destination site in meters above sea level | m |  | >= 0 |
| `YearEstablished` | integer | yes | Year the experiment was established at this site |  |  |  |
| `YearMin` | integer | yes | First year of observations in the cleaned community data |  |  |  |
| `YearMax` | integer | yes | Last year of observations in the cleaned community data |  |  |  |
| `YearRange` | integer | no | YearMax minus YearEstablished |  |  |  |
| `PlotSize_m2` | numeric | yes | Plot/turf size in square meters | m2 |  | >= 0 |
| `Country` | character | yes | Country of the destination site |  |  |  |

