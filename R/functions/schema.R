# Load and use the canonical schema defined in config/schema.yml.
#
# This is the single source of truth for the common dataset's structure.
# Both the validation stage (R/pipeline/validate_site.R) and the data
# dictionary (docs/data_dictionary.md) are generated from this file so they
# cannot drift apart.

load_schema <- function(path = "config/schema.yml") {
  yaml::read_yaml(path)
}

#' Required columns for a schema table (e.g. "community", "cover", "meta")
schema_required_columns <- function(schema, table) {
  cols <- schema[[table]]
  names(cols)[vapply(cols, function(x) isTRUE(x$required), logical(1))]
}

#' All columns (required + optional) for a schema table
schema_all_columns <- function(schema, table) {
  names(schema[[table]])
}

#' Render config/schema.yml into a human-readable data dictionary.
#'
#' @param schema_path Path to the schema yaml file.
#' @param out_path Path to write the markdown data dictionary to.
generate_data_dictionary <- function(schema_path = "config/schema.yml",
                                      out_path = "docs/data_dictionary.md") {
  schema <- load_schema(schema_path)

  out_dir <- dirname(out_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  render_table <- function(table_name, columns) {
    header <- c(
      paste0("## `", table_name, "` table"),
      "",
      "| Column | Type | Required | Description | Unit | Allowed values | Range |",
      "|---|---|---|---|---|---|---|"
    )
    rows <- vapply(names(columns), function(col_name) {
      col <- columns[[col_name]]
      allowed <- if (!is.null(col$allowed_values)) paste(col$allowed_values, collapse = ", ") else ""
      range <- if (!is.null(col$min) || !is.null(col$max)) {
        paste0(
          if (!is.null(col$min)) paste0(">= ", col$min) else "",
          if (!is.null(col$min) && !is.null(col$max)) ", " else "",
          if (!is.null(col$max)) paste0("<= ", col$max) else ""
        )
      } else {
        ""
      }
      paste0(
        "| `", col_name, "` | ", col$type %||% "", " | ",
        if (isTRUE(col$required)) "yes" else "no", " | ",
        col$description %||% "", " | ", col$unit %||% "", " | ",
        allowed, " | ", range, " |"
      )
    }, character(1))
    c(header, rows, "")
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  lines <- c(
    "# TransPlant Network - common dataset data dictionary",
    "",
    "This file is auto-generated from [config/schema.yml](../config/schema.yml).",
    "Do not edit by hand - edit the schema file and re-run `generate_data_dictionary()`.",
    "",
    unlist(lapply(names(schema), function(tbl) render_table(tbl, schema[[tbl]])))
  )

  writeLines(lines, out_path)
  invisible(out_path)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
