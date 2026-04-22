#!/usr/bin/env Rscript

# Lightweight NONMEM / PsN post-processing helpers.
# Assumptions for the MVP:
# - the project root is the current working directory when scripts run
# - scalar THETA / OMEGA / SIGMA entries are usually written one per line
# - PsN raw_results_*.csv is the preferred metadata source when it exists
# - NONMEM table file names are read from the model's $TABLE FILE= statements

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

normalize_slashes <- function(path) {
  gsub("\\\\", "/", path)
}

safe_read_lines <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(character())
  }
  readLines(path, warn = FALSE)
}

escape_regex <- function(text) {
  gsub("([][{}()+*^$.|\\\\?])", "\\\\\\1", text)
}

escape_html <- function(text) {
  if (length(text) == 0 || is.null(text)) {
    return("")
  }
  text <- as.character(text)
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text
}

format_value <- function(x, digits = 4) {
  if (length(x) > 1) {
    return(vapply(x, format_value, character(1), digits = digits))
  }
  if (length(x) == 0 || is.null(x) || is.na(x)) {
    return("NA")
  }
  if (is.logical(x)) {
    return(ifelse(x, "TRUE", "FALSE"))
  }
  if (is.numeric(x)) {
    return(formatC(x, digits = digits, format = "fg", flag = "#"))
  }
  as.character(x)
}

relative_to_root <- function(path, root) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  norm_root <- normalize_slashes(normalizePath(root, winslash = "/", mustWork = FALSE))
  norm_path <- normalize_slashes(normalizePath(path, winslash = "/", mustWork = FALSE))
  sub(paste0("^", escape_regex(norm_root), "/?"), "", norm_path)
}

extract_run_number <- function(run_id) {
  hit <- regmatches(run_id, regexpr("[0-9]+$", run_id))
  if (length(hit) == 0 || identical(hit, character(0)) || !nzchar(hit)) {
    return(Inf)
  }
  suppressWarnings(as.numeric(hit))
}

order_run_ids <- function(run_ids) {
  run_ids <- unique(run_ids)
  ord <- order(vapply(run_ids, extract_run_number, numeric(1)), run_ids)
  run_ids[ord]
}

first_existing <- function(paths) {
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  hits <- paths[file.exists(paths)]
  if (length(hits) == 0) {
    return(NA_character_)
  }
  hits[[1]]
}

discover_run_dir <- function(run_id, project_root) {
  exact_dir <- file.path(project_root, run_id)
  dir_variants <- list.files(
    project_root,
    pattern = paste0("^", escape_regex(run_id), "\\.dir[0-9]+$"),
    full.names = TRUE
  )
  candidates <- c(exact_dir, dir_variants)
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  candidates[[1]]
}

discover_raw_results_file <- function(run_id, project_root, run_dir = NA_character_) {
  expected_name <- paste0("raw_results_", run_id, ".csv")
  candidates <- c(
    file.path(run_dir, expected_name),
    file.path(project_root, expected_name)
  )
  if (!is.na(run_dir) && dir.exists(run_dir)) {
    recursive_hits <- list.files(
      run_dir,
      pattern = paste0("^", escape_regex(expected_name), "$"),
      recursive = TRUE,
      full.names = TRUE
    )
    candidates <- c(candidates, recursive_hits)
  }
  if (dir.exists(project_root)) {
    recursive_hits <- list.files(
      project_root,
      pattern = paste0("^", escape_regex(expected_name), "$"),
      recursive = TRUE,
      full.names = TRUE
    )
    filtered <- recursive_hits[vapply(recursive_hits, function(x) {
      parent <- basename(dirname(x))
      parent == run_id || grepl(paste0("^", escape_regex(run_id), "\\.dir[0-9]+$"), parent)
    }, logical(1))]
    candidates <- c(candidates, filtered)
  }
  first_existing(candidates)
}

find_psn_error_file <- function(run_dir) {
  if (is.na(run_dir) || !dir.exists(run_dir)) {
    return(NA_character_)
  }
  hits <- list.files(
    run_dir,
    pattern = "^psn_nonmem_error_messages\\.txt$",
    recursive = TRUE,
    full.names = TRUE
  )
  first_existing(hits)
}

resolve_run_context <- function(target = ".", project_root = getwd()) {
  project_root <- normalize_slashes(normalizePath(project_root, winslash = "/", mustWork = FALSE))
  target <- if (length(target) == 0 || is.null(target) || !nzchar(target[[1]])) "." else target[[1]]
  target_path <- target
  if (!grepl("^/", target_path)) {
    target_path <- file.path(project_root, target_path)
  }
  target_path <- normalize_slashes(normalizePath(target_path, winslash = "/", mustWork = FALSE))

  if (file.exists(target_path)) {
    if (dir.exists(target_path)) {
      run_id <- basename(target_path)
      run_dir <- target_path
    } else {
      run_id <- sub("\\.[^.]+$", "", basename(target_path))
      run_dir <- discover_run_dir(run_id, project_root)
    }
  } else {
    run_id <- sub("\\.[^.]+$", "", basename(target))
    run_dir <- discover_run_dir(run_id, project_root)
    target_path <- normalize_slashes(file.path(project_root, target))
  }

  list(
    project_root = project_root,
    target = target,
    target_path = target_path,
    run_id = run_id,
    run_dir = run_dir
  )
}

parse_mod_annotations <- function(mod_path) {
  annotations <- list(
    model = NA_character_,
    parent = NA_character_,
    description = NA_character_,
    change = NA_character_,
    label = NA_character_,
    author = NA_character_,
    all = data.frame(key = character(), value = character(), stringsAsFactors = FALSE)
  )

  lines <- safe_read_lines(mod_path)
  if (!length(lines)) {
    return(annotations)
  }

  top_lines <- utils::head(lines, 80)
  hits <- lapply(top_lines, function(line) {
    m <- regexec("^;+\\s*([^:]+):\\s*(.*)$", line)
    x <- regmatches(line, m)[[1]]
    if (length(x) == 3) {
      data.frame(key = trimws(x[[2]]), value = trimws(x[[3]]), stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  hits <- Filter(Negate(is.null), hits)
  if (!length(hits)) {
    return(annotations)
  }

  ann_df <- do.call(rbind, hits)
  annotations$all <- ann_df

  normalized_keys <- tolower(gsub("^[0-9xX.\\s]+", "", ann_df$key))
  normalized_keys <- trimws(normalized_keys)

  pick_value <- function(patterns) {
    idx <- which(vapply(patterns, function(pattern) {
      grepl(pattern, normalized_keys, ignore.case = TRUE)
    }, logical(length(normalized_keys))), arr.ind = TRUE)
    if (!length(idx)) {
      return(NA_character_)
    }
    ann_df$value[idx[[1]]]
  }

  annotations$model <- pick_value(c("^model$"))
  annotations$parent <- pick_value(c("^parent$", "^base model$", "^base_model$", "^based on$"))
  annotations$description <- pick_value(c("^description$", "^desc$"))
  annotations$change <- pick_value(c("^change$", "^changes$"))
  annotations$label <- pick_value(c("^label$"))
  annotations$author <- pick_value(c("^author$"))
  annotations
}

parse_bounds_from_code <- function(code) {
  numbers <- regmatches(code, gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[Ee][-+]?[0-9]+)?", code, perl = TRUE))[[1]]
  numbers <- suppressWarnings(as.numeric(numbers))
  if (!length(numbers)) {
    return(list(lower = NA_real_, upper = NA_real_, initial = NA_real_))
  }
  if (grepl("\\(", code)) {
    if (length(numbers) == 1) {
      return(list(lower = NA_real_, upper = NA_real_, initial = numbers[[1]]))
    }
    if (length(numbers) == 2) {
      return(list(lower = numbers[[1]], upper = NA_real_, initial = numbers[[2]]))
    }
    return(list(lower = numbers[[1]], upper = numbers[[3]], initial = numbers[[2]]))
  }
  list(lower = NA_real_, upper = NA_real_, initial = numbers[[1]])
}

parse_parameter_blocks <- function(mod_path) {
  lines <- safe_read_lines(mod_path)
  if (!length(lines)) {
    return(data.frame(
      class = character(),
      index = integer(),
      label = character(),
      fixed = logical(),
      lower_bound = numeric(),
      upper_bound = numeric(),
      initial = numeric(),
      source_line = integer(),
      stringsAsFactors = FALSE
    ))
  }

  blocks <- c("THETA", "OMEGA", "SIGMA")
  active_block <- NA_character_
  counters <- setNames(as.list(rep(0L, length(blocks))), blocks)
  rows <- list()

  for (i in seq_along(lines)) {
    line <- lines[[i]]
    trimmed <- trimws(line)
    if (!nzchar(trimmed) || grepl("^;", trimmed)) {
      next
    }
    if (grepl("^\\$", trimmed)) {
      block_name <- sub("^\\$([A-Za-z0-9_]+).*", "\\1", trimmed)
      if (block_name %in% blocks) {
        active_block <- block_name
        trimmed <- trimws(sub("^\\$[A-Za-z0-9_]+", "", trimmed))
        if (!nzchar(trimmed)) {
          next
        }
      } else {
        active_block <- NA_character_
        next
      }
    }
    if (is.na(active_block)) {
      next
    }

    comment <- ""
    code <- trimmed
    if (grepl(";", code, fixed = TRUE)) {
      pieces <- strsplit(code, ";", fixed = TRUE)[[1]]
      code <- trimws(pieces[[1]])
      comment <- trimws(paste(pieces[-1], collapse = ";"))
    }
    if (!nzchar(code)) {
      next
    }

    counters[[active_block]] <- counters[[active_block]] + 1L
    bounds <- parse_bounds_from_code(code)
    rows[[length(rows) + 1L]] <- data.frame(
      class = active_block,
      index = counters[[active_block]],
      label = if (nzchar(comment)) comment else paste0(active_block, counters[[active_block]]),
      fixed = grepl("\\bFIX(?:ED)?\\b", code, ignore.case = TRUE),
      lower_bound = bounds$lower,
      upper_bound = bounds$upper,
      initial = bounds$initial,
      source_line = i,
      stringsAsFactors = FALSE
    )
  }

  if (!length(rows)) {
    return(data.frame(
      class = character(),
      index = integer(),
      label = character(),
      fixed = logical(),
      lower_bound = numeric(),
      upper_bound = numeric(),
      initial = numeric(),
      source_line = integer(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

extract_table_filenames <- function(mod_path) {
  lines <- safe_read_lines(mod_path)
  out <- c(sdtab = NA_character_, patab = NA_character_, cotab = NA_character_, catab = NA_character_)
  if (!length(lines)) {
    return(out)
  }
  table_blocks <- list()
  current_block <- character()
  in_table <- FALSE

  for (line in lines) {
    trimmed <- trimws(line)
    if (grepl("^\\$", trimmed)) {
      if (in_table && length(current_block)) {
        table_blocks[[length(table_blocks) + 1L]] <- paste(current_block, collapse = " ")
      }
      in_table <- grepl("^\\$TABLE", trimmed, ignore.case = TRUE)
      current_block <- if (in_table) trimmed else character()
      next
    }
    if (in_table) {
      current_block <- c(current_block, trimmed)
    }
  }
  if (in_table && length(current_block)) {
    table_blocks[[length(table_blocks) + 1L]] <- paste(current_block, collapse = " ")
  }

  for (block in table_blocks) {
    hit <- regexec("FILE\\s*=\\s*([^\\s,]+)", block, ignore.case = TRUE, perl = TRUE)
    values <- regmatches(block, hit)[[1]]
    if (length(values) != 2) {
      next
    }
    filename <- trimws(values[[2]])
    key <- tolower(filename)
    if (startsWith(key, "sdtab") && is.na(out[["sdtab"]])) {
      out[["sdtab"]] <- filename
    } else if (startsWith(key, "patab") && is.na(out[["patab"]])) {
      out[["patab"]] <- filename
    } else if (startsWith(key, "cotab") && is.na(out[["cotab"]])) {
      out[["cotab"]] <- filename
    } else if (startsWith(key, "catab") && is.na(out[["catab"]])) {
      out[["catab"]] <- filename
    }
  }
  out
}

locate_run_files <- function(context) {
  run_id <- context$run_id
  project_root <- context$project_root
  run_dir <- context$run_dir

  mod_candidates <- c(
    file.path(project_root, paste0(run_id, ".mod")),
    file.path(project_root, paste0(run_id, ".ctl")),
    file.path(run_dir, paste0(run_id, ".mod")),
    file.path(run_dir, paste0(run_id, ".ctl")),
    file.path(run_dir, "NM_run1", "psn.mod")
  )
  mod_path <- first_existing(mod_candidates)
  table_names <- extract_table_filenames(mod_path)

  resolve_named_file <- function(filename, extra_candidates = character()) {
    if (is.na(filename)) {
      return(NA_character_)
    }
    candidates <- c(
      file.path(project_root, filename),
      file.path(run_dir, filename),
      file.path(run_dir, "NM_run1", filename),
      extra_candidates
    )
    first_existing(candidates)
  }

  raw_results <- discover_raw_results_file(run_id, project_root, run_dir)
  psn_error <- find_psn_error_file(run_dir)

  rows <- list(
    data.frame(file_type = "model", path = mod_path, stringsAsFactors = FALSE),
    data.frame(file_type = "lst", path = first_existing(c(
      file.path(project_root, paste0(run_id, ".lst")),
      file.path(run_dir, paste0(run_id, ".lst")),
      file.path(run_dir, "NM_run1", "psn.lst")
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "ext", path = first_existing(c(
      file.path(project_root, paste0(run_id, ".ext")),
      file.path(run_dir, paste0(run_id, ".ext")),
      file.path(run_dir, "NM_run1", paste0(run_id, ".ext"))
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "cov", path = first_existing(c(
      file.path(project_root, paste0(run_id, ".cov")),
      file.path(run_dir, paste0(run_id, ".cov"))
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "cor", path = first_existing(c(
      file.path(project_root, paste0(run_id, ".cor")),
      file.path(run_dir, paste0(run_id, ".cor"))
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "coi", path = first_existing(c(
      file.path(project_root, paste0(run_id, ".coi")),
      file.path(run_dir, paste0(run_id, ".coi"))
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "phi", path = first_existing(c(
      file.path(project_root, paste0(run_id, ".phi")),
      file.path(run_dir, paste0(run_id, ".phi"))
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "shk", path = first_existing(c(
      file.path(project_root, paste0(run_id, ".shk")),
      file.path(run_dir, paste0(run_id, ".shk"))
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "grd", path = first_existing(c(
      file.path(project_root, paste0(run_id, ".grd")),
      file.path(run_dir, paste0(run_id, ".grd"))
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "xml", path = first_existing(c(
      file.path(project_root, paste0(run_id, ".xml")),
      file.path(run_dir, paste0(run_id, ".xml"))
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "raw_results", path = raw_results, stringsAsFactors = FALSE),
    data.frame(file_type = "command", path = first_existing(c(
      file.path(run_dir, "command.txt"),
      file.path(project_root, "command.txt")
    )), stringsAsFactors = FALSE),
    data.frame(file_type = "psn_error", path = psn_error, stringsAsFactors = FALSE),
    data.frame(file_type = "sdtab", path = resolve_named_file(table_names[["sdtab"]]), stringsAsFactors = FALSE),
    data.frame(file_type = "patab", path = resolve_named_file(table_names[["patab"]]), stringsAsFactors = FALSE),
    data.frame(file_type = "cotab", path = resolve_named_file(table_names[["cotab"]]), stringsAsFactors = FALSE),
    data.frame(file_type = "catab", path = resolve_named_file(table_names[["catab"]]), stringsAsFactors = FALSE)
  )

  file_df <- do.call(rbind, rows)
  file_df$exists <- !is.na(file_df$path) & file.exists(file_df$path)
  file_df$relative_path <- vapply(seq_len(nrow(file_df)), function(i) {
    if (!file_df$exists[[i]]) {
      return(NA_character_)
    }
    relative_to_root(file_df$path[[i]], project_root)
  }, character(1))
  rownames(file_df) <- file_df$file_type
  file_df
}

read_raw_results <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
}

read_nm_table <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    utils::read.table(
      path,
      header = TRUE,
      skip = 1,
      fill = TRUE,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )
}

read_ext_estimates <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  ext_df <- tryCatch(
    utils::read.table(
      path,
      header = TRUE,
      skip = 1,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(ext_df) || !"ITERATION" %in% names(ext_df)) {
    return(NULL)
  }
  final_row <- ext_df[ext_df$ITERATION == -1000000000, , drop = FALSE]
  if (!nrow(final_row)) {
    final_row <- utils::tail(ext_df, 1)
  }
  as.list(final_row[1, , drop = FALSE])
}

safe_flag_value <- function(raw_results, name) {
  if (is.null(raw_results) || !name %in% names(raw_results)) {
    return(NA)
  }
  value <- raw_results[[name]][1]
  if (is.logical(value)) {
    return(value)
  }
  suppressWarnings({
    numeric_value <- as.numeric(value)
    if (!is.na(numeric_value)) {
      return(numeric_value == 1)
    }
  })
  if (is.character(value)) {
    value <- trimws(tolower(value))
    if (value %in% c("true", "t", "yes", "y")) {
      return(TRUE)
    }
    if (value %in% c("false", "f", "no", "n")) {
      return(FALSE)
    }
  }
  NA
}

parse_lst_status <- function(lst_path) {
  lines <- safe_read_lines(lst_path)
  if (!length(lines)) {
    return(list(
      minimization_successful = NA,
      covariance_successful = NA,
      error_clues = character()
    ))
  }

  minimization_successful <- if (any(grepl("MINIMIZATION SUCCESSFUL", lines, ignore.case = TRUE))) {
    TRUE
  } else if (any(grepl("MINIMIZATION TERMINATED", lines, ignore.case = TRUE))) {
    FALSE
  } else {
    NA
  }

  covariance_successful <- if (any(grepl("COVARIANCE MATRIX OF ESTIMATE", lines, ignore.case = TRUE))) {
    TRUE
  } else if (any(grepl("COVARIANCE STEP OMITTED:\\s+YES", lines, ignore.case = TRUE))) {
    FALSE
  } else {
    NA
  }

  clue_hits <- unique(trimws(lines[
    grepl("ERROR|ABORT|FAILED|TERMINATED|ROUNDING|S MATRIX|SINGULAR|NOT POSITIVE", lines, ignore.case = TRUE)
  ]))
  clue_hits <- clue_hits[
    !grepl("^\\$", clue_hits) &
      !grepl("^\\(", clue_hits) &
      !grepl("^;+\\s*", clue_hits)
  ]

  list(
    minimization_successful = minimization_successful,
    covariance_successful = covariance_successful,
    error_clues = utils::head(clue_hits[nzchar(clue_hits)], 12)
  )
}

extract_error_clues <- function(lst_path, psn_error_path) {
  clues <- parse_lst_status(lst_path)$error_clues
  psn_lines <- safe_read_lines(psn_error_path)
  if (length(psn_lines)) {
    psn_lines <- trimws(psn_lines[nzchar(trimws(psn_lines))])
    clues <- unique(c(clues, utils::head(psn_lines, 10)))
  }
  clues
}

parameter_meta_columns <- c(
  "model", "problem", "subproblem", "covariance_step_run",
  "minimization_successful", "covariance_step_successful", "covariance_step_warnings",
  "estimate_near_boundary", "rounding_errors", "zero_gradients", "final_zero_gradients",
  "hessian_reset", "s_matrix_singular", "significant_digits", "condition_number",
  "est_methods", "model_run_time", "subprob_est_time", "subprob_cov_time", "ofv"
)

get_parameter_columns <- function(raw_results) {
  if (is.null(raw_results) || !nrow(raw_results)) {
    return(character())
  }
  cols <- names(raw_results)
  cols[!(
    cols %in% parameter_meta_columns |
      grepl("^se", cols) |
      grepl("^shrinkage_", cols) |
      grepl("^EI[0-9]+$", cols)
  )]
}

derive_shrinkage_map <- function(raw_results) {
  shrinkage <- list()
  if (is.null(raw_results) || !nrow(raw_results)) {
    return(shrinkage)
  }
  cols <- names(raw_results)[grepl("^shrinkage_", names(raw_results))]
  for (col in cols) {
    shrinkage[[col]] <- suppressWarnings(as.numeric(raw_results[[col]][1]))
  }
  shrinkage
}

build_parameter_reference <- function(defs, raw_results = NULL, ext_estimates = NULL) {
  parameter_cols <- get_parameter_columns(raw_results)
  n_rows <- nrow(defs)
  if (!n_rows) {
    return(data.frame(
      class = character(),
      index = integer(),
      parameter = character(),
      estimate = numeric(),
      SE = numeric(),
      lower_bound = numeric(),
      upper_bound = numeric(),
      fixed = logical(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- vector("list", n_rows)
  for (i in seq_len(n_rows)) {
    def <- defs[i, , drop = FALSE]
    parameter_name <- if (length(parameter_cols) >= i) parameter_cols[[i]] else def$label[[1]]

    estimate <- NA_real_
    se <- NA_real_
    if (!is.null(raw_results) && length(parameter_cols) >= i) {
      estimate <- suppressWarnings(as.numeric(raw_results[[parameter_name]][1]))
      se_col <- paste0("se", parameter_name)
      if (se_col %in% names(raw_results)) {
        se <- suppressWarnings(as.numeric(raw_results[[se_col]][1]))
      }
    } else if (!is.null(ext_estimates)) {
      ext_name <- paste0(def$class[[1]], def$index[[1]])
      if (!is.null(ext_estimates[[ext_name]])) {
        estimate <- suppressWarnings(as.numeric(ext_estimates[[ext_name]]))
      }
    }

    rows[[i]] <- data.frame(
      class = def$class[[1]],
      index = def$index[[1]],
      parameter = parameter_name,
      estimate = estimate,
      SE = se,
      lower_bound = def$lower_bound[[1]],
      upper_bound = def$upper_bound[[1]],
      fixed = isTRUE(def$fixed[[1]]),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

compute_eta_shrinkage <- function(defs, parameter_reference, raw_results = NULL, files = NULL) {
  shrinkage_map <- derive_shrinkage_map(raw_results)
  eta_df <- NULL
  if (!is.null(files) && nrow(files)) {
    eta_df <- read_phi_or_patab_etas(files["phi", "path"], files["patab", "path"])
  }

  omega_defs <- defs[defs$class == "OMEGA", , drop = FALSE]
  if (!nrow(omega_defs)) {
    return(setNames(list(), character()))
  }

  eta_shrinkage <- setNames(as.list(rep(NA_real_, nrow(omega_defs))), paste0("ETA", omega_defs$index))
  for (i in seq_len(nrow(omega_defs))) {
    eta_index <- omega_defs$index[[i]]
    eta_key <- paste0("shrinkage_eta", eta_index, "(%)")
    shrinkage_value <- shrinkage_map[[eta_key]] %||% NA_real_

    if (!is.finite(shrinkage_value) && !is.null(eta_df)) {
      eta_col <- paste0("ETA", eta_index)
      ref_row <- parameter_reference[
        parameter_reference$class == "OMEGA" & parameter_reference$index == eta_index,
        ,
        drop = FALSE
      ]
      omega_estimate <- if (nrow(ref_row)) ref_row$estimate[[1]] else NA_real_

      if (
        eta_col %in% names(eta_df) &&
        is.finite(omega_estimate) &&
        omega_estimate > 0
      ) {
        eta_values <- eta_df[[eta_col]]
        eta_values <- eta_values[is.finite(eta_values)]
        if (length(eta_values) >= 2) {
          # Fallback: derive ETA shrinkage from EBEs and OMEGA when PsN did not export shrinkage explicitly.
          shrinkage_value <- 100 * (1 - stats::var(eta_values, na.rm = TRUE) / omega_estimate)
        }
      }
    }

    eta_shrinkage[[paste0("ETA", eta_index)]] <- if (is.finite(shrinkage_value)) shrinkage_value else NA_real_
  }

  eta_shrinkage
}

compute_eps_shrinkage <- function(raw_results = NULL) {
  shrinkage_map <- derive_shrinkage_map(raw_results)
  eps_shrinkage <- shrinkage_map[["shrinkage_iwres(%)"]] %||% NA_real_
  if (is.finite(eps_shrinkage)) {
    return(eps_shrinkage)
  }
  NA_real_
}

build_parameter_table <- function(defs, raw_results = NULL, ext_estimates = NULL, files = NULL) {
  parameter_reference <- build_parameter_reference(defs, raw_results, ext_estimates)
  n_rows <- nrow(parameter_reference)
  if (!n_rows) {
    return(data.frame(
      parameter = character(),
      estimate = numeric(),
      SE = numeric(),
      RSE = numeric(),
      CI = character(),
      CI_lower = numeric(),
      CI_upper = numeric(),
      fixed_estimated = character(),
      boundary_flag = character(),
      ETA_shrinkage = numeric(),
      EPS_shrinkage = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  eta_shrinkage <- compute_eta_shrinkage(defs, parameter_reference, raw_results, files)
  eps_shrinkage <- compute_eps_shrinkage(raw_results)
  rows <- vector("list", n_rows)
  global_boundary <- safe_flag_value(raw_results, "estimate_near_boundary")

  for (i in seq_len(n_rows)) {
    ref <- parameter_reference[i, , drop = FALSE]
    estimate <- ref$estimate[[1]]
    se <- ref$SE[[1]]

    rse <- if (!is.na(estimate) && !is.na(se) && abs(estimate) > .Machine$double.eps) {
      abs(se / estimate) * 100
    } else {
      NA_real_
    }

    ci_lower <- if (!is.na(estimate) && !is.na(se)) estimate - 1.96 * se else NA_real_
    ci_upper <- if (!is.na(estimate) && !is.na(se)) estimate + 1.96 * se else NA_real_
    ci <- if (!is.na(ci_lower) && !is.na(ci_upper)) {
      paste0("[", format_value(ci_lower, digits = 4), ", ", format_value(ci_upper, digits = 4), "]")
    } else {
      NA_character_
    }

    lower_bound <- ref$lower_bound[[1]]
    upper_bound <- ref$upper_bound[[1]]
    tolerance <- if (!is.na(estimate)) max(1e-06, abs(estimate) * 1e-04) else NA_real_
    boundary_flag <- ""
    if (!is.na(lower_bound) && !is.na(estimate) && abs(estimate - lower_bound) <= tolerance) {
      boundary_flag <- "near_lower"
    }
    if (!is.na(upper_bound) && !is.na(estimate) && abs(estimate - upper_bound) <= tolerance) {
      boundary_flag <- if (nzchar(boundary_flag)) paste(boundary_flag, "near_upper", sep = ";") else "near_upper"
    }
    if (!nzchar(boundary_flag) && isTRUE(global_boundary)) {
      boundary_flag <- "global_warning"
    }

    eta_shrinkage_value <- NA_real_
    eps_shrinkage_value <- NA_real_
    if (ref$class[[1]] == "OMEGA") {
      eta_shrinkage_value <- eta_shrinkage[[paste0("ETA", ref$index[[1]])]] %||% NA_real_
    }
    if (ref$class[[1]] == "SIGMA") {
      eps_shrinkage_value <- eps_shrinkage
    }

    rows[[i]] <- data.frame(
      parameter = ref$parameter[[1]],
      estimate = estimate,
      SE = se,
      RSE = rse,
      CI = ci,
      CI_lower = ci_lower,
      CI_upper = ci_upper,
      fixed_estimated = if (isTRUE(ref$fixed[[1]])) "fixed" else "estimated",
      boundary_flag = if (nzchar(boundary_flag)) boundary_flag else NA_character_,
      ETA_shrinkage = eta_shrinkage_value,
      EPS_shrinkage = eps_shrinkage_value,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

maybe_numeric <- function(value) {
  suppressWarnings(as.numeric(value))
}

build_status <- function(raw_results, lst_path, psn_error_path, ext_estimates = NULL) {
  lst_status <- parse_lst_status(lst_path)
  min_success <- safe_flag_value(raw_results, "minimization_successful")
  cov_success <- safe_flag_value(raw_results, "covariance_step_successful")
  runtime <- if (!is.null(raw_results) && "model_run_time" %in% names(raw_results)) raw_results$model_run_time[[1]] else NA_character_
  ofv <- if (!is.null(raw_results) && "ofv" %in% names(raw_results)) maybe_numeric(raw_results$ofv[[1]]) else NA_real_
  if (is.na(ofv) && !is.null(ext_estimates) && !is.null(ext_estimates[["OBJ"]])) {
    ofv <- maybe_numeric(ext_estimates[["OBJ"]])
  }
  condition_number <- if (!is.null(raw_results) && "condition_number" %in% names(raw_results)) maybe_numeric(raw_results$condition_number[[1]]) else NA_real_
  rounding_errors <- safe_flag_value(raw_results, "rounding_errors")
  zero_gradients <- safe_flag_value(raw_results, "zero_gradients")
  boundary_warning <- safe_flag_value(raw_results, "estimate_near_boundary")

  list(
    minimization_successful = if (!is.na(min_success)) min_success else lst_status$minimization_successful,
    covariance_successful = if (!is.na(cov_success)) cov_success else lst_status$covariance_successful,
    runtime = runtime,
    ofv = ofv,
    condition_number = condition_number,
    rounding_errors = rounding_errors,
    zero_gradients = zero_gradients,
    estimate_near_boundary = boundary_warning,
    error_clues = extract_error_clues(lst_path, psn_error_path)
  )
}

read_phi_or_patab_etas <- function(phi_path, patab_path) {
  phi_df <- read_nm_table(phi_path)
  if (!is.null(phi_df)) {
    eta_cols <- grep("^ETA\\(", names(phi_df), value = TRUE)
    if (length(eta_cols)) {
      keep <- c("ID", eta_cols)
      keep <- keep[keep %in% names(phi_df)]
      eta_df <- phi_df[, keep, drop = FALSE]
      eta_df <- eta_df[!duplicated(eta_df$ID), , drop = FALSE]
      names(eta_df) <- gsub("^ETA\\(([0-9]+)\\)$", "ETA\\1", names(eta_df))
      return(eta_df)
    }
  }

  patab_df <- read_nm_table(patab_path)
  if (!is.null(patab_df)) {
    eta_cols <- grep("^ETA[0-9]+$", names(patab_df), value = TRUE)
    if (length(eta_cols)) {
      keep <- c("ID", eta_cols)
      keep <- keep[keep %in% names(patab_df)]
      eta_df <- patab_df[, keep, drop = FALSE]
      eta_df <- eta_df[!duplicated(eta_df$ID), , drop = FALSE]
      return(eta_df)
    }
  }

  NULL
}

draw_scatter_panel <- function(x, y, title, xlab, ylab, identity = FALSE, horizontal_zero = FALSE) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]

  graphics::plot(
    x, y,
    pch = 19,
    cex = 0.6,
    col = grDevices::rgb(27 / 255, 78 / 255, 116 / 255, alpha = 0.45),
    xlab = xlab,
    ylab = ylab,
    main = title
  )
  graphics::grid(col = "#d8dee9")

  if (identity) {
    graphics::abline(0, 1, col = "black", lwd = 2, lty = 1)
  }
  if (horizontal_zero) {
    graphics::abline(h = 0, col = "black", lwd = 2, lty = 1)
  }

  legend_items <- character()
  legend_cols <- character()
  legend_lty <- integer()
  legend_lwd <- numeric()

  if (identity || horizontal_zero) {
    legend_items <- c(legend_items, "Reference")
    legend_cols <- c(legend_cols, "black")
    legend_lty <- c(legend_lty, 1)
    legend_lwd <- c(legend_lwd, 2)
  }

  if (length(unique(x)) >= 2) {
    linear_fit <- tryCatch(stats::lm(y ~ x), error = function(e) NULL)
    if (!is.null(linear_fit)) {
      graphics::abline(linear_fit, col = "#1f77b4", lwd = 2, lty = 1)
      legend_items <- c(legend_items, "Linear")
      legend_cols <- c(legend_cols, "#1f77b4")
      legend_lty <- c(legend_lty, 1)
      legend_lwd <- c(legend_lwd, 2)
    }
  }

  if (length(unique(x)) >= 4) {
    loess_fit <- tryCatch(stats::loess(y ~ x, span = 0.75), error = function(e) NULL)
    if (!is.null(loess_fit)) {
      x_grid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 200)
      y_loess <- tryCatch(stats::predict(loess_fit, newdata = data.frame(x = x_grid)), error = function(e) rep(NA_real_, length(x_grid)))
      keep_loess <- is.finite(x_grid) & is.finite(y_loess)
      if (any(keep_loess)) {
        graphics::lines(x_grid[keep_loess], y_loess[keep_loess], col = "#d62728", lwd = 2, lty = 2)
        legend_items <- c(legend_items, "Loess")
        legend_cols <- c(legend_cols, "#d62728")
        legend_lty <- c(legend_lty, 2)
        legend_lwd <- c(legend_lwd, 2)
      }
    }
  }

  if (length(legend_items)) {
    graphics::legend("topleft", legend = legend_items, col = legend_cols, lty = legend_lty, lwd = legend_lwd, bty = "n", cex = 0.8)
  }
}

plot_scatter <- function(x, y, output_path, title, xlab, ylab, identity = FALSE, horizontal_zero = FALSE) {
  grDevices::png(output_path, width = 1400, height = 1100, res = 160)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(5, 5, 4, 2) + 0.1)
  draw_scatter_panel(x, y, title, xlab, ylab, identity = identity, horizontal_zero = horizontal_zero)
}

plot_gof_overview <- function(sdtab_df, output_path) {
  if (is.null(sdtab_df)) {
    return(FALSE)
  }
  required_cols <- c("DV", "PRED", "IPRED", "CWRES", "TIME")
  if (!all(required_cols %in% names(sdtab_df))) {
    return(FALSE)
  }
  df <- sdtab_df
  if ("MDV" %in% names(df)) {
    df <- df[df$MDV == 0, , drop = FALSE]
  }
  df <- df[stats::complete.cases(df[, required_cols]), , drop = FALSE]
  if (!nrow(df)) {
    return(FALSE)
  }

  grDevices::png(output_path, width = 1600, height = 1400, res = 160)
  on.exit(grDevices::dev.off(), add = TRUE)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1.5))

  draw_scatter_panel(df$PRED, df$DV, "DV vs PRED", "PRED", "DV", identity = TRUE)
  draw_scatter_panel(df$IPRED, df$DV, "DV vs IPRED", "IPRED", "DV", identity = TRUE)
  draw_scatter_panel(df$TIME, df$CWRES, "CWRES vs TIME", "TIME", "CWRES", horizontal_zero = TRUE)
  draw_scatter_panel(df$PRED, df$CWRES, "CWRES vs PRED", "PRED", "CWRES", horizontal_zero = TRUE)

  TRUE
}

plot_eta_histograms <- function(eta_df, output_path) {
  if (is.null(eta_df)) {
    return(FALSE)
  }
  eta_cols <- grep("^ETA[0-9]+$", names(eta_df), value = TRUE)
  if (!length(eta_cols)) {
    return(FALSE)
  }
  n_cols <- min(2, length(eta_cols))
  n_rows <- ceiling(length(eta_cols) / n_cols)

  grDevices::png(output_path, width = 1600, height = max(900, 700 * n_rows), res = 160)
  on.exit(grDevices::dev.off(), add = TRUE)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(n_rows, n_cols), mar = c(4.5, 4.5, 3, 1.5))

  for (eta_col in eta_cols) {
    values <- eta_df[[eta_col]]
    values <- values[is.finite(values)]
    if (!length(values)) {
      graphics::plot.new()
      graphics::title(main = eta_col)
      graphics::text(0.5, 0.5, "No ETA values available")
      next
    }
    graphics::hist(
      values,
      breaks = "FD",
      col = "#7ea8be",
      border = "white",
      main = paste(eta_col, "distribution"),
      xlab = eta_col
    )
    graphics::abline(v = 0, col = "#b23a48", lwd = 2, lty = 2)
    graphics::grid(col = "#d8dee9")
  }

  TRUE
}

plot_shrinkage <- function(parameter_table, output_path) {
  if (is.null(parameter_table) || !nrow(parameter_table)) {
    return(FALSE)
  }
  values <- ifelse(
    is.finite(parameter_table$ETA_shrinkage),
    parameter_table$ETA_shrinkage,
    parameter_table$EPS_shrinkage
  )
  keep <- is.finite(values)
  if (!any(keep)) {
    return(FALSE)
  }
  values <- values[keep]
  labels <- parameter_table$parameter[keep]

  grDevices::png(output_path, width = 1600, height = 1000, res = 160)
  on.exit(grDevices::dev.off(), add = TRUE)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(9, 5, 4, 2) + 0.1)
  mids <- graphics::barplot(
    values,
    names.arg = labels,
    las = 2,
    col = "#7ea8be",
    main = "Shrinkage summary",
    ylab = "Percent"
  )
  graphics::grid(nx = NA, ny = NULL, col = "#d8dee9")
  graphics::text(mids, values, labels = format_value(values, digits = 3), pos = 3, cex = 0.8)
  TRUE
}

plot_record <- function(output_plot_dir, out_file, name, title, category) {
  data.frame(
    name = name,
    title = title,
    category = category,
    file = out_file,
    relative_path = normalize_slashes(file.path("..", "output", basename(dirname(output_plot_dir)), "plots", basename(out_file))),
    stringsAsFactors = FALSE
  )
}

filter_sdtab_rows <- function(sdtab_df, required_cols) {
  df <- sdtab_df
  if ("MDV" %in% names(df)) {
    df <- df[df$MDV == 0, , drop = FALSE]
  }
  keep_cols <- required_cols[required_cols %in% names(df)]
  if (!length(keep_cols)) {
    return(df[0, , drop = FALSE])
  }
  df[stats::complete.cases(df[, keep_cols, drop = FALSE]), , drop = FALSE]
}

plot_individual_fit_pages <- function(sdtab_df, output_plot_dir, max_panels_per_page = 12) {
  plots <- data.frame(
    name = character(),
    title = character(),
    category = character(),
    file = character(),
    relative_path = character(),
    stringsAsFactors = FALSE
  )
  notes <- character()

  if (is.null(sdtab_df)) {
    notes <- c(notes, "Individual fitting plots were not generated because SDTAB was not available.")
    return(list(plots = plots, notes = notes))
  }

  required_base <- c("ID", "TIME", "DV")
  if (!all(required_base %in% names(sdtab_df))) {
    notes <- c(notes, "Individual fitting plots were not generated because SDTAB is missing one of ID, TIME, or DV.")
    return(list(plots = plots, notes = notes))
  }

  has_pred <- "PRED" %in% names(sdtab_df)
  has_ipred <- "IPRED" %in% names(sdtab_df)
  if (!has_pred && !has_ipred) {
    notes <- c(notes, "Individual fitting plots were not generated because neither PRED nor IPRED was available in SDTAB.")
    return(list(plots = plots, notes = notes))
  }
  if (!has_pred) {
    notes <- c(notes, "Individual fitting plots used only IPRED because PRED was not present in SDTAB.")
  }
  if (!has_ipred) {
    notes <- c(notes, "Individual fitting plots used only PRED because IPRED was not present in SDTAB.")
  }

  keep_cols <- c(required_base, if (has_pred) "PRED", if (has_ipred) "IPRED")
  df <- filter_sdtab_rows(sdtab_df, required_base)
  if (!nrow(df)) {
    notes <- c(notes, "Individual fitting plots were not generated because there were no observation rows after MDV filtering.")
    return(list(plots = plots, notes = notes))
  }
  df <- df[, keep_cols, drop = FALSE]

  subject_ids <- sort(unique(df$ID))
  page_index <- ceiling(seq_along(subject_ids) / max_panels_per_page)
  subject_pages <- split(subject_ids, page_index)

  for (page_id in seq_along(subject_pages)) {
    ids <- subject_pages[[page_id]]
    n_panels <- length(ids)
    n_cols <- min(3, n_panels)
    n_rows <- ceiling(n_panels / n_cols)
    out_file <- file.path(output_plot_dir, sprintf("individual_fits_page_%02d.png", page_id))

    grDevices::png(out_file, width = 1800, height = max(1100, 520 * n_rows), res = 160)
    old_par <- graphics::par(no.readonly = TRUE)
    graphics::par(mfrow = c(n_rows, n_cols), mar = c(4.2, 4.2, 3, 1))

    for (subject_id in ids) {
      subject_df <- df[df$ID == subject_id, , drop = FALSE]
      subject_df <- subject_df[order(subject_df$TIME), , drop = FALSE]
      y_values <- c(
        subject_df$DV,
        if (has_pred) subject_df$PRED,
        if (has_ipred) subject_df$IPRED
      )
      y_values <- y_values[is.finite(y_values)]

      if (!length(y_values)) {
        graphics::plot.new()
        graphics::title(main = paste("ID", subject_id))
        graphics::text(0.5, 0.5, "No plotting values available")
        next
      }

      x_values <- subject_df$TIME[is.finite(subject_df$TIME)]
      x_range <- range(x_values, na.rm = TRUE)
      if (!all(is.finite(x_range)) || diff(x_range) == 0) {
        x_range <- c(min(x_values, na.rm = TRUE) - 0.5, max(x_values, na.rm = TRUE) + 0.5)
      }
      y_range <- range(y_values, na.rm = TRUE)
      if (!all(is.finite(y_range)) || diff(y_range) == 0) {
        y_range <- c(min(y_values, na.rm = TRUE) - 0.5, max(y_values, na.rm = TRUE) + 0.5)
      }

      graphics::plot(
        NA,
        xlim = x_range,
        ylim = y_range,
        xlab = "TIME",
        ylab = "DV / PRED / IPRED",
        main = paste("ID", subject_id)
      )
      graphics::grid(col = "#d8dee9")

      if (has_pred) {
        pred_df <- subject_df[is.finite(subject_df$TIME) & is.finite(subject_df$PRED), , drop = FALSE]
        if (nrow(pred_df)) {
          graphics::lines(pred_df$TIME, pred_df$PRED, col = "#1f77b4", lwd = 2)
        }
      }
      if (has_ipred) {
        ipred_df <- subject_df[is.finite(subject_df$TIME) & is.finite(subject_df$IPRED), , drop = FALSE]
        if (nrow(ipred_df)) {
          graphics::lines(ipred_df$TIME, ipred_df$IPRED, col = "#d62728", lwd = 2)
        }
      }

      dv_df <- subject_df[is.finite(subject_df$TIME) & is.finite(subject_df$DV), , drop = FALSE]
      if (nrow(dv_df)) {
        graphics::points(dv_df$TIME, dv_df$DV, pch = 16, cex = 0.65, col = "black")
      }

      legend_items <- c()
      legend_cols <- c()
      legend_lty <- c()
      legend_pch <- c()
      if (has_pred) {
        legend_items <- c(legend_items, "PRED")
        legend_cols <- c(legend_cols, "#1f77b4")
        legend_lty <- c(legend_lty, 1)
        legend_pch <- c(legend_pch, NA)
      }
      if (has_ipred) {
        legend_items <- c(legend_items, "IPRED")
        legend_cols <- c(legend_cols, "#d62728")
        legend_lty <- c(legend_lty, 1)
        legend_pch <- c(legend_pch, NA)
      }
      legend_items <- c(legend_items, "DV")
      legend_cols <- c(legend_cols, "black")
      legend_lty <- c(legend_lty, NA)
      legend_pch <- c(legend_pch, 16)
      graphics::legend("topright", legend = legend_items, col = legend_cols, lty = legend_lty, pch = legend_pch, bty = "n", cex = 0.75)
    }

    total_panels <- n_rows * n_cols
    if (total_panels > n_panels) {
      for (i in seq_len(total_panels - n_panels)) {
        graphics::plot.new()
      }
    }

    grDevices::dev.off()
    graphics::par(old_par)
    plots <- rbind(
      plots,
      plot_record(output_plot_dir, out_file, sprintf("individual_fits_page_%02d", page_id), sprintf("Individual fitting page %d", page_id), "individual")
    )
  }

  list(plots = plots, notes = unique(notes))
}

generate_diagnostic_plots <- function(files, raw_results, parameter_table, output_plot_dir) {
  ensure_dir(output_plot_dir)
  plots <- data.frame(
    name = character(),
    title = character(),
    category = character(),
    file = character(),
    relative_path = character(),
    stringsAsFactors = FALSE
  )
  notes <- character()

  sdtab_df <- read_nm_table(files["sdtab", "path"])
  if (!is.null(sdtab_df)) {
    if (all(c("DV", "PRED") %in% names(sdtab_df))) {
      df <- filter_sdtab_rows(sdtab_df, c("DV", "PRED"))
      if (nrow(df)) {
        out_file <- file.path(output_plot_dir, "dv_vs_pred.png")
        plot_scatter(df$PRED, df$DV, out_file, "DV vs PRED", "PRED", "DV", identity = TRUE)
        plots <- rbind(plots, plot_record(output_plot_dir, out_file, "dv_vs_pred", "DV vs PRED", "gof"))
      } else {
        notes <- c(notes, "SDTAB found, but DV/PRED rows were empty after MDV filtering.")
      }
    } else {
      notes <- c(notes, "SDTAB found, but DV vs PRED could not be generated because columns were missing.")
    }

    if (all(c("DV", "IPRED") %in% names(sdtab_df))) {
      df <- filter_sdtab_rows(sdtab_df, c("DV", "IPRED"))
      if (nrow(df)) {
        out_file <- file.path(output_plot_dir, "dv_vs_ipred.png")
        plot_scatter(df$IPRED, df$DV, out_file, "DV vs IPRED", "IPRED", "DV", identity = TRUE)
        plots <- rbind(plots, plot_record(output_plot_dir, out_file, "dv_vs_ipred", "DV vs IPRED", "gof"))
      }
    }

    if (all(c("CWRES", "TIME") %in% names(sdtab_df))) {
      df <- filter_sdtab_rows(sdtab_df, c("CWRES", "TIME"))
      if (nrow(df)) {
        out_file <- file.path(output_plot_dir, "cwres_vs_time.png")
        plot_scatter(df$TIME, df$CWRES, out_file, "CWRES vs TIME", "TIME", "CWRES", horizontal_zero = TRUE)
        plots <- rbind(plots, plot_record(output_plot_dir, out_file, "cwres_vs_time", "CWRES vs TIME", "gof"))
      }
    }

    if (all(c("CWRES", "PRED") %in% names(sdtab_df))) {
      df <- filter_sdtab_rows(sdtab_df, c("CWRES", "PRED"))
      if (nrow(df)) {
        out_file <- file.path(output_plot_dir, "cwres_vs_pred.png")
        plot_scatter(df$PRED, df$CWRES, out_file, "CWRES vs PRED", "PRED", "CWRES", horizontal_zero = TRUE)
        plots <- rbind(plots, plot_record(output_plot_dir, out_file, "cwres_vs_pred", "CWRES vs PRED", "gof"))
      }
    }

    gof_file <- file.path(output_plot_dir, "gof_overview.png")
    if (plot_gof_overview(sdtab_df, gof_file)) {
      plots <- rbind(plots, plot_record(output_plot_dir, gof_file, "gof_overview", "GOF overview", "gof"))
    }
  } else {
    notes <- c(notes, "SDTAB not found, so GOF scatter plots could not be generated.")
  }

  eta_df <- read_phi_or_patab_etas(files["phi", "path"], files["patab", "path"])
  eta_file <- file.path(output_plot_dir, "eta_distribution.png")
  if (plot_eta_histograms(eta_df, eta_file)) {
    plots <- rbind(plots, plot_record(output_plot_dir, eta_file, "eta_distribution", "ETA distribution", "diagnostic"))
  } else {
    notes <- c(notes, "ETA distribution plot was not generated because neither PHI nor PATAB supplied usable ETA columns.")
  }

  shrinkage_file <- file.path(output_plot_dir, "shrinkage_summary.png")
  if (plot_shrinkage(parameter_table, shrinkage_file)) {
    plots <- rbind(plots, plot_record(output_plot_dir, shrinkage_file, "shrinkage_summary", "Shrinkage summary", "diagnostic"))
  } else {
    notes <- c(notes, "Shrinkage summary plot was not generated because shrinkage columns were not available.")
  }

  individual_fit_results <- plot_individual_fit_pages(sdtab_df, output_plot_dir)
  if (nrow(individual_fit_results$plots)) {
    plots <- rbind(plots, individual_fit_results$plots)
  }
  notes <- c(notes, individual_fit_results$notes)

  list(plots = plots, notes = unique(notes))
}

to_html_table <- function(df, digits = 4, na_label = "NA") {
  if (is.null(df) || !nrow(df)) {
    return("<p class=\"muted\">No rows available.</p>")
  }

  header <- paste0(
    "<tr>",
    paste(sprintf("<th>%s</th>", escape_html(names(df))), collapse = ""),
    "</tr>"
  )

  rows <- apply(df, 1, function(row) {
    cells <- vapply(seq_along(row), function(i) {
      value <- row[[i]]
      if (is.na(value) || identical(value, "NA")) {
        value <- na_label
      } else if (suppressWarnings(!is.na(as.numeric(value))) && !grepl("^0[0-9]+$", value)) {
        numeric_value <- suppressWarnings(as.numeric(value))
        if (!is.na(numeric_value)) {
          value <- format_value(numeric_value, digits = digits)
        }
      }
      sprintf("<td>%s</td>", escape_html(value))
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  })

  paste0("<table><thead>", header, "</thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
}

status_card <- function(label, value, good = NULL) {
  class_name <- if (isTRUE(good)) {
    "status-good"
  } else if (identical(good, FALSE)) {
    "status-bad"
  } else {
    "status-neutral"
  }
  sprintf(
    "<div class=\"status-card %s\"><div class=\"status-label\">%s</div><div class=\"status-value\">%s</div></div>",
    class_name,
    escape_html(label),
    escape_html(value)
  )
}

plot_gallery_html <- function(plot_df, empty_message) {
  if (is.null(plot_df) || !nrow(plot_df)) {
    return(sprintf("<p class=\"muted\">%s</p>", escape_html(empty_message)))
  }

  image_src <- function(path) {
    if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) {
      return("")
    }
    base64enc::dataURI(file = path, mime = "image/png")
  }

  paste(apply(plot_df, 1, function(row) {
    sprintf(
      "<figure><img src=\"%s\" alt=\"%s\" /><figcaption>%s</figcaption></figure>",
      escape_html(image_src(row[["file"]])),
      escape_html(row[["title"]]),
      escape_html(row[["title"]])
    )
  }), collapse = "\n")
}

render_html_report <- function(summary, template_path, output_html) {
  template <- paste(safe_read_lines(template_path), collapse = "\n")
  if (!nzchar(template)) {
    stop("Report template is empty or missing.")
  }

  metadata_df <- data.frame(
    field = c("run_id", "model_file", "description", "base_model", "change", "author", "runtime", "OFV"),
    value = c(
      summary$run_id,
      summary$model_file %||% NA_character_,
      summary$description %||% NA_character_,
      summary$base_model %||% NA_character_,
      summary$change %||% NA_character_,
      summary$author %||% NA_character_,
      summary$status$runtime %||% NA_character_,
      if (!is.na(summary$status$ofv)) format_value(summary$status$ofv, digits = 6) else NA_character_
    ),
    stringsAsFactors = FALSE
  )

  status_cards <- paste(
    status_card("Minimization", ifelse(isTRUE(summary$status$minimization_successful), "successful",
      ifelse(identical(summary$status$minimization_successful, FALSE), "failed", "unknown")
    ), if (!is.na(summary$status$minimization_successful)) summary$status$minimization_successful else NULL),
    status_card("Covariance", ifelse(isTRUE(summary$status$covariance_successful), "successful",
      ifelse(identical(summary$status$covariance_successful, FALSE), "failed", "unknown")
    ), if (!is.na(summary$status$covariance_successful)) summary$status$covariance_successful else NULL),
    status_card("Runtime", summary$status$runtime %||% "NA"),
    status_card("OFV", if (!is.na(summary$status$ofv)) format_value(summary$status$ofv, digits = 6) else "NA"),
    sep = ""
  )

  file_df <- summary$files[, c("file_type", "exists", "relative_path"), drop = FALSE]
  names(file_df) <- c("file_type", "exists", "path")
  file_df$exists <- ifelse(file_df$exists, "yes", "no")

  error_section <- if (length(summary$status$error_clues)) {
    paste0(
      "<ul>",
      paste(sprintf("<li>%s</li>", escape_html(summary$status$error_clues)), collapse = ""),
      "</ul>"
    )
  } else {
    "<p class=\"muted\">No obvious NONMEM / PsN error clues were found in the available files.</p>"
  }

  gof_plots <- summary$plots[summary$plots$category == "gof", , drop = FALSE]
  diagnostic_plots <- summary$plots[summary$plots$category == "diagnostic", , drop = FALSE]
  individual_plots <- summary$plots[summary$plots$category == "individual", , drop = FALSE]

  notes_section <- if (length(summary$plot_notes)) {
    paste0(
      "<ul>",
      paste(sprintf("<li>%s</li>", escape_html(summary$plot_notes)), collapse = ""),
      "</ul>"
    )
  } else {
    "<p class=\"muted\">No diagnostic-generation warnings were recorded.</p>"
  }

  replacements <- c(
    "{{REPORT_TITLE}}" = escape_html(paste(summary$run_id, "summary")),
    "{{RUN_ID}}" = escape_html(summary$run_id),
    "{{GENERATED_AT}}" = escape_html(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "{{STATUS_CARDS}}" = status_cards,
    "{{MODEL_METADATA}}" = to_html_table(metadata_df, digits = 6),
    "{{FILE_TABLE}}" = to_html_table(file_df, digits = 6),
    "{{PARAMETER_TABLE}}" = to_html_table(summary$parameter_table, digits = 6),
    "{{TRACKER_TABLE}}" = to_html_table(summary$tracker_preview, digits = 6),
    "{{ERROR_SECTION}}" = error_section,
    "{{GOF_GALLERY}}" = plot_gallery_html(gof_plots, "No GOF plots were generated for this run."),
    "{{DIAGNOSTIC_GALLERY}}" = plot_gallery_html(diagnostic_plots, "No non-GOF diagnostic plots were generated for this run."),
    "{{INDIVIDUAL_GALLERY}}" = plot_gallery_html(individual_plots, "No individual fitting plots were generated for this run."),
    "{{NOTES_SECTION}}" = notes_section
  )

  for (key in names(replacements)) {
    template <- gsub(key, replacements[[key]], template, fixed = TRUE)
  }

  writeLines(template, con = output_html, useBytes = TRUE)
  invisible(output_html)
}

combine_description <- function(annotations) {
  pieces <- c()
  if (!is.na(annotations$description) && nzchar(annotations$description)) {
    pieces <- c(pieces, annotations$description)
  }
  if (!is.na(annotations$change) && nzchar(annotations$change)) {
    pieces <- c(pieces, paste0("Change: ", annotations$change))
  }
  if (!length(pieces)) {
    return(NA_character_)
  }
  paste(pieces, collapse = " | ")
}

build_run_summary <- function(context, generate_artifacts = TRUE) {
  project_root <- context$project_root
  run_id <- context$run_id
  output_dir <- file.path(project_root, "output", run_id)
  plot_dir <- file.path(output_dir, "plots")

  files <- locate_run_files(context)
  annotations <- parse_mod_annotations(files["model", "path"])
  defs <- parse_parameter_blocks(files["model", "path"])
  raw_results <- read_raw_results(files["raw_results", "path"])
  ext_estimates <- read_ext_estimates(files["ext", "path"])
  parameter_table <- build_parameter_table(defs, raw_results, ext_estimates, files)
  status <- build_status(raw_results, files["lst", "path"], files["psn_error", "path"], ext_estimates)

  param_csv <- file.path(output_dir, paste0(run_id, "_parameters.csv"))
  plots <- data.frame(
    name = character(),
    title = character(),
    category = character(),
    file = character(),
    relative_path = character(),
    stringsAsFactors = FALSE
  )
  plot_notes <- character()

  if (generate_artifacts) {
    ensure_dir(output_dir)
    utils::write.csv(parameter_table, param_csv, row.names = FALSE, na = "")
    plot_results <- generate_diagnostic_plots(files, raw_results, parameter_table, plot_dir)
    plots <- plot_results$plots
    plot_notes <- plot_results$notes
  }

  summary <- list(
    run_id = run_id,
    project_root = project_root,
    model_file = files["model", "relative_path"],
    files = files,
    annotations = annotations,
    description = combine_description(annotations),
    base_model = annotations$parent %||% NA_character_,
    change = annotations$change %||% NA_character_,
    author = annotations$author %||% NA_character_,
    raw_results = raw_results,
    parameter_table = parameter_table,
    parameter_csv = relative_to_root(param_csv, project_root),
    status = status,
    output_path = normalize_slashes(file.path("output", run_id)),
    report_path = normalize_slashes(file.path("reports", paste0(run_id, "_summary.html"))),
    plots = plots,
    plot_notes = plot_notes
  )

  summary$n_parameters <- if (nrow(parameter_table)) {
    sum(parameter_table$fixed_estimated == "estimated", na.rm = TRUE)
  } else {
    0
  }

  summary
}

discover_run_ids <- function(project_root) {
  model_files <- list.files(project_root, pattern = "\\.(mod|ctl)$", full.names = FALSE)
  model_ids <- sub("\\.(mod|ctl)$", "", model_files)

  raw_result_files <- list.files(project_root, pattern = "^raw_results_.*\\.csv$", recursive = TRUE, full.names = TRUE)
  raw_ids <- character()
  for (path in raw_result_files) {
    run_id <- sub("^raw_results_(.*)\\.csv$", "\\1", basename(path))
    parent <- basename(dirname(path))
    if (parent == run_id || grepl(paste0("^", escape_regex(run_id), "\\.dir[0-9]+$"), parent)) {
      raw_ids <- c(raw_ids, run_id)
    }
  }

  order_run_ids(unique(c(model_ids, raw_ids)))
}

tracker_row_from_summary <- function(summary) {
  data.frame(
    run_id = summary$run_id,
    model_file = summary$model_file %||% NA_character_,
    description = summary$description %||% NA_character_,
    base_model = summary$base_model %||% NA_character_,
    minimization_successful = summary$status$minimization_successful,
    covariance_successful = summary$status$covariance_successful,
    OFV = summary$status$ofv,
    delta_OFV_vs_previous = NA_real_,
    n_parameters = summary$n_parameters,
    runtime = summary$status$runtime %||% NA_character_,
    output_path = summary$output_path,
    report_path = summary$report_path,
    stringsAsFactors = FALSE
  )
}

build_model_tracker <- function(project_root, current_summary = NULL) {
  run_ids <- discover_run_ids(project_root)
  if (!length(run_ids)) {
    tracker <- data.frame(
      run_id = character(),
      model_file = character(),
      description = character(),
      base_model = character(),
      minimization_successful = logical(),
      covariance_successful = logical(),
      OFV = numeric(),
      delta_OFV_vs_previous = numeric(),
      n_parameters = integer(),
      runtime = character(),
      output_path = character(),
      report_path = character(),
      stringsAsFactors = FALSE
    )
    return(tracker)
  }

  rows <- vector("list", length(run_ids))
  for (i in seq_along(run_ids)) {
    run_id <- run_ids[[i]]
    if (!is.null(current_summary) && identical(current_summary$run_id, run_id)) {
      rows[[i]] <- tracker_row_from_summary(current_summary)
    } else {
      ctx <- resolve_run_context(run_id, project_root)
      rows[[i]] <- tracker_row_from_summary(build_run_summary(ctx, generate_artifacts = FALSE))
    }
  }

  tracker <- do.call(rbind, rows)
  tracker <- tracker[order(vapply(tracker$run_id, extract_run_number, numeric(1)), tracker$run_id), , drop = FALSE]
  tracker$delta_OFV_vs_previous <- c(NA_real_, diff(tracker$OFV))
  rownames(tracker) <- NULL
  tracker
}
