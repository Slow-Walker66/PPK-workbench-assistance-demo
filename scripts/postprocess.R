#!/usr/bin/env Rscript

# Entry point for the lightweight NONMEM / PsN reporting workflow.
# Assumption: run this from the project root so relative paths stay simple.

args <- commandArgs(trailingOnly = TRUE)
project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

source(file.path(project_root, "scripts", "model_summary.R"))

target <- if (length(args)) args[[1]] else "."

ensure_dir(file.path(project_root, "output"))
ensure_dir(file.path(project_root, "reports"))

context <- resolve_run_context(target, project_root)
summary <- build_run_summary(context, generate_artifacts = TRUE)
tracker <- build_model_tracker(project_root, current_summary = summary)

tracker_path <- file.path(project_root, "output", "model_tracker.csv")
utils::write.csv(tracker, tracker_path, row.names = FALSE, na = "")

summary$tracker_preview <- tracker
template_path <- file.path(project_root, "reports", "templates", "run_summary_template.html")
report_path <- file.path(project_root, summary$report_path)
render_html_report(summary, template_path, report_path)

message("Post-processing complete")
message(paste("Run:", summary$run_id))
message(paste("Parameters:", summary$parameter_csv))
message(paste("Tracker:", relative_to_root(tracker_path, project_root)))
message(paste("Report:", summary$report_path))
