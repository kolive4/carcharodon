yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports", 
                   pattern = "^c[1-2]1\\.[0-1]005[0-5][0-4].01_12\\.yaml$",
                   #pattern = glob2rx("c[1-2]*.0002*.01_12.yaml"),
                   full.names = TRUE)

# Rscript workflows/modeling_workflow/maxent_modeling.R --config workflows/modeling_workflow/v01.2012.yaml
script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports/compiled_report_sharks.R"

for (yaml in yamls) {
  cmd = sprintf("Rscript %s --config %s", script, yaml)
  cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  cat("Script returned: ", ok, "\n")
}
