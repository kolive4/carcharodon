yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports",
                   pattern = "^c[1-2]1\\.[0-1]0056[0-4].01_12\\.yaml$",
                   full.names = TRUE)

# yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports", 
#                    pattern = "^c[1-2][2-3]\\.000[2,3,4]6[0-4].01_12.yaml$",
#                    full.names = TRUE)

script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports/compiled_report_sharks.R"

for (yaml in yamls) {
  cmd = sprintf("Rscript %s --config %s", script, yaml)
  cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  cat("Script returned: ", ok, "\n")
}

charlier::sendmail("koliveira@bigelow.org", message = "Reports complete.")