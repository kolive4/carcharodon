yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports",
                   pattern = "^c[1-2]1\\.1[0,1,2]0[3,9]6[0-4].01_12\\.yaml$",
                   full.names = TRUE)

# yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports", 
#                    pattern = "^c[1-2][2-3]\\.000[2,3,4]6[0-4].01_12.yaml$",
#                    full.names = TRUE)

script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports/compiled_report_sharks.R"

charlier::start_logger(filename = file.path("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports/log"))
for (yaml in yamls) {
  cmd = sprintf("Rscript %s --config %s", script, yaml)
  charlier::info("cmd = %s", cmd)
  # cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  charlier::info("Returned value = %i", ok)
  # cat("Script returned: ", ok, "\n")
}


charlier::sendmail("koliveira@bigelow.org", message = "Reports complete.")