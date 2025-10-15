yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_cast", 
           pattern = "^t[1-2]1\\.1[0,1,2]0[3,9]6[0-4]\\.\\d+\\.yaml$",
           #pattern = glob2rx("t[1-2][2-3].0003[0-5][0-4].*.yaml"),
           full.names = TRUE)
  
# Rscript workflows/modeling_workflow/maxent_modeling.R --config workflows/modeling_workflow/v01.2012.yaml
script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_cast/tidy_ccar_cast.R"

charlier::start_logger(filename = file.path("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_cast/log"))
for (yaml in yamls) {
  cmd = sprintf("Rscript %s --config %s", script, yaml)
  charlier::info("cmd = %s", cmd)
  # cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  charlier::info("Returned value = %i", ok)
  # cat("Script returned: ", ok, "\n")
}

charlier::sendmail("koliveira@bigelow.org", message = "Casts complete.")