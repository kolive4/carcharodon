yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_workflow", 
           pattern = "^t1[2-3]\\.000[2,3,4]6\\.\\d+\\.yaml$",
           full.names = TRUE)
  
# Rscript workflows/modeling_workflow/maxent_modeling.R --config workflows/modeling_workflow/v01.2012.yaml
script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_workflow/tidy_seals.R"

for (yaml in yamls) {
  cmd = sprintf("Rscript %s --config %s", script, yaml)
  cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  cat("Script returned: ", ok, "\n")
}

charlier::sendmail("koliveira@bigelow.org")