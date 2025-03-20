yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_workflow", 
           pattern = glob2rx("t9*.00020.w.yaml"),
           full.names = TRUE)
  
# Rscript workflows/modeling_workflow/maxent_modeling.R --config workflows/modeling_workflow/v01.2012.yaml
script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_workflow/tidy_seals.R"

for (yaml in yamls) {
  cmd = sprintf("Rscript %s --config %s", script, yaml)
  cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  cat("Script returned: ", ok, "\n")
}