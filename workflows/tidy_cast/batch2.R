yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_cast", 
                   pattern = "^t[1-2][2-3]\\.00046[0-4]\\.\\d+\\.yaml$",
                   #pattern = glob2rx("t[1-2][2-3].0003[0-5][0-4].*.yaml"),
                   full.names = TRUE)

# Rscript workflows/modeling_workflow/maxent_modeling.R --config workflows/modeling_workflow/v01.2012.yaml
script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_cast/tidy_seal_cast.R"

for (yaml in yamls) {
  cmd = sprintf("Rscript %s --config %s", script, yaml)
  cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  cat("Script returned: ", ok, "\n")
}