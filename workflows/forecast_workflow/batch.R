yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/forecast_workflow", 
           pattern = glob2rx("v02.0104.*.yaml"),
           full.names = TRUE)
  
# Rscript workflows/modeling_workflow/maxent_modeling.R --config workflows/modeling_workflow/v01.2012.yaml
script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/forecast_workflow/forecasting.R"

for (yaml in yamls) {
  cfg = charlier::read_config(yaml)
  model_file = file.path(cfg$root_path, cfg$modeling_vpath, "model.rds")
  if (file.exists(model_file)) {
    cmd = sprintf("Rscript %s --config %s", script, yaml)
    cat("Running command: ", cmd, "\n")
    ok = system(cmd)
    cat("Script returned: ", ok, "\n")
  } else {
    m_ver = cfg$model_version_maj_minor
    cat("Model not found for ", m_ver, "\n")
  }
  
}
