files = list.files(path = "workflows/modeling_workflow", pattern = glob2rx("v01.**0.*.yaml"), full.names = TRUE)

cfgs = lapply(files, function(file){
  cfg = charlier::read_config(file)
  cfg$obs_filter$basisOfRecord = c(cfg$obs_filter$basisOfRecord, "iNaturalist")
  charlier::write_config(cfg, file)
  return(cfg)
})
