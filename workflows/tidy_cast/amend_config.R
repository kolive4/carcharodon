library(yaml)

files <- list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_cast", 
                    pattern = glob2rx("t1*.000**.*.yaml"),
                    full.names = TRUE)

for (file in files) {
  cfg <- yaml.load_file(file)
  
  if (!is.null(cfg$graphics$ggtitle)) {
    cfg$graphics$ggtitle <- "Habitat Suitability Index"
    write_yaml(cfg, file)
  }
}
