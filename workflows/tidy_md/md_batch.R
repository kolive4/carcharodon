yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md", 
                   pattern = "m22.00046.yaml",
                     #"^m[1-2][1-3]\\.\\d{5}\\.yaml$",
                   full.names = TRUE)

script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md/call_md.R"

for (yaml in yamls) {
  cmd = sprintf("Rscript %s %s", script, yaml)
  cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  cat("Script returned: ", ok, "\n")
}
