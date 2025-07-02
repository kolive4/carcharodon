yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md", 
                   pattern = "^m[1-2]1\\.\\d{5}\\.yaml$",
                   full.names = TRUE)

script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md/call_md.R"

for (yaml in yamls) {
  cmd = sprintf("Rscript %s %s", script, yaml)
  cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  cat("Script returned: ", ok, "\n")
}
