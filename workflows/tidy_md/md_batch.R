# yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md", 
#                    pattern = "^m[1-2]1\\.[0-1]00[3,5,6,7][0,1,2,6].yaml$",
#                    full.names = TRUE)

yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md", 
                   pattern = "^m[1-2][2-3]\\.[0-1]00[2,3,4]6.yaml$",
                   full.names = TRUE)

script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md/call_md.R"

for (yaml in yamls) {
  cmd = sprintf("Rscript %s %s", script, yaml)
  cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  cat("Script returned: ", ok, "\n")
}

charlier::sendmail("koliveira@bigelow.org")
