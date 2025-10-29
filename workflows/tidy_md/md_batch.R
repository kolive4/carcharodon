yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md",
                   pattern = "^m[1-2]1\\.10116.yaml$",
                   full.names = TRUE)

# yamls = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md", 
#                    pattern = "^m[1-2][2-3]\\.[0-1]00[2,3,4]6.yaml$",
#                    full.names = TRUE)

script = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md/call_md.R"

charlier::start_logger(filename = file.path("/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_md/log"))
for (yaml in yamls) {
  cmd = sprintf("Rscript %s %s", script, yaml)
  charlier::info("cmd = %s", cmd)
  # cat("Running command: ", cmd, "\n")
  ok = system(cmd)
  charlier::info("Returned value = %i", ok)
  # cat("Script returned: ", ok, "\n")
}

charlier::sendmail("koliveira@bigelow.org", message = "Markdowns complete.")
