# We've rerun the winter models under the versions t92 for gray and t93 for harbor as one-off version runs for winter models of seals. Rather than attempting to rework our functionality to output a winter model, we use this script to copy the outputs of t92 and t93 into t02 and t03 under the winter months (01, 02, and 03). All of the files in the t02.00030.01,02,12 and the t03.00030.01,02,12 versions correspond to the t92 and t93 runs.

root_path = "/mnt/s1/projects/ecocast/projects/koliveira/subprojects/carcharodon"
i_vers = "workflows/tidy_workflow/versions/t92/00020/t92.00020.w"
t_ver = "t92"
# find all the files in t92.00030.w and t93.00030 and return their names
ff = list.files(i_vers, full.names = TRUE)
o_vers = "workflows/tidy_workflow/versions/t02/00020/t02.00020.w"
o_ver = "t02"

# for each of .01, .02, and .12 
for (month in c(".01", ".02", ".12")) {
  # substitute .w with the month string in the output version path
  outpath = sub(".w", month, o_vers, fixed = TRUE)
  # create a list of files that substitute .w with the month string in all of the files in the t92/t93 path
  off = gsub(".w", month, ff, fixed = TRUE)
  # create a list of files that substitute the 92/93 version with the output version in all of the files in the t92/t93 path
  off = gsub(t_ver, o_ver, off, fixed = TRUE)
  # create the new t02.*.01,02,12/t03.*.01,02,12 paths
  dir.create(outpath, recursive = TRUE, showWarnings = FALSE)
  # copy all the files into the new paths
  file.copy(from = ff, to = off, recursive = FALSE)
}
