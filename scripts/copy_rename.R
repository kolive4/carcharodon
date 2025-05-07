library(stringr)

copies = list.files(path = "/mnt/ecocast/projects/koliveira/subprojects/carcharodon/workflows/tidy_reports", 
                    pattern = glob2rx("t81.*.*.yaml"),
                    full.names = TRUE)

file.rename(copies, 
            str_replace(copies, pattern = "t81", "c81"))
