docker <- readLines("Dockerfile")
to_update <- grep("^    .*=", docker)
packages <- gsub(" *(.*)=.*", "apt-cache policy \\1", docker[to_update])
version <- sapply(packages, function(x) {
  output <- system(x, intern = TRUE)
  gsub(".*: *(.*)", "\\1", output[grepl("(Kandidaat|Candidate)", output)])
})
docker[to_update] <- sprintf(
  "%s%s%s", gsub("(.*=)(.*)", "\\1", docker[to_update]), version,
  ifelse(grepl("\\\\$", docker[to_update]), " \\", "")
)
cat(docker, sep = "\n")
