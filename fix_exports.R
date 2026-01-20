# Fix S3 exports - replace @exportS3Method with @export
content <- readLines("R/04-S3-Methods.R")
content <- gsub("#' @exportS3Method [a-z.]+ [a-z_]+", "#' @export", content)
writeLines(content, "R/04-S3-Methods.R")
cat("Done replacing in 04-S3-Methods.R\n")

content2 <- readLines("R/05-causal.R")
content2 <- gsub("#' @exportS3Method [a-z.]+ [a-z_]+", "#' @export", content2)
writeLines(content2, "R/05-causal.R")
cat("Done replacing in 05-causal.R\n")
