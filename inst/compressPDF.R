library(tools)
pdf_files <- list.files(system.file("doc", package = "mypackage"), pattern = "\\.pdf$", full.names = TRUE)
for (file in pdf_files) {
  compactPDF(file, gs_quality = "ebook")
}
