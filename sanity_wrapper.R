sanity = function (countmatrix) {
  input = round(countmatrix) # Sanity only accepts integers
  input = cbind(
    data.frame(GeneID = row.names(input)),
    input)
  tempdir = tempfile()
  mkdirs(tempdir)
  write.table(
    input,
    paste0(tempdir, "/input.txt"),
    quote = FALSE,
    sep = "\t",
    row.names = FALSE)
  sys::exec_wait(
    cmd = "~/Downloads/Sanity/bin/Sanity",
    args = c("-f",
             paste0(tempdir, "/input.txt"),
             "-d",
             tempdir,
             "-n",
             "1"))
  output = read.table(
    paste0(tempdir, "/log_transcription_quotients.txt"),
    sep = "\t",
    header = TRUE)
  row.names(output) = output$GeneID
  output = output[, -1]
  output = as.matrix(output)
  output = exp(output)
  output =
    output *
    matrix(median(colSums(countmatrix)) / colSums(output),
           nrow = nrow(output),
           ncol = ncol(output),
           byrow = TRUE)
  colnames(output) = colnames(countmatrix)
  rownames(output) = rownames(countmatrix)
  unlink(tempdir, recursive = TRUE)
  return(output)
}