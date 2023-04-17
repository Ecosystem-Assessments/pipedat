# ------------------------------------------------------------------------------
# Update R/sysdata.rda
update_rda <- function() {
  pipeline <- read.csv(file = "inst/extdata/pipeline.csv")
  contact <- read.csv(file = "inst/extdata/contact.csv")
  pcite <- read.csv(file = "inst/extdata/pipeline_citekey.csv")
  pcontact <- read.csv(file = "inst/extdata/pipeline_contact.csv")
  bib <- RefManageR::ReadBib("inst/extdata/pipedat.bib")
  
  usethat::use_data(
    pipeline,
    contact,
    pcite,
    pcontact,
    pcreator,
    bib,
    internal = TRUE,
    overwrite = TRUE
  )
}