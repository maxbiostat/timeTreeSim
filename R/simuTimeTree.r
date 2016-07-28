### The idea is to provide an R wrapper to use BEAST for time-tree simulation
### assumes both beast and beastgen are installed in the system, possibly at /usr/bin
simuTimeTree <- function(
  coalmodel = c("constant", "exponential", "logistic"),
  simu_name,
  taxa_times,
  popSize = NULL,
  growthRate = NULL,
  T50 = NULL,
  SEED = NULL,
  verbose = FALSE
){
  coalmodel <- match.arg(coalmodel)
  ## begin checks
  if(is.null(popSize)) stop("Please specify a population size")
  if(any(coalmodel == "exponential", coalmodel == "logistic") && is.null(growthRate)) stop("Please specify a growth rate")
  if(coalmodel == "logistic" && is.null(T50)) stop("Please specify a t50")
  ## end checks
  if(is.null(growthRate)) growthRate <- 0 ## mock parameters
  if(is.null(T50))T50 <- 0
  require(ape)
  N <- length(taxa_times)
  emptyAln <- matrix("?", nrow = N)
  rownames(emptyAln) <- paste("seq_", taxa_times, sep = "")
  fastaName <- tempfile(simu_name, fileext = ".fasta")
  XMLName <- tempfile(simu_name, fileext = ".xml")
  ape::write.dna(emptyAln, file = fastaName , format = "fasta")
  template <- switch (coalmodel,
                      constant = system.file("extdata", "constant_pop.template", package = "timeTreeSim"),
                      exponential =  system.file("extdata", "exponential_growth.template", package = "timeTreeSim"),
                      logistic = system.file("extdata", "logistic_growth.template", package = "timeTreeSim")
  )
  ## begin hack
  fastaName <- sub("/tmp/", "", fastaName)
  XMLName.short <- sub("/tmp/", "", XMLName)
  tempTemplate <- tempfile("BGenTemplate", fileext = ".template")
  system(paste("cat", template, ">>", tempTemplate))
  template <- sub("/tmp/", "", tempTemplate)
  oriD <- getwd()
  setwd("/tmp/")
  ## end hack
  CmdString <- sprintf(
    "beastgen -D \"pop_size=%f,growth_rate=%f,t50=%f\" -date_order -1 -date_prefix \\_",
    popSize, growthRate, T50
  )
  genCommand <- paste(CmdString, template, fastaName, XMLName.short)
  system(command = genCommand, ignore.stdout = !verbose)
  if(is.null(SEED)){
    runCommand <- paste("beast -overwrite", XMLName)
  }else{
    runCommand <- paste("beast -overwrite -seed", SEED,  XMLName)
  }
  system(runCommand, ignore.stdout = !verbose)
  res <- ape::read.nexus(sub(".xml", ".tree", XMLName))
  setwd(oriD)
  return(res)
}
