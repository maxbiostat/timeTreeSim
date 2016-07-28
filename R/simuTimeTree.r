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
  fastaName <- paste(simu_name, ".fasta", sep = "")
  ape::write.dna(emptyAln, file = fastaName , format = "fasta")
  template <- switch (coalmodel,
                      constant = paste("constant_pop.template"),
                      exponential = paste("exponential_growth.template"),
                      logistic = paste("logistic_growth.template")
  )
  CmdString <- sprintf(
    "beastgen -D \"pop_size=%f,growth_rate=%f,t50=%f\" -date_order -1 -date_prefix \\_",
    popSize, growthRate, T50
  )
  genCommand <- paste(CmdString, template, fastaName, sub(".fasta", ".xml", fastaName) )
  system(command = genCommand, ignore.stdout = !verbose)
  if(is.null(SEED)){
    runCommand <- paste("beast -overwrite", sub(".fasta", ".xml", fastaName))
  }else{
    runCommand <- paste("beast -overwrite -seed", SEED,  sub(".fasta", ".xml", fastaName))
  }
  system(runCommand, ignore.stdout = !verbose)
  res <- ape::read.nexus(paste(simu_name,".tree", sep = ""))
  return(res)
}
