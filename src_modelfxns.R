# Details ----
  #' 01_src_modelfxns.R
  #' Paper: [Fine-tuning established morphometric models...]
  #' DOI: 10.1111/csp2.13308
  #' Author: Nikki Biskis
  #' Date: 2025-11-11
  #' Purpose: Set functions of published models for later call
  #' Output: Functions to be used in 01_draw_pub_models.R
  #' 
  #' *Changes from original: N/A 
  #' - just separated for clarity
#' -----------

#Anoxy----

AFariaL <- function(SRL){
  TL <- SRL*4.8904-75.012
  return(TL)
}
AFariaP <- function(SRL){
  TL <- 2.5626*SRL^1.1052
  return(TL)
}
APev <- function(SRL) {
  TL <- (5.2961*SRL)-72.683
  return(TL)
}
AWhittyL <- function(SRL){
  TL <- SRL/0.2
  return(TL)
}
AWhittyQ <- function(SRL){
  TL <- (0.1795+sqrt(0.00036*SRL-0.071208))/0.00018
  length(TL) == length(SRL)
  return(TL)
}

#Clavata----

CFariaL <- function(SRL){
  TL <- SRL*5.8895-170.44
  return(TL)
}
CFariaP <- function(SRL){
  TL <- 1.6671*SRL^1.2085
  return(TL)
}
CPev <- function(SRL) {
  TL <- (8.16*SRL)-437.5
  return(TL)
}
CThor <- function(SRL){
  TL <- 1.8173*(SRL^1.1799)
  return(TL)
}
CWhittyL <- function(SRL){
  TL <- SRL/0.19
  return(TL)
}
CWhittyP <- function(SRL){
  TL <- 2.213*(SRL^1.157)
  return(TL)
}

#Pristis----

PFariaL <- function(SRL){
  TL <- SRL*5.460-289.37
  return(TL)
}
PFariaP <- function(SRL){
  TL <- 0.9913*SRL^1.2526
  return(TL)
}
PMorg <- function(SRL){
  TL <- 5.2579*SRL - 180.708
  return(TL)
}
PPev <- function(SRL) {
  TL <- (4.1598*SRL)+121.49
  return(TL)
}
PThorF <- function(SRL){
  TL <- 2.5304*(SRL^1.0973)
  return(TL)
}
PThorM <- function(SRL){
  TL <- 1.281*(SRL^1.2107)
  return(TL)
}
PWhittyL <- function(SRL){
  TL <- SRL/0.23
  return(TL)
}
PWhittyP <- function(SRL){
  TL <- 1.943*(SRL^1.143)
  return(TL)
}

#Zijsron----

ZFariaL <- function(SRL){
  TL <- SRL*3.7666+166.15
  return(TL)
}
ZFariaP <- function(SRL){
  TL <- 5.0242*SRL^0.9704
  return(TL)
}
ZLearF <- function(SRL){
  TL <- 2.71828^(1.02*log(SRL)+1.29)
  return(TL)
}
ZLearM <- function(SRL){
  TL <- 2.71828^(1.05*log(SRL)+1.08)
  return(TL)
}
ZPev <- function(SRL) {
  TL <- (3.703*SRL)+225.536
  return(TL)
}
ZWhittyL <- function(SRL){
  TL <- SRL/0.24
  return(TL)
}
ZWhittyP <- function(SRL){
  TL <- 9.3627*(SRL^0.8559)
  return(TL)
}
