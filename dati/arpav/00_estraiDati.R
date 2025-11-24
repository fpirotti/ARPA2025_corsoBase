library(stringr)
## data mining ---
csvs<-list.files("dati/arpav/", pattern = ".csv$", full.names = T)
n <- length(csvs)
df <- data.frame(stazione=character(n),
                 GBEST=rep(NA_real_, n),
                 GBNORD=rep(NA_real_, n) )
for(m in month.abb){
  df <- cbind(df, m=rep(NA_real_, n))
}
names(df) <- c("stazione", "GBEST", "GBNORD", month.abb)

DFreadit <- function(rown){
  file <- csvs[[rown]]
  fi <- read.csv(file,sep = ";")
  stz <- grep("Stazione ", fi[,1],ignore.case = T, fixed = T)
  stz <- gsub("Stazione ", "", fi[ stz[[1]] ,1])
  if(is.na(stz) || trimws(stz)==""){
    browser()
  }

  coor <- grep("Coordinata ", fi[,1],ignore.case = T, fixed = T)
  if(length(coor)<2){
    message(stz, " no coordinates, skipping...")
    # browser()
    return(NA)
  }
  x <- gsub("Coordinata ", "", fi[ coor[[1]] ,1])
  coorx <- str_extract(x, "[0-9]+")
  y <- gsub("Coordinata ", "", fi[ coor[[2]] ,1])
  coory <- str_extract(y, "[0-9]+")
  if(is.na(coory) || trimws(coory)==""){
    browser()
  }
  vv <- grep("Medio mensile", fi[,1], ignore.case = T, fixed = T)
  df[ rown, 1:3 ] <<- c(stz,  coorx, coory)
  df[ rown, (1:3)*-1 ] <<- as.numeric(fi[vv[[1]],2:13])
}

for(rown in 1:length(csvs)){
  message(rown)
  suppressWarnings(DFreadit(rown))
}

dati.arpav <- na.omit(df)
dati.arpav$tot.precipitation.mm <-  rowSums(dati.arpav[,c(-1,-2,-3)])
write.csv(dati.arpav, "dati/venetoDatiMeteoV2.csv")
