# Organize and rename crab photos:

file <- data.frame(name = dir(path = "W:/Crab/Offshore Crab Common/Fishing Year 2024/Trawl Survey/photos/crab",
                   full.names = TRUE, include.dirs = FALSE))
file$full <- file$name

file$name <- gsub(" +[Cc]rab +#", " crab #", file$name)
file$name <- gsub(" +", " ", file$name)
file$name <- gsub("[#] +", "# ", file$name)
file <- file[grep(" crab ", file$name), ]

file$name <- unlist(lapply(strsplit(file$name, "/"), function(x) x[length(x)]))


file$tow.id <- unlist(lapply(strsplit(file$name, " "), function(x) x[1]))


for (i in as.character(0:9)) file$name <- gsub(paste0(i, "000") , paste0(i, " 000"), file$name)
file$name <- gsub("110010" , "110 010", file$name)

file$crab.number <- as.numeric(unlist(lapply(strsplit(file$name, " "), function(x) x[4])))
item        <- unlist(lapply(strsplit(file$name, " "), function(x) x[5]))
item        <- gsub("[.]JPG", "", item)
item        <- gsub("couleur", "", item)
item        <- gsub("^0+", "", item)
file$item   <- item

r <- data.frame(tow.id = file$tow.id,
                crab.number = file$crab.number, 
                item = file$item)

tmp <- aggregate(r["item"], by = r[c("tow.id", "crab.number")], unique)
tmp$length <- unlist(lapply(tmp[, 3], length))

crabs <- unique(file[c("tow.id", "crab.number")])
path <- "C:/Users/SuretteTJ/Desktop/github/snow-crab-measurement-project/studies/July 2024 - Snow crab survey/photos/"
for (i in 1:nrow(crabs)){
   print(i)
   ix <- which(file$tow.id == crabs$tow.id[i] & file$crab.number == crabs$crab.number[i])
   
   # Dorsal view:
   to <- paste0(path, paste0("SCS", 2024, "_", file$tow.id[ix[1]], "_crab_",  file$crab.number[ix[1]], "_dorsal.jpg"))
   if (!file.exists(to)) file.copy(file$full[ix[1]], to)
   
   # Ventral view:
   if (length(ix >= 2)){
      for (j in 2:length(ix)){
         to <- paste0(path, paste0("SCS", 2024, "_", file$tow.id[ix[j]], "_crab_",  file$crab.number[ix[j]], "_ventral"))
         if (length(ix) > 2) to <- paste0(to, "_", j-1, ".jpg") else to <- paste0(to, ".jpg")
         if (!file.exists(to))  file.copy(file$full[ix[j]], to)      
      }
   }
}

