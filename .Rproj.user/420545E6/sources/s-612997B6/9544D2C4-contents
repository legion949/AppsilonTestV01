


data_set <- read.csv(file = "002_data/ships.csv", stringsAsFactors = F)

dt_amandaV2 <- data_set$"SHIP_ID" == 323355
if (sum(dt_amandaV2) > 0) data_set$"SHIPNAME"[dt_amandaV2] <- "AMANDA V2"

#############################################################################
my_ship_type <- "Cargo"

data <- filter(data_set, ship_type == my_ship_type)


juntos <- unique(paste0(data$"SHIPNAME", "z8z",   data$"SHIP_ID"))
table(juntos)
metralla <- unlist(strsplit(juntos, "z8z"))[c(T,F)]
tabla <- table(metralla)
dt_este <- tabla == 2
names(tabla)[dt_este]
#############################################################################

"AMANDA"
# There 2 "AMANDA"!!!!!!!!!!
data <- filter(data_set, SHIPNAME == "AMANDA")
my_id <- unique(data$SHIP_ID)
my_id

#########################################################################

# But only 1 AMANDA have id "323355"
data <- filter(data_set, SHIP_ID == 323355)
my_name <- unique(data$SHIPNAME)
my_name




############################################################################
#############################################################################
my_ship_type <- "Cargo"

data <- filter(data_set, ship_type == my_ship_type)


juntos <- unique(paste0(data$"SHIPNAME", "z8z",   data$"SHIP_ID"))
table(juntos)
metralla <- unlist(strsplit(juntos, "z8z"))[c(T,F)]
tabla <- table(metralla)
dt_este <- tabla == 2
names(tabla)[dt_este]
#############################################################################
