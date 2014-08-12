Sys.setlocale(category = "LC_ALL", locale = "C")
library(foreign)
ibge <- read.dbf('55mu2500psr.dbf')
ibge <- ibge[,c(1,3,4)]
#SC
sc <- read.dbf('42MUE250GC_SIR.dbf')
sc <- data.frame(sc,SIGLA='SC')
sc <- sc[,c(2,4,3)]
#PA
pa <- read.dbf('15MUE250GC_SIR.dbf')
pa <- data.frame(pa,SIGLA='PA')
pa <- pa[,c(2,4,3)]
#MS
ms <- read.dbf('50MUE250GC_SIR.dbf')
ms <- data.frame(ms,SIGLA='MS')
ms <- ms[,c(2,4,3)]
#RS
rs <- read.dbf('43MUE250GC_SIR.dbf')
rs <- data.frame(rs,SIGLA='RS')
rs <- rs[,c(2,4,3)]

names(ibge) <- names(sc)
ibge_uni <- rbind(ibge,sc,pa,ms,rs)

old1 <- "àáâãäåçèéêëìíîïòóôõöùúûü"
old2 <- "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÒÓÔÕÖÙÚÛÜ"
new  <- "aaaaaaceeeeiiiiooooouuuu"

ibge_uni$NM_MUNICIP <- chartr(old1, new, ibge_uni$NM_MUNICIP)
ibge_uni$NM_MUNICIP <- chartr(old2, new, ibge_uni$NM_MUNICIP)
ibge_uni$NM_MUNICIP <- tolower(ibge_uni$NM_MUNICIP)

nchar(old2)
nchar(new)
old1

repetidos <- ibge_uni$CD_GEOCODM[duplicated(ibge_uni$CD_GEOCODM)]
View(ibge_uni[ ibge_uni$CD_GEOCODM %in% repetidos, ])



tmp <- iconv(ibge_uni$NM_MUNICIP, from='WINDOWS-1252', to="ASCII//TRANSLIT")
View(tmp)
tmp <- enc2native(ibge_uni$NM_MUNICIP)
tmp <- encodeString(tmp)
tmp <- iconv(tmp, to="ASCII//TRANSLIT")
tmp <- enc2native(tmp)

tmp <- tolower(tmp)
sum(duplicated(tmp))
intToUtf8(tmp)

ibge_uni$NM_MUNICIP <- tmp

View(tmp[tmp %in% tmp[duplicated(tmp)]])
tmp <-gsub("[^[:alpha:]]", "", ibge_uni$NM_MUNICIP)


ibge_uni <- unique(ibge_uni)
ibge_uni <- ibge_uni[order(ibge_uni$CD_GEOCODM),]
length(unique(ibge_uni$CD_GEOCODM))
repetidos <- ibge_uni$CD_GEOCODM[duplicated(ibge_uni$CD_GEOCODM)]
View(ibge_uni[ ibge_uni$CD_GEOCODM %in% repetidos, ])
