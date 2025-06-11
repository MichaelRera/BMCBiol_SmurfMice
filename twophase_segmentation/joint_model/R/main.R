library(data.table) ; library(JMbayes2)
library(lattice) ; library(splines)

# https://drizopoulos.github.io/JMbayes2/

##################################################################
#                       LOAD & CURATE DATA                       #
##################################################################

c57 <- rbindlist(list(f=readr::read_csv("./data/C57_F_data.csv", na = c("NA")),
                      m=readr::read_csv("./data/C57_M_data.csv", na = c("NA"))),
                 fill = T, idcol = "sex")

akrj <- as.data.table( readr::read_csv("./data/AKRJ_F_data.csv", na = c("NA")) )

c57[, mouse:=paste0(sex, "_", mice)]
akrj[, mouse:=mice]
c57[, mouse_f:=as.factor(mouse)]
akrj[, mouse_f:=as.factor(mouse)]


c57[, mice:=NULL]  # avoid confusions...
akrj[, mice:=NULL]


# create death indicator. Cf. mail July 16, 202x4
c57[, death:=dplyr::case_when(
  mouse%in% c("m_36","m_37","m_38","m_39") ~ 0L,
  TRUE ~ 1L)] 
akrj[, death:=dplyr::case_when(
  mouse%in% c("3", "7", "12", "13", "14") ~ 0L,
  TRUE ~ 1L)]

# # pb mesure juste avant décès. On change à la main la date de dernière 
# # manip : un jour avant le décès
# View(c57[mouse %in% c("f_20", "m_7", "m_17", "m_27"),
#         .(mouse,DOB, date_expe, AGE, REMAINTIME, DOD)])
# lubridate::ymd("2021-09-23")-lubridate::ymd("2019-01-19")

c57[(mouse=="f_20" & date_expe=="2022-08-16")|
      (mouse=="m_7" & date_expe=="2021-09-23")|
      (mouse=="m_17" & date_expe=="2021-07-13")|
      (mouse=="m_27" & date_expe=="2021-08-11"),
    `:=`(date_expe=DOD-1)]
# View(c57[mouse %in% c("f_20", "m_7", "m_17", "m_27"),
#          .(mouse,DOB, date_expe, AGE, REMAINTIME, DOD)])

# recalcule de AGE et REMAINTIME pour éviter toute discrepancy.
# Seule différence : on prend REMAINTIME positif
c57[, AGE:=as.numeric(date_expe-DOB)]
c57[, REMAINTIME:=as.numeric(DOD-date_expe)]
c57[,  `:=`(AGE_DEATH=AGE+REMAINTIME)]
# # histogramme de l'âge à la mort naturelle (non censurée)
# unique(c57[death==1], by="mouse")[death==1, hist(AGE_DEATH/30.25, xlab="lifespan (months)", main="C57")]

akrj[, AGE:=as.numeric(date_expe-DOB)]
akrj[, REMAINTIME:=as.numeric(DOD-date_expe)]
akrj[,  `:=`(AGE_DEATH=AGE+REMAINTIME)]
# histogramme de l'âge à la mort naturelle (non censurée)
unique(akrj[death==1], by="mouse")[, hist(AGE_DEATH/30.25, xlab="lifespan (months)", main="AKRJ")]


akrj[, `:=`( BW=as.numeric(BW), 
             BT=as.numeric(BT), 
             FAT=as.numeric(FAT), 
             GLY=as.numeric(GLY), 
             P3=as.numeric(P3))]

c57[, `:=`(BT=as.numeric(BT), 
           BW=as.numeric(BW), 
           FAT=as.numeric(FAT), 
           GLY=as.numeric(GLY), 
           P5=as.numeric(P5))]

# correction à la main : valeurs données par la machine cassée
akrj[, FAT:=ifelse(FAT<1e-8, NA_real_, FAT)]
c57[, FAT:=ifelse(FAT<1e-8, NA_real_, FAT)]



# test_set <- sample(unique(c57$mouse), size = 10, replace = F)
# c57_test <- c57[mouse%in% test_set]
# c57 <- c57[!mouse %in% test_set]
# test_set <- sample(unique(akrj$mouse), size = 10, replace = F)
# akrj_test <- akrj[mouse%in% test_set]
# akrj <- akrj[!mouse %in% test_set]


##################################################################
#                        DESCRIPTIVE STATS                       #
##################################################################

##### STATS
summary(c57$BT)
c57[, round(mean(BT, na.rm=T), 1), keyby=sex]
summary(c57$GLY)
c57[, round(mean(GLY, na.rm=T), 1), keyby=sex]

c57[,.N, by=mouse][,range(N)]
c57[, N_obs := .N, by=mouse] # number of observations / mouse

setkey(c57, mouse, AGE)

par(mar=c(1, 1, 0, 0))
xyplot(BT ~ AGE | mouse, 
       data = c57,
       subset = mouse%in% sample(unique(c57$mouse), 12),
       xlab = "age", 
       pch=20, cex=0.5)
par(mar=rep(4, 4))

hist(c57$FAT, breaks=20)
hist(akrj$FAT, breaks=20)

par(mar=c(4, 4, 4, 4))
c57[, plot(BW, FAT, col=as.factor(sex), pch=20)]


##################################################################
#                           JOINT MODEL                          #
##################################################################

# ### Base de splines pour l'âge
# matplot(min(c57$AGE):max(c57$AGE),
#         ns(min(c57$AGE):max(c57$AGE), 2), 
#         xlab="Age", ylab="y", type="l")


source("./R/jm_c57.R")
source("./R/jm_akrj.R")
source("./R/dynamic_preds.R")

