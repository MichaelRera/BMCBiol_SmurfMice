
akrj[!is.na(FAT), MAX_AGE_OBS := max(AGE), by = mouse]

# check
akrj[mouse==13, .(AGE, AGE_DEATH, FAT, MAX_AGE_OBS)]


akrj_m0=akrj[AGE==MAX_AGE_OBS]

akrj_temp=akrj[AGE!=MAX_AGE_OBS]

akrj_temp[!is.na(FAT), MAX_AGE_OBS := max(AGE), by = mouse]

akrj_m1 <- akrj_temp[AGE==MAX_AGE_OBS]

akrj_m0[, .(mouse, AGE, FAT)]
akrj_m1[, .(mouse, AGE, FAT)]

akrjm0m1 <- merge(akrj_m0, akrj_m1, by = "mouse", all = TRUE, suffixes = c("_m0", "_m1"))
akrjm0m1[,hist(FAT_m1-FAT_m0, breaks=20)]
akrjm0m1[, mean(FAT_m1-FAT_m0>5, na.rm=T)]

