
###############################
############# JOINT MODEL, AKRJ

akrj_mice_model = akrj[, any(!is.na(BT)) &
                         any(!is.na(FAT)) &
                         any(!is.na(GLY)) &
                         any(!is.na(P3)),
                       by=mouse][(V1), mouse]
akrj_mod = akrj[mouse %in% akrj_mice_model]

### 
lme_controls = lmeControl(msMaxIter = 100, msMaxEval = 1000, 
                          msVerbose = TRUE, opt="optim")
fm1_akrj <- lme(BT ~ ns(AGE, 3), random = ~ ns(AGE, 2) | mouse, 
                data = akrj_mod, na.action = na.omit,
                control = lme_controls)
fm2_akrj <- lme(FAT ~ ns(AGE, knots = c(175, 250, 325, 400)), # c(200, 250, 300, 400)
                random = ~ ns(AGE, knots = c(175, 250, 325, 400)) | mouse,
                data = akrj_mod, na.action = na.omit,
                control = lme_controls)
fm3_akrj <- lme(GLY ~ ns(AGE, 3), random = ~ ns(AGE, 3) | mouse,
                data = akrj_mod, na.action = na.omit,
                control = lme_controls)
fm4_akrj <- lme(P3 ~ ns(AGE, 3), random = ~ ns(AGE, 3) | mouse,
                data = akrj_mod, na.action = na.omit,
                control = lme_controls)
akrj_mod$pred_fm1 <- predict(fm1_akrj, akrj_mod)
akrj_mod$pred_fm2 <- predict(fm2_akrj, akrj_mod)
akrj_mod$pred_fm3 <- predict(fm3_akrj, akrj_mod)
akrj_mod$pred_fm4 <- predict(fm4_akrj, akrj_mod)

# check predictions
smp_tst1 <- sample(unique(akrj_mod$mouse), 1)
akrj_mod[mouse==smp_tst1, 
         plot(AGE, FAT, pch=16, ylab="F",
              ylim= range(akrj_mod$FAT, na.rm = T),
              main=smp_tst1)]
akrj_mod[mouse==smp_tst1, lines(AGE, pred_fm2, lwd=2)]


coxFit_akrj <- coxph(Surv(AGE_DEATH, death) ~ 1,
                     data = unique(akrj_mod, by = "mouse"), 
                     x = TRUE)

fForms_akrj <- list(
  "BT" = ~ value(BT),
  "FAT" = ~ slope(FAT),
  "GLY" = ~ value(GLY),
  "P3" = ~ value(P3)    )

jointFit_akrj <- jm(coxFit_akrj, list(fm1_akrj, fm2_akrj, fm3_akrj, fm4_akrj), time_var = "AGE",
                    n_iter = 100000, # 450000L,
                    n_burnin = 25000, # 150000L, 
                    functional_forms = fForms_akrj)

save(jointFit_akrj, file = "./saved_models/jm_akrj.RData")
load("./saved_models/jm_akrj.RData")

# traceplot(jointFit_akrj)
summary(jointFit_akrj)


######################
##### TRY WITHOUT GLY

fForms_akrj_nogly <- list(
  "BT" = ~ value(BT),
  "FAT" = ~ slope(FAT),
  # "GLY" = ~ value(GLY),
  "P3" = ~ value(P3)    )
jointFit_akrj_nogly <- jm(coxFit_akrj, list(fm1_akrj, fm2_akrj, fm4_akrj), time_var = "AGE",
                          n_iter = 450000L,
                          n_burnin = 150000L, 
                          functional_forms = fForms_akrj_nogly)

save(jointFit_akrj_nogly, file = "./saved_models/jm_akrj_nogly.RData")
load("./saved_models/jm_akrj_nogly.RData")
summary(jointFit_akrj_nogly)


fForms_akrj_nop3 <- list(
  "BT" = ~ value(BT),
  "FAT" = ~ slope(FAT),
  "GLY" = ~ value(GLY)#,
  # "P3" = ~ value(P3) 
)
jointFit_akrj_nop3 <- jm(coxFit_akrj, list(fm1_akrj, fm2_akrj, fm3_akrj), time_var = "AGE",
                         n_iter = 450000L,
                         n_burnin = 150000L, 
                         functional_forms = fForms_akrj_nop3)

save(jointFit_akrj_nop3, file = "./saved_models/jointFit_akrj_nop3.RData")
load("./saved_models/jointFit_akrj_nop3.RData")
summary(jointFit_akrj_nop3)



#########################################
############# JOINT MODEL, AKRJ, LOG(FAT)

akrj_mod$logFAT <- log(akrj_mod$FAT)

### 
lme_controls = lmeControl(msMaxIter = 100, msMaxEval = 1000, 
                          msVerbose = TRUE, opt="optim")
fm1_akrj2 <- lme(BT ~ ns(AGE, 3), random = ~ ns(AGE, 2) | mouse, 
                 data = akrj_mod, na.action = na.omit,
                 control = lme_controls)
fm2_akrj2 <- lme(logFAT ~ ns(AGE, knots = c(175, 250, 325, 400)), # c(200, 250, 300, 400)
                 random = ~ ns(AGE, knots = c(175, 250, 325, 400)) | mouse,
                 data = akrj_mod, na.action = na.omit,
                 control = lme_controls)
fm3_akrj2 <- lme(GLY ~ ns(AGE, 3), random = ~ ns(AGE, 3) | mouse,
                 data = akrj_mod, na.action = na.omit,
                 control = lme_controls)
fm4_akrj2 <- lme(P3 ~ ns(AGE, 3), random = ~ ns(AGE, 3) | mouse,
                 data = akrj_mod, na.action = na.omit,
                 control = lme_controls)
akrj_mod$pred2_fm1 <- predict(fm1_akrj2, akrj_mod)
akrj_mod$pred2_fm2 <- predict(fm2_akrj2, akrj_mod)
akrj_mod$pred2_fm3 <- predict(fm3_akrj2, akrj_mod)
akrj_mod$pred2_fm4 <- predict(fm4_akrj2, akrj_mod)

# check predictions
smp_tst1 <- sample(unique(akrj_mod$mouse), 1)
akrj_mod[mouse==smp_tst1, 
         plot(AGE, logFAT, pch=16, ylab="F",
              ylim= range(akrj_mod$logFAT, na.rm = T),
              main=smp_tst1)]
akrj_mod[mouse==smp_tst1, lines(AGE, pred2_fm2, lwd=2)]


coxFit_akrj2 <- coxph(Surv(AGE_DEATH, death) ~ 1,
                      data = unique(akrj_mod, by = "mouse"), 
                      x = TRUE)

fForms_akrj2 <- list(
  "BT" = ~ value(BT),
  "logFAT" = ~ slope(logFAT),
  "GLY" = ~ value(GLY),
  "P3" = ~ value(P3)    )

jointFit_akrj2 <- jm(coxFit_akrj2, list(fm1_akrj2, fm2_akrj2, fm3_akrj2, fm4_akrj2), time_var = "AGE",
                     n_iter = 100000, # 450000L,
                     n_burnin = 25000, # 150000L, 
                     functional_forms = fForms_akrj2)

save(jointFit_akrj2, file = "./saved_models/jm_akrj2.RData")
load("./saved_models/jm_akrj2.RData")

# traceplot(jointFit_akrj)
summary(jointFit_akrj2)


