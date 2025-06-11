##############################
############# JOINT MODEL, C57

# souris avec au moins une mesure pour chaque variable considérée
c57_mice_model = c57[, any(!is.na(BT)) &
                       any(!is.na(FAT)) &
                       any(!is.na(GLY)) &
                       any(!is.na(P5)),
                     by=mouse][(V1), mouse]
c57_mod = c57[mouse %in% c57_mice_model]


### 
lme_controls = lmeControl(msMaxIter = 1000, msMaxEval = 1000, opt='optim')
# fm1_c57 <- lme(FINKCALs ~ sex+AGE, random = ~ AGE | mouse,
#            data = c57_mod, na.action = na.omit, subset=T)
fm1_c57 <- lme(BT ~ sex + ns(AGE, 3), random = ~ ns(AGE, 3) | mouse_f, 
               data = c57_mod, na.action = na.omit, subset=T,
               control = lme_controls)
fm2_c57 <- lme(FAT ~ sex + ns(AGE, 3), random = ~ ns(AGE, 3) | mouse_f,
               data = c57_mod, na.action = na.omit, subset=T,
               control = lme_controls)
fm3_c57 <- lme(GLY ~ sex + ns(AGE, 3), random = ~ ns(AGE, 3) | mouse_f,
               data = c57_mod, na.action = na.omit, subset=T,
               control = lme_controls)
fm4_c57 <- lme(P5 ~ sex + ns(AGE, 2), random = ~ ns(AGE, 2) | mouse_f,
               data = c57_mod, na.action = na.omit, subset=T,
               control = lme_controls)

fm2_c57_nordm <- lme(FAT ~ sex + ns(AGE, 3), random = ~ 1 | mouse_f,
                     data = c57_mod, na.action = na.omit, subset=T,
                     control = lme_controls)

c57_mod$pred_fm1 <- predict(fm1_c57, c57_mod)
c57_mod$pred_fm2 <- predict(fm2_c57, c57_mod)
c57_mod$pred_fm3 <- predict(fm3_c57, c57_mod)
c57_mod$pred_fm4 <- predict(fm4_c57, c57_mod)

### check predictions

smp1 <- "f_1"
c57_mod[mouse==smp1, 
        plot(AGE, FAT, pch=16, 
             ylim= quantile(c57_mod$FAT, c(0.03, 1), na.rm = T),
             xlim=c(min(c57_mod$AGE), max(c57_mod$AGE)),
             xlab="Age", ylab="B2", col="red")]
c57_mod[mouse==smp1, lines(AGE, pred_fm2, lwd=2, col="red")]

par(mfrow=c(2,2), mar=c(4, 2, 1, 1))
for(i in unique(c57_mod$mouse)){
  c57_mod[mouse==i, 
          plot(AGE, FAT, pch=16, 
               ylim= quantile(c57_mod$FAT, c(0.03, 1), na.rm = T),
               xlim=c(min(c57_mod$AGE), max(c57_mod$AGE)),
               xlab="Age", ylab="B2", col="red")]
  c57_mod[mouse==i, lines(AGE, pred_fm2, lwd=2, col="red")]
  mtext(i, col="red", font = 2)
}



# summary(fm1_c57)
# check estimates of random effects are centered 
head(fm1_c57$coefficients$random$mouse, 5)
colSums(fm1_c57$coefficients$random$mouse)
VarCorr(fm1_c57)
cor(fm1_c57$coefficients$random$mouse)

### Modèle de survie
coxFit_c57 <- coxph(Surv(AGE_DEATH, death) ~ sex,
                    data = unique(c57_mod, by = "mouse_f"), 
                    x = TRUE)


jointFit_c57 <- jm(coxFit_c57,
                   list(fm1_c57, fm2_c57, fm3_c57, fm4_c57), 
                   time_var = "AGE",
                   n_iter = 250000L,
                   n_burnin = 80000L, 
                   functional_forms = list(
                     "BT" = ~ value(BT),
                     "FAT" = ~ value(FAT),
                     "GLY" = ~ value(GLY),
                     "P5" = ~ value(P5)    ) )
save(jointFit_c57, 
     file = "./saved_models/jointFit_c57.RData")
# load("../saved_models/jointFit_c57.RData")
summary(jointFit_c57)



mcmc_alphas <- do.call('rbind', jointFit_c57$mcmc$alphas)

quantile(mcmc_alphas[,"value(GLY)"], 0.01*c(2.5, 97.5))
hist(mcmc_alphas[,"value(GLY)"], main="Posterior sample", xlab="GLY effect")


par(mar=c(4, 4, 0, 0))
age_design <- ns(700:1100, 2)
dim(age_design)
pred_age <- c(-0.15, -0.34) %*% t(age_design)
plot(700:1100, as.numeric(t(pred_age)))

# str(jointFit1$mcmc)
# head(jointFit1$mcmc$betas)
# hist(jointFit1$mcmc$betas[,1])

head(jointFit_c57$mcmc)


