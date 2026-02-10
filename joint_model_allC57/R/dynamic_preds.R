##################################################################
#                       DYNAMIC PREDICTIONS                      #
##################################################################

# https://github.com/drizopoulos/JMbayes2/blob/master/vignettes/Dynamic_Predictions.Rmd

t0=1000

alive_t0 = c57[AGE_DEATH>t0 & sex=="f", (unique(mouse))]

c57_t0 = c57[AGE<t0 & mouse %in% alive_t0]

# # pour choix des souris : une très mal à t0, une qui a l'air bien
par(mar=c(1, 1, 0, 0))
xyplot(BT ~ AGE | mouse,
       data = c57_t0,
       xlab = "age",
       pch=20, cex=0.5)
par(mar=rep(4, 4))

# "m_1" ajouté car sinon erreur contraste !!!!!
# ND <- c57[mouse %in% c("f_1", "f_20", "f_14", "f_40", "f_42", "m_1") & AGE < t0]
selected_nd <- c("f_1", "f_14",  "m_1")
ND <- c57[mouse %in% selected_nd & AGE < t0]
unique(c57[mouse %in% selected_nd, .(mouse, AGE_DEATH)])

# On censure artificiellement
ND$death <- 0
ND$AGE_DEATH <- t0

predLong <- predict(jointFit_c57, 
                    newdata = ND,
                    times = seq(t0, t0+9*30, length.out = 100),
                    return_newdata = TRUE)
predSurv <- predict(jointFit_c57, 
                    newdata = ND, process = "event",
                    times = seq(t0, t0+9*30, length.out = 100),
                    return_newdata = TRUE)
plot(predLong, outcomes = 1, subject = "f_14")
plot(predLong, predSurv, outcomes = 1:3, subject = "f_1" )

# attr(predLong, "ranges")[[1]] <- c(35, 38)

# fm1 -> BT
# fm2 -> FAT
# fm3 -> GLY 
# fm4 <- P5

plot_mouse = function(mouse_id){
        
        cols <- c('#F28322', '#D973B5', '#4A90E2')
        
        plot(predLong, predSurv, outcomes = c(1, 3, 4),
             subject = mouse_id,
             fun_long = list(identity, identity, identity),
             fun_event = function (x) 1 - x,
             ylab_event = "Survival Probability",
             # ylab_long = c("BT",  "GLY", "P5"),
              ylab_long = c("",  "", ""),
             col_points = cols, 
             col_line_long = cols,
             # col_line_event =   #F7F7FF',
             # col_axis = "white", 
             fill_CI_long = paste0(cols, "80"),
             # fill_CI_event = "#F7F7FF80",
             pos_ylab_long = c(12, 0.5, 4)
             , ylim_long_outcome_range=F
             ) 
}

pdf("../figs/pred_f1.pdf", width = 9, height=5)
plot_mouse("f_1")
dev.off()

pdf("../figs/pred_f14.pdf", width = 9, height=5)
plot_mouse("f_14")
dev.off()


########## Pour reviewer

alive_t0 = c57[AGE_DEATH>t0, (unique(mouse))]

ND <- c57[mouse %in% alive_t0 & AGE < t0]
# unique(c57[mouse %in% selected_nd, .(mouse, AGE_DEATH)])

# On censure artificiellement
ND$death <- 0
ND$AGE_DEATH <- t0
predSurv <- predict(jointFit_c57, 
                    newdata = ND, process = "event",
                    times = seq(t0, t0+9*30, length.out = 500),
                    return_newdata = TRUE)

predSurv_sht <- predSurv[, AGE_DEATH:upp_CIF]
t50_pred <- predSurv[, .SD[which.min(abs(pred_CIF - 0.5))], by = mouse]
setnames(t50_pred, "AGE_DEATH", "AGE_T50")
t50_pred[c57, on=.(mouse), AGE_DEATH:=AGE_DEATH]

t50_pred[, plot(AGE_DEATH, AGE_T50)]
t50_pred[, summary(lm(AGE_DEATH~AGE_T50))]

