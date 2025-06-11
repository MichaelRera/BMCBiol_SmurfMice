##################################################################
#                       DYNAMIC PREDICTIONS                      #
##################################################################

# https://github.com/drizopoulos/JMbayes2/blob/master/vignettes/Dynamic_Predictions.Rmd

t0=1000

alive_t0 = c57[AGE_DEATH>t0 & sex=="f", (unique(mouse))]

c57_t0 = c57[AGE<t0 & mouse %in% alive_t0]

# # pour choix des souris : une très mal à t0, une qui a l'air bien
# par(mar=c(1, 1, 0, 0))
# xyplot(FAT ~ AGE | mouse,
#        data = c57_t0,
#        xlab = "age",
#        pch=20, cex=0.5)
# par(mar=rep(4, 4))

# "m_1" ajouté car sinon erreur contraste !!!!!
ND <- c57[mouse %in% c("f_1", "f_14", "m_1") & AGE < t0]

# On censure artificiellement
ND$death <- 0
ND$AGE_DEATH <- t0

predLong <- predict(jointFit_c57, 
                    newdata = ND,
                    times = seq(t0, t0+12*30, length.out = 100),
                    return_newdata = TRUE)
predSurv <- predict(jointFit_c57, 
                    newdata = ND, process = "event",
                    times = seq(t0, t0+12*30, length.out = 100),
                    return_newdata = TRUE)
plot(predLong, ylab_long = "B2", outcomes = 2, subject = "f_1")
plot(predLong, predSurv, outcomes = 1:3, subject = "f_1" )

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
             ylab_long = c("Body Temperature",  "Glycemia", "5h Permeability"),
             
        
             col_points = cols, 
             col_line_long = cols,
             # col_line_event =   #F7F7FF',
             # col_axis = "white", 
             fill_CI_long = paste0(cols, "80"),
             # fill_CI_event = "#F7F7FF80",
             pos_ylab_long = c(12, 0.5, 4)
             )
        abline(v=c57[mouse==mouse_id, AGE_DEATH], lty=2, col="red")
        
        text(x = c57[mouse==mouse_id, AGE_DEATH], 
             y = 0.8,
             labels="Actual\ndeath",
             col="red", pos=4)
}

pdf("pred_f1.pdf", width = 9, height=5)
plot_mouse("f_1")
dev.off()

pdf("pred_f14.pdf", width = 9, height=5)
plot_mouse("f_14")
dev.off()

