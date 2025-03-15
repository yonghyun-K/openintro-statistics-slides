poverty = read.table("poverty.txt", header = T, sep = "\t")

library(openintro)
library(broom)
data(COL)

# scatterplot

pdf("poverty_hsgrad.pdf", 5.5, 4.3)
par(mar=c(4,4,1,1), las=1, mgp=c(2.5,0.7,0), cex.lab = 1.5, cex.axis = 1.5)
plot(poverty$Poverty ~ poverty$Graduates, ylab = "% in poverty", xlab = "% HS grad", pch=19, col=COL[1,2])
dev.off()

# scatterplot, with line

pdf("poverty_hsgrad_line.pdf", 5.5, 4.3)
par(mar=c(4,4,1,1), las=1, mgp=c(2.5,0.7,0), cex.lab = 1.5, cex.axis = 1.5)
plot(poverty$Poverty ~ poverty$Graduates, ylab = "% in poverty", xlab = "% HS grad", pch=19, col=COL[1,2])
lm_pov_grad = lm(poverty$Poverty ~ poverty$Graduates)
abline(lm_pov_grad, col = COL[4], lwd = 3)
abline(64, -0.6, col = COL[2], lwd = 3, lty = 2)
dev.off()

# linreg

lm(poverty$Poverty ~ poverty$Graduates)

tidy(lm(poverty$Poverty ~ poverty$Graduates)) %>%
  xtable()


# scatterplot, %poverty vs.%no husband

pdf("poverty_nohusband.pdf", 5.5, 4.3)
par(mar=c(4,4,1,1), las=1, mgp=c(2.5,0.7,0), cex.lab = 1.5, cex.axis = 1.5)
plot(poverty$Poverty ~ poverty$PercentFemaleHouseholderNoHusbandPresent, ylab = "% in poverty", xlab = "% female householder, no husband present", pch=19, col=COL[1,2])
dev.off()

pdf("poverty_hsgrad_manylines2.pdf", 5.5, 4.3)
par(mar=c(4,4,1,1), las=1, mgp=c(2.5,0.7,0), cex.lab = 1.5, cex.axis = 1.5)
plot(possum$total_l, possum$head_l,
     pch = 19,
     col = COL[1, 2],
     cex = 1.2,
     xlab = 'Total Length (cm)',
     ylab = 'Head Length (mm)')
lm_pov_grad = lm(possum$head_l ~ possum$total_l)
abline(lm_pov_grad, col = COL[4], lwd = 3)
coef = lm_pov_grad$coefficients
abline(coef[1] - 5, coef[2] * 0.99, col = COL[2], lwd = 3, lty = 2)
abline(0, coef[2] * 2, col = COL[3], lwd = 3, lty = 4)
abline(95, 0, lwd = 3, lty = 3)

legend("topleft",
       legend = c("(a)", "(b)", "(c)", "(d)"),
       col = c(COL[4], COL[2], COL[3], "black"), 
       lty = c(1, 2, 4, 3), 
       lwd = 3,  # Broader lines in legend
       bty = "n")  # Remove legend box
dev.off()


pdf("poverty_hsgrad_res2.pdf", 7.5, 4.3)

par(mar=c(4,4,1,1), las=1, mgp=c(2.5,0.7,0), cex.lab = 1.5, cex.axis = 1.5)

plot(possum$total_l, possum$head_l,
     pch = 19,
     col = COL[1, 2],
     # cex = 1.2,
     xlab = 'Total Length (cm)',
     ylab = 'Head Length (mm)')
lm_pov_grad = lm(possum$head_l ~ possum$total_l)
pred = predict(lm_pov_grad)
x = possum$total_l
for(i in 1:length(pred)){
  lines(c(x[i],x[i]), c(possum$head_l[i],pred[i]), col = COL[2])
}
abline(lm_pov_grad, col = COL[4], lwd = 3)

dev.off()