plot(envs[1], main = names(envs)[1], reset = FALSE)
# To add sf objects to a plot, use add = TRUE
plot(shark_occs, add = TRUE, col = 'orange')
plot(shark_occs.buf, border = "blue", lwd = 3, add = TRUE)

plot(envs[1], main = names(envs)[1], reset = FALSE)
# To add sf objects to a plot, use add = TRUE
plot(grey_seal_occs, add = TRUE, col = 'orange')
plot(gseal_occs.buf, border = "blue", lwd = 3, add = TRUE)

plot(envs[1], main = names(envs)[1], reset = FALSE)
# To add sf objects to a plot, use add = TRUE
plot(harbor_seal_occs, add = TRUE, col = 'orange')
plot(hseal_occs.buf, border = "blue", lwd = 3, add = TRUE)