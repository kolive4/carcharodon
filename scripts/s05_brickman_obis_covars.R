plot(present_brick[1,,,8], reset = FALSE)
plot(aug_shark, add = TRUE, col = 'orange')
plot(st_geometry(brick_buf), border = "blue", lwd = 3, add = TRUE)

# may = present_brick[1,,,5, drop = TRUE]
# z = may[[1]]
# ix = z <= 0
# z[ix] <- NA
# 
# may2 = may
# may2[[1]] = z
# 
# plot(may2)
