.(`<-`, x, .(sort, .(sample, .(`:`, 1, 10), 5, replace = TRUE)))
.(`for`, i, x, .(print, .(`:`, 1, i)))