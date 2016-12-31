# The example below in the usual R syntax:
#x <- sort(sample(1:10, 5, replace=TRUE))
#for(i in x)
#{
#    print(1:i)
#}

#In our Lisp-style representation:
.(`<-`, x,
  .(sort,
    .(sample,
      .(`:`, 1, 10),
      5, replace=TRUE)))

.(`for`, i, x,
  .(`{`,
    .(print,
      .(`:`, 1, i))))
