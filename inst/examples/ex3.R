x <- sort(sample(1:10, 5, replace = TRUE))
for (i in x) {
    print(1:i)
}

#Equivalent:
schemeR::schemeR({
    .(`<-`, x, .(sort, .(sample, .(`:`, 1, 10), 5, replace = TRUE)))
    .(`for`, i, x, R({
        print(1:i)
    }))
})
