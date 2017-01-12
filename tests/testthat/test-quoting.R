context("Quoting")

test_that("Quasiquote works", {
    #Interpolation
    f <- list(4,5,6)
    expect_equal(
        eval(quasiquote(list(1, 2, 3, .c(f), 7, 8, 9))),
        list(1,2,3, list(4,5,6), 7, 8, 9)
    )

    f <- list("a", "b", "c")
    expect_equal(
        eval(quasiquote(c(1, 2, 3, .c(f), 7, 8, 9))),
        c(1, 2, 3, list("a","b","c"), 7, 8, 9)
    )

    f <- list(4,5,6)
    expect_equal(
        eval(quasiquote(list(1, 2, 3, .c(f), 7, 8, 9))),
        list(1, 2, 3, list(4, 5, 6), 7, 8, 9)
    )

    f <- list()
    expect_equal(
        eval(quasiquote(list(1, 2, 3, .c(f), 4, 5, 6))),
        list(1, 2, 3, list(), 4, 5, 6)
    )

    f <- nil
    expect_equal(
        eval(quasiquote(list(1, 2, 3, .c(f), 4, 5, 6))),
        list(1, 2, 3, nil, 4, 5, 6)
    )

    #Splicing
    f <- list("a", "b", "c")
    expect_equal(
        eval(quasiquote(c(1, 2, 3, .s(f), 7, 8, 9))),
        as.character(list(1,2,3,"a","b","c",7,8,9))
    )

    f <- list(4,5,6)
    expect_equal(
        eval(quasiquote(list(1, 2, 3, .s(f), 7, 8, 9))),
        as.list(1:9)
    )

    f <- list()
    expect_equal(
        eval(quasiquote(list(1, 2, 3, .s(f), 4, 5, 6))),
        as.list(1:6)
    )

    f <- nil
    expect_equal(
        eval(quasiquote(list(1, 2, 3, .s(f), 4, 5, 6))),
        as.list(1:6)
    )
})
