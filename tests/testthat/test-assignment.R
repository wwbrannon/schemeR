context("Assignment operators")

test_that("The define operator works", {
    define(a, 10)
    expect_equal(a, 10)

    define(b, a)
    expect_equal(b, 10)

    expect_error(define(a))
})

test_that("The set operator works", {
    set(a, 10)
    expect_equal(a, 10)

    set(b, a)
    expect_equal(b, 10)

    expect_error(set(a))
})

test_that("The set.pos operator works", {
    f <- 1:10
    set.pos(f, 3, 10)
    expect_equal(f, c(1, 2, 10, 4:10))

    g <- as.list(1:10)
    set.pos(g, 3, 10)
    expect_equal(g, as.list(c(1, 2, 10, 4:10)))

    h <- as.pairlist(g)
    set.pos(h, 3, 10)
    expect_equal(h, as.pairlist(as.list(c(1, 2, 10, 4:10))))

    f <- 1:10
    expect_error(set.pos(f, -1, 10))
    expect_error(set.pos(f, 11, 10))
})

test_that("The set.car operator works", {
    f <- 1:10
    set.car(f, 10)
    expect_equal(f, c(10, 2:10))

    g <- as.list(1:10)
    set.car(g, 10)
    expect_equal(g, as.list(c(10, 2:10)))

    h <- as.pairlist(g)
    set.car(h, 10)
    expect_equal(h, as.pairlist(as.list(c(10, 2:10))))
})

test_that("The set.cdr operator works", {
    f <- 1:10
    set.cdr(f, 10)
    expect_equal(f, c(10, 2:10))

    g <- as.list(1:10)
    set.car(g, 10)
    expect_equal(g, as.list(c(10, 2:10)))

    h <- as.pairlist(g)
    set.car(h, 10)
    expect_equal(h, as.pairlist(as.list(c(10, 2:10))))

    #errors
})
