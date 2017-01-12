context("Assignment operators")

#FIXME need type coercion tests for the set.* functions

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
    set.cdr(f, 1:3)
    expect_equal(f, c(1, 1:3))

    f <- 1:10
    set.cdr(f, as.list(1:3))
    expect_equal(f, as.list(c(1, 1:3)))

    f <- 1:10
    set.cdr(f, as.pairlist(as.list(1:3)))
    expect_equal(f, as.pairlist(as.list(c(1, 1:3))))

    g <- as.list(1:10)
    set.cdr(g, 1:3)
    expect_equal(g, as.list(c(1, 1:3)))

    g <- as.list(1:10)
    set.cdr(g, as.list(1:3))
    expect_equal(g, as.list(c(1, 1:3)))

    g <- as.list(1:10)
    set.cdr(g, as.pairlist(as.list(1:3)))
    expect_equal(g, as.pairlist(as.list(c(10, 2:10))))

    h <- as.pairlist(as.list(1:10))
    set.cdr(h, 1:3)
    expect_equal(h, as.pairlist(as.list(c(1, 1:3))))

    h <- as.pairlist(as.list(1:10))
    set.cdr(h, as.list(1:3))
    expect_equal(h, as.pairlist(as.list(c(1, 1:3))))

    h <- as.pairlist(as.list(1:10))
    set.cdr(h, as.pairlist(as.list(1:3)))
    expect_equal(h, as.pairlist(as.list(c(1, 1:3))))

    f <- 1:10
    set.cdr(f, 10)
    expect_equal(f, c(1, 10))

    f <- as.list(1:10)
    set.cdr(f, 10)
    expect_equal(f, list(1, 10))

    f <- as.pairlist(as.list(1:10))
    set.cdr(f, 10)
    expect_equal(f, as.pairlist(list(1, 10)))

    f <- 1:10
    f <- set.cdr(f, nil)
    expect_equal(f, 1)

    f <- as.list(1:10)
    f <- set.cdr(f, nil)
    expect_equal(f, list(1))

    f <- as.pairlist(as.list(1:10))
    f <- set.cdr(f, nil)
    expect_equal(f, as.pairlist(as.list(1)))

    f <- 1:10
    expect_error(set.cdr(f, as.numeric))
    expect_error(set.cdr(f, numeric(0)))
    expect_error(set.cdr(f, list()))
})
