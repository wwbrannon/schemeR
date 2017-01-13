context("Functional-programming operators")

test_that("Funprog aliases and functions are correct", {
    expect_identical(map, Map)
    expect_identical(reduce, Reduce)
    expect_identical(keep.matching, Filter)

    expect_equal(delete.matching(is.positive, seq(-10, 10)), seq(-10, 0))
    expect_equal(delete.matching(is.zero, seq(-10, 10)),
                 c(seq(-10, -1), seq(1, 10)))
})

test_that("Function composition is correct", {
    expect_equal(compose(sum, c)(3), sum(c(3)))

    f <- as.list(1:10)
    expect_equal(compose(car, cdr)(f), car(cdr(f)))
    expect_equal(compose(cdr, car)(f), cdr(car(f)))
    expect_equal(compose(car, cdr, cdr)(f), car(cdr(cdr(f))))
    expect_equal(compose(cdr, car, cdr)(f), cdr(car(cdr(f))))
    expect_equal(compose(car, cdr, cdr)(f), car(cdr(cdr(f))))

    expect_error(compose(nil, nil)(3))
    expect_error(compose(nil, c)(3))
    expect_error(compose(sum, nil)(3))
    expect_error(compose(list(), c)(3))
    expect_error(compose("a", c)(3))
})

test_that("Misc higher-order functions work correctly", {
    g <- seq(-15, 15)
    expect_equal(member.if(conjunct(is.positive, is.even), g), 2:15)

    g <- as.list(seq(-15, 15))
    expect_equal(member.if(conjunct(is.positive, is.even), g), as.list(2:15))

    g <- as.pairlist(as.list(seq(-15, 15)))
    expect_equal(member.if(conjunct(is.positive, is.even), g),
                 as.pairlist(as.list(2:15)))

    g <- seq(-15, 15)
    expect_null(member.if(function(x) x > 30, g))
    expect_error(member.if(f=3, x=g))

    g <- seq(-15, 15)
    expect_equal(member.if.not(disjunct(is.negative, is.odd), g), 0:15)

    g <- as.list(seq(-15, 15))
    expect_equal(member.if.not(disjunct(is.negative, is.odd), g), as.list(0:15))

    g <- as.pairlist(as.list(seq(-15, 15)))
    expect_equal(member.if.not(disjunct(is.negative, is.odd), g),
                 as.pairlist(as.list(0:15)))

    g <- seq(-15, 15)
    expect_null(member.if.not(function(x) x < 30, g))
    expect_error(member.if.not(3, g))

    expect_equal(zip(), list())
    expect_equal(zip(nil), list())
    expect_equal(zip(nil, nil), list())
    expect_equal(zip(1:3, 4:6, 7:9),
                 list(list(1, 4, 7), list(2, 5, 8), list(3, 6, 9)))
    expect_equal(zip(1:3, 4:6, nil), list())
    expect_equal(zip(1:3, 4:6, list()), list())
    expect_equal(zip(1,2,3), list(list(1,2,3)))
    expect_error(zip(sum, c, body))

    #recycling for zip
    expect_equal(zip(1:3, 4:6, 7),
                 list(list(1, 4, 7), list(2, 5, 7), list(3, 6, 7)))

    f <- function(...) cat(sum(as.numeric(list(...))), "\n", sep="")
    expect_equal(capture.output(r <- for.each(f, 1:3, 4:6, 7:9)),
                 c("12", "15", "18"))
    expect_length(capture.output(r <- for.each(f)), 0)

    expect_null(conjunct())
    expect_error(conjunct(1,2))
    expect_error(conjunct("a", "b"))
    expect_error(conjunct("a", 1))
    expect_error(conjunct("a", list(1,2)))

    f <- conjunct(is.positive, is.even)
    expect_true(f(4))
    expect_false(f(-4))
    expect_false(f(0))
    expect_error(f("a"))

    expect_null(disjunct())
    expect_error(disjunct(1,2))
    expect_error(disjunct("a", "b"))
    expect_error(disjunct("a", 1))
    expect_error(disjunct("a", list(1,2)))

    f <- disjunct(is.positive, is.even)
    expect_true(f(4))
    expect_true(f(-4))
    expect_false(f(-3))
    expect_true(f(0))
    expect_error(f("a"))
})

test_that("Currying works as expected", {
    #Currying
    f <- curry(sum)
    expect_equal(f(), 0)
    expect_equal(f(3, 4), 7)

    f <- curry(sum, nil)
    expect_equal(f(3), 3)

    f <- curry(sum, 5, 6)
    expect_equal(f(3), 14)

    f <- curry(compose, sum)(scale)
    expect_equal(f(1:10), 0) #centered

    #Lazy currying - first that all the above calls work, then
    #test that it's lazy
    f <- lazy.curry(sum)
    expect_equal(f(), 0)
    expect_equal(f(3, 4), 7)

    f <- lazy.curry(sum, nil)
    expect_equal(f(3), 3)

    f <- lazy.curry(sum, 5, 6)
    expect_equal(f(3), 14)

    f <- lazy.curry(compose, sum)(scale)
    expect_equal(f(1:10), 0) #centered

    f1 <-
    function(...)
    {
        args <- eval(substitute(alist(...)))
        lst <- c(list(as.symbol("{")), args)
        return(as.call(lst))
    }
    expect_length(capture.output(f2 <- lazy.curry(f, print("foo"))), 0)
    expect_equal(f1(print("foo"), print("bar")), expression({
        print("foo")
        print("bar")
    })[[1]])

    #Uncurrying
    f <- curry(compose, sum)
    expect_equal(uncurry(f), compose)

    f <- curry(curry(sum, 4), 6)
    expect_false(isTRUE(all.equal(uncurry(f), sum)))
    expect_equal(uncurry(uncurry(f)), sum)
})

test_that("lambda works as expected", {
    f <- lambda(list(y=alist(z=)$z, x=5), x+y)
    expect_error(f())
    expect_equal(f(3), 8)

    f <- lambda(list(y=2, x=5), x+y)
    expect_equal(f(), 7)
    expect_equal(f(4, 5), 9)

    f <- lambda(list(), nil)
    expect_null(f())

    expect_error(lambda(list()))
    expect_error(lambda(list(n), n+5))
    expect_error(lambda(list(n=alist(x=)$x)))
})
