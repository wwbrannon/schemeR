context("Prefix flow-control operators")

test_that('"or" and "and" work correctly', {
    expect_true(or(1==2, TRUE, print("foo")))
    expect_length(capture.output(f <- (or(1==2, TRUE, print("foo")))), 0)
    expect_equal(f, TRUE)

    expect_equal(or(1==2, FALSE, capture.output(cat("foo")), 1), "foo")
    expect_length(capture.output(f <- (or(1==2, FALSE, print("foo")))), 1)
    expect_equal(f, "foo")

    expect_false(and(1==2, TRUE, print("foo")))
    expect_length(capture.output(f <- and(1==2, TRUE, print("foo"))), 0)
    expect_equal(f, FALSE)

    expect_equal(and(1==1, TRUE, 1, capture.output(cat("foo"))), "foo")
    expect_length(capture.output(f <- and(1==1, TRUE, print("foo"))), 1)
    expect_equal(f, "foo")
})

test_that("when and unless work correctly", {
    expect_equal(capture.output(when(1==2, cat("foo"))), "NULL")
    expect_equal(capture.output(when(NULL, cat("foo"))), "NULL")

    expect_equal(capture.output(when(1==1, cat("foo"))), "foo")
    expect_equal(capture.output(when(1==1, 3+4, cat("foo"))), "foo")

    expect_equal(capture.output(unless(1==1, cat("foo"))), "NULL")
    expect_equal(capture.output(unless(NULL, cat("foo"))), "foo")

    expect_equal(capture.output(unless(1==2, cat("foo"))), "foo")
    expect_equal(capture.output(unless(1==2, 3+4, cat("foo"))), "foo")
})

test_that("case works correctly", {
    f <- capture.output(case(3, list(3, cat("foo")), list(4, FALSE)))
    expect_equal(f, "foo")

    f <- capture.output(case(4, list(3, print("foo")), list(4, FALSE)))
    expect_equal(f, "[1] FALSE")

    #Evaluation of first arg
    expect_equal(case(4 + 6,  list(quote(4 + 6), "a"), list(10, "b")), "b")

    f <- case(sample(c(10,11,12), 1),  list(c(4,5,6), "a"),
                                       list(c(10, 11, 12), "b"))
    expect_equal(f, "b")

    f <- case(sample(c(10,11,12), 1),  list(list(4,5,6), "a"),
              list(list(10, 11, 12), "b"))
    expect_equal(f, "b")

    expect_error(case())
    expect_error(case(3))
    expect_error(case(3, 4))
    expect_error(case(3, list()))

    expect_equal(capture.output(schemeR({
        .(case, .(`+`, 1, 1),
            .(3, .(cat, "foo")),
            .(2, .(cat, "bar")))
    })), "bar")
})

test_that("cond works correctly", {
    expect_null(cond())

    expect_equal(cond(list(3, "a"), list(4, "b")), "a")
    expect_equal(cond(list(FALSE, "a"), list(4, "b")), "b")
    expect_equal(cond(list(0, "a"), list(4, "b")), "b")
    expect_equal(capture.output(cond(list(FALSE, cat("foo")),
                                     list(TRUE, cat("bar")))),
                 "bar")

    expect_error(cond(3))
    expect_error(cond(3, 4))
    expect_error(cond(list(3), 4))
    expect_error(cond(list(3), list(4)))
    expect_error(suppressWarnings(cond(list(quote(3+4), "a"), list(TRUE, "b"))))

    expect_equal(capture.output(schemeR({
        .(cond,
            .(.(`==`, .(`+`, 1, 4), 4), .(cat, "foo")),
            .(.(`==`, .(`+`, 1, 3), 5), .(cat, "bar")),
            .(TRUE, .(cat, "baz")))
    })), "baz")
})

test_that("do works correctly", {
    #Things that shouldn't work don't
    expect_error(do())
    expect_error(do(list(list(x, 1))))
    expect_error(do(list(list(x))))
    expect_error(do(list(list(x, 1), list(y, 2))))
    expect_error(do(list(list(x), list(y))))

    #A twofer: a) we can give no bindings, b) vars in containing scopes
    #are found
    x <- 9
    expect_equal(do(list(), list(x > 5, x)), 9)
    f <- function() { do(list(), list(x > 5, x)) }
    expect_equal(f(), 9)

    #Works with and without step expressions
    expect_equal(do(list(list(x, 6), list(y, 1)), list(x > 10, 20),
                    x <- x + 1), 20)
    expect_equal(do(list(list(x, 6, x+1), list(y, 1, y+1)),
                    list(x > 10, 20)),
                 20)

    #The test expression's value is returned if another isn't supplied
    expect_true(do(list(list(x, 6, x+1), list(y, 1, y+1)),
                    list(x > 10)))

    #The command expressions are evaluated (in order); also tested above
    expect_equal(capture.output(do(list(list(x, 6), list(y, 1)), list(x > 10, 20),
                    x <- x + 1, cat("foo"), cat("bar"))),
                 "foobarfoobarfoobarfoobarfoobar[1] 20")

    #Another twofer: a) works in prefix notation, b) on each iteration, all
    #step expressions are recomputed before rebinding any of the variables
    expect_equal(schemeR({
        .(do, .(.(x, .(c, 1, 3, 5, 7, 9), .(cdr, x)),
                .(s, 0, .(`+`, s, .(car, x)))),
            .(.(is.nil, x), s))
    }), 25)
})
