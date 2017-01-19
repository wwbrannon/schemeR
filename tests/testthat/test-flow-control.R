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
    #FIXME

    expect_equal(capture.output(schemeR({
        .(cond,
            .(.(`==`, .(`+`, 1, 4), 4), .(cat, "foo")),
            .(.(`==`, .(`+`, 1, 3), 5), .(cat, "bar")),
            .(TRUE, .(cat, "baz")))
    })), "baz")
})

test_that("do works correctly", {
    #FIXME

    # schemeR({
    #     .(let, .(.(x, .(c, 1, 3, 5, 7, 9))),
    #         .(do, .(.(x, .(c, 1, 3, 5, 7, 9), .(cdr, x)),
    #                 .(s, 0, .(`+`, s, .(car, x))),
    #                 .(foo, 4)),
    #             .(.(is.nil, x), s),
    #     TRUE)) == 25
    # })
})
