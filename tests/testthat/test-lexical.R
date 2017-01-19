context("Lexical binding constructs")

test_that("let works correctly", {
    expect_equal(let(list(list(i, 3)), i+6), 9)

    i <- 3 #not mutually recursive
    expect_equal(let(list(list(i, 7), list(g, i+3)), g+i), 13)

    expect_error(let())
    expect_error(let(i+6))
    expect_error(let(list(), i+6))
    expect_error(let(list(i, 9), i+6))

    #A test of infix-format commands working
    expect_true(schemeR({
    .(let, .(.(i, 3), .(foo, 5)),
        .(all.equal, i, .(`-`, foo, 2)))
    }))

    expect_true(schemeR({
        .(set, i, 3)
        .(let, .(.(i, 7), .(g, .(`+`, i, 3))),
            .(all.equal, .(sum, g, i), 13))
    }))
})

test_that("let.star works correctly", {
    expect_equal(let.star(list(list(i, 3)), i+6), 9)

    i <- 3 #the inner binding for i binds more tightly
    expect_equal(let.star(list(list(i, 7), list(g, i+3)), g+i), 17)

    expect_error(let.star())
    expect_error(let.star(i+6))
    expect_error(let.star(list(), i+6))
    expect_error(let.star(list(i, 9), i+6))

    expect_true(schemeR({
    .(let.star, .(.(i, 3), .(foo, 5)),
        .(all.equal, i, .(`-`, foo, 2)))
    }))

    expect_true(schemeR({
        .(set, i, 3)
        .(let.star, .(.(i, 7), .(g, .(`+`, i, 3))),
          .(eq, .(sum, g, i), 17))
    }))
})

test_that("letrec works correctly", {
    expect_equal(letrec(list(list(i, 3)), i+6), 9)

    #mutually recursive - polar <=> cartesian
    expect_equal(letrec(list(list(x, 1), list(y, 1),
                        list(r, sqrt(x*x + y*y)), list(theta, atan2(y, x))),
                            c(r, theta)),
                 c(sqrt(2), pi / 4))

    expect_equal(letrec(list(list(x, r * cos(theta)), list(y, r * sin(theta)),
                        list(r, sqrt(2)), list(theta, pi / 4)),
                            c(x, y)),
                 c(1,1))

    expect_error(letrec())
    expect_error(letrec(i+6))
    expect_error(letrec(list(), i+6))
    expect_error(letrec(list(i, 9), i+6))
    expect_error(letrec(list(list(i, g+7), list(g, i+3)), g+i))

    schemeR({
    .(letrec, .(.(i, 3), .(foo, 5)),
        .(all.equal, i, .(`-`, foo, 2)))
    })

    schemeR({
    .(letrec, .(.(i, 3),
                .(foo, .(lambda, .(n), .(`+`, n, 1)))),
              .(all.equal, i, .(foo, 2)))
    })

    expect_true(schemeR({
        .(letrec, .(.(x, 1), .(y, 1),
                    .(r, .(sqrt, .(`+`, .(`*`, x, x), .(`*`, y, y)))),
                    .(theta, .(atan2, y, x))),
            .(and, .(all.equal, r, .(sqrt, 2)),
                   .(all.equal, theta, .(`/`, pi, 4))))
    }))

    expect_true(schemeR({
        .(letrec, .(.(x, 1), .(y, 1),
                    .(r, .(sqrt, .(`+`, .(`*`, x, x), .(`*`, y, y)))),
                    .(theta, .(atan2, y, x))),
            .(and, .(all.equal, x, 1),
                   .(all.equal, y, 1)))
    }))

    expect_true(schemeR({
        .(letrec, .(.(x, .(`*`, r, .(cos, theta))),
                    .(y, .(`*`, r, .(sin, theta))),
                    .(r, .(sqrt, 2)),
                    .(theta, .(`/`, pi, 4))),
           .(and, .(all.equal, x, 1),
                  .(all.equal, y, 1)))
    }))
})
