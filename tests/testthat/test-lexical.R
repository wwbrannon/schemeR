context("Lexical binding constructs")

test_that("let works correctly", {
    #schemeR::schemeR({
    #.(let, .(.(i, 3),
    #         .(foo, 5)),
    #    .(`==`, i, .(`-`, foo, 2)))
    #}, pkg=TRUE)
})

test_that("let.star works correctly", {
    #schemeR::schemeR({
    #.(let.star, .(.(i, 3), .(foo, 5)),
    #           .(`==`, i,
    #               .(`-`, foo, 2)))
    #}, pkg=TRUE)
})

test_that("letrec works correctly", {
    #schemeR::schemeR({
    #.(letrec, .(.(i, 3), .(foo, 5)),
    #          .(`==`, i,
    #              .(`-`, foo, 2)))
    #}, pkg=TRUE)

    #schemeR::schemeR({
    #.(letrec, .(.(i, 3),
    #            .(foo, .(lambda, .(n), .(`+`, n, 1)))),
    #          .(`==`, i, .(foo, 2)))
    #}, pkg=TRUE)
})
