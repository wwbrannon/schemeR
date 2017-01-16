# A Common Lisp version from StackOverflow, not exactly analogous to what's
# below:
#
# (defmacro lcomp (expression for var in list conditional conditional-test)
#     (let ((result (gensym)))
#         `(let ((,result nil))
#           (loop for ,var in ,list
#               ,conditional ,conditional-test
#               do (setq ,result (append ,result (list ,expression))))
#           ,result)))

# An infix version
args <- c("exp", "s1", "var", "s2", "lst", "s3", "test")
al <- replicate(length(args), alist(x=)$x)
names(al) <- args

defmacro(lcomp, al,
     let(list(list(result, gensym("G1")), list(lvar, gensym("G2"))),
         quasiquote({
             let(list(list(.c(lvar), .c(lst))),
                 do(list(
                     list(.c(result), nil, cond(list(.c(test), append(.c(result), list(.c(exp)))),
                                                list(TRUE, .c(result)))), #leave unchanged
                     list(.c(var), car(.c(lvar)), car(.c(lvar)))),
                    list(is.nil(.c(lvar)), .c(result)),
                    set(.c(lvar), cdr(.c(lvar)))))
         })
     )
)

lcomp(x^2, `for`, x, `in`, 1:10, `if`, x >= 5)

# A prefix version
schemeR::schemeR({
.(defmacro, lcomp2, .(exp, s1, var, s2, lst, s3, test),
    .(let, .(.(result, .(gensym, "G1")), .(lvar, .(gensym, "G2"))),
        .b(.(let, .(.(.c(lvar), .c(lst))),
                .(do, .(.(.c(result), nil, .(cond, .(.c(test), .(append, .c(result), .c(exp))),
                                                   .(TRUE, .c(result)))),
                        .(.c(var), car(.c(lvar)), car(.c(lvar)))),
                      .(.(is.nil, .c(lvar)), .c(result)),
                      .(set, .c(lvar), .(cdr, .c(lvar))))))))
}, pkg=TRUE)

lcomp2(x^2, `for`, x, `in`, 1:10, `if`, x >= 5)
