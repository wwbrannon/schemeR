# A Common Lisp version from StackOverflow
# (defmacro lcomp (expression for var in list conditional conditional-test)
#     (let ((result (gensym)))
#         `(let ((,result nil))
#           (loop for ,var in ,list
#               ,conditional ,conditional-test
#               do (setq ,result (append ,result (list ,expression))))
#           ,result)))

# A prefix version
schemeR::schemeR({
.(defmacro, lcomp, .(exp, s1, var, s2, lst, s3, test),
    .(let, .(.(result, .(gensym, "G1")), .(lvar, .(gensym, "G2"))),
        .b(.(let, .(.(.c(lvar), .c(lst))),
                .(do, .(.(.c(result), nil, .(cond, .(.c(test), .(append, .c(result), .c(exp))),
                                                   .(TRUE, .c(result)))),
                        .(.c(var), car(.c(lvar)), car(.c(lvar)))),
                      .(.(is.nil, .c(lvar)), .c(result)),
                      .(set, .c(lvar), .(cdr, .c(lvar))))))))
}, pkg=TRUE)

# An infix version
zls <- alist(x=)$x
defmacro(lcomp, alist(exp=zls, s1=zls, var=zls, s2=zls, lst=zls,
                      s3=zls, test=zls),
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
