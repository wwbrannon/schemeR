#schemeR({
#.(defmacro, lcomp, .(exp, s1, var, s2, lst, conditional, test),
#    .(let, .(.(result, .(gensym))),
#        .b(.(do, .(.(.c(result), nil, .(append, .c(result), .(list, exp)))),
#                 .(.(conditional, test), .c(result))))))
#})
#
