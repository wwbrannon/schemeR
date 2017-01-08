#This example uses some of the more advanced Scheme functions we've
#implemented, and their translations into 'straight' R look odd, so
#the infix version of this isn't shown.

schemeR::schemeR({
.(let, .(x, .(c, 1, 3, 5, 7, 9)),
  .(do, .(.(x, .(c, 1, 3, 5, 7, 9), .(cdr, x)),
          .(s, 0, .(`+`, s, .(car, x))),
          .(foo, 4)),
    .(.(is.nil, x), s),
    T))
}) #=> 25
