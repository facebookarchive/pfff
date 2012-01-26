//  now various shorthands
x = {a} //  means {a=void}
x = {a b=2} //  means {a=void b=2}
x = {~a b=2} //  means {a=a b=2}
x = ~{a b} //  means {a=a b=b}
x = ~{a b c=4} //  means {a=a b=b c=4}
x = ~{a={b} c} //  means {a={b=void} c=c}, NOT {a={b=b} c=c}
