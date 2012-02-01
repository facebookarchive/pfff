x = {a=1 b={c="mlk" d=3.}}
y = {x with a=3} //  means {a=3; b=x.b}
y = {x with a=3 b={e}} //  you can redefine as many fields as you want
                       //  at the same time (but not zero) and even all of them
//  You can also update fields deep in the record
y = {x with a.c = "po"} //  means {x with a = {x.a with c = "po"}}
                        //  whose value is {a=1 b={c="po" d=3.}}
//  the same syntactic shortcuts as above are available
y = {x with a} //  means {x with a=void}, even if it is not terribly useful
y = {x with ~a} //  means {x with a=a}
y = ~{x with a b={e}} //  means {x with a=a b={e}}
