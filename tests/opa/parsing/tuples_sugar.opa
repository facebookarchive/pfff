x = (1,"hello")
//do @assert(x == {f1 = 1; f2 = "hello"})
//do @assert(x.f1 == 1)
//do @assert({x with f2 = "goodbye"} == (1,"goodbye"))

b1 = (x == {f1 = 1; f2 = "hello"})
//b2 = (x.f1 == 1)
b3 = ({x with f2 = "goodbye"} == (1,"goodbye"))
