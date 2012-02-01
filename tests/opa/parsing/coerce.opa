x = [] : list(int)

x = [] : list(list) // means list(list('a))

x : list(int) = [] // same as s = [] : list(int)
f(x) : list(int) = [x] // annotation of the body of the function
                       // same as f(x) = [x] : list(int)
