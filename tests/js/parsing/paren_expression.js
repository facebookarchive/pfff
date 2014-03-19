
// this test shows why shifting on rparen instead of reducing can be bad
var t1 = (x) + y

// this test reveals an existing bug due to a shift/reduce conflict
//var t2 = (x,1)