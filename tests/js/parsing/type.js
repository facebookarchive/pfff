function foo(str:string, i:int):string {
  return str;
}
var bar: (str:string, i:int)=>string = foo;

var qux = function(str:string, i:int):string { return foo(str,i); }

var obj: {str:string; i:int} = {str: "...", i: 0};
