
type unaryOp  = GetRef | DeRef | UnPlus |  UnMinus | Tilde | Not | GetRefLabel

type fixOp    = Dec | Inc

type assignOp = SimpleAssign | OpAssign of arithOp

and binaryOp = Arith of arithOp | Logical of logicalOp

       and arithOp   = 
         | Plus | Minus | Mul | Div | Mod
         | DecLeft | DecRight 
         | And | Or | Xor

       and logicalOp = 
         | Inf | Sup | InfEq | SupEq 
         | Eq | NotEq 
         | AndLog | OrLog


(* todo? a real generic code ast ? and generic visitor ? *)
