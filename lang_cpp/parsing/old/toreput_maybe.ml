define_val:
 /*(* but quite stupid cos better to use typedef than define for that *)*/
 | TIdent_Typedef
    { DefineType (nQ,(TypeName(fst $1,noTypedefDef()),[snd $1]))}
