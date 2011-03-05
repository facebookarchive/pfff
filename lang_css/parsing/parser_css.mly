/********************************************************************************/
/*      Parser.mly
        Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
        This software is distributed under the terms of the GNU GPL version 2.
        See LICENSE file for full license text.
*/
/********************************************************************************/

%{
let nelist = function
        | hd :: tl -> (hd, tl)
        | []       -> failwith "nelist"
%}


/********************************************************************************/
/* Token declarations.                                                          */
/********************************************************************************/

%token EOF
%token S

%token CHARSET IMPORT MEDIA PAGE FONTFACE

%token OPEN_CURLY CLOSE_CURLY
%token OPEN_ROUND CLOSE_ROUND
%token OPEN_SQUARE CLOSE_SQUARE
%token SEMICOLON COLON DOUBLE_COLON COMMA PERIOD SLASH
%token ASTERISK QUOTIENT PLUS MINUS
%token TILDE GT IMPORTANT

%token ATTR_EQUALS
%token ATTR_INCLUDES
%token ATTR_DASHMATCH
%token ATTR_PREFIX
%token ATTR_SUFFIX
%token ATTR_SUBSTRING

%token URI
%token <string> STRING
%token <string> IDENT
%token <string> NTH
%token <string> HASH
%token <string> VAR

%token <string> SEL_FUNC
%token <string> TERM_FUNC

%token <Ast.quantity_t> QUANTITY


/********************************************************************************/
/* Associativity and precedence declarations.                                   */
/********************************************************************************/

%left PLUS MINUS
%left ASTERISK QUOTIENT


/********************************************************************************/
/* Top-level statements.                                                        */
/********************************************************************************/

%type <Ast.t> stylesheet

%start stylesheet

%%

stylesheet:
        | s_star charset_opt statement_star EOF                                 {($2, $3)}

charset:
        | CHARSET STRING SEMICOLON                                      {$2}

statement:
        | IMPORT source s_opt media_list_opt SEMICOLON
            {`Import ($2, $4)}
        | MEDIA media_list OPEN_CURLY rule_plus CLOSE_CURLY
            {`Media ($2, $4)}
        | PAGE pseudo_page_opt declaration_block
            {`Page ($2, $3)}
        | FONTFACE declaration_block
            {`Fontface $2}
        | VAR COLON expr SEMICOLON  
            { raise Todo (* `Vardecl ($startpos($1), $1, $3) *)}
        | rule
            {`Rule $1}

source:
        | STRING
            {`String $1}
        | URI STRING CLOSE_ROUND
            {`Uri $2}

media_list:
        | medium_separated_nonempty_list_COMMA                  {$1}

medium:
        | IDENT                                                         {$1}

pseudo_page:
        | COLON IDENT                                                   {$2}

rule:
        | selector_list declaration_block                               {($1, $2)}


/********************************************************************************/
/* Selectors.                                                                   */
/********************************************************************************/

selector_list:
        | selector_separated_nonempty_list_COMMA                        {$1}

selector:
        | simple_selector combination_star                                      {($1, $2)}

combination:
        | combinator simple_selector                                    {($1, $2)}

combinator:
        | S                                                             {`Descendant}
        | TILDE                                                         {`General_sibling}
        | PLUS                                                          {`Adjacent_sibling}
        | GT                                                            {`Child}

simple_selector:
        | element qualifier_star                                                {`Explicit ($1, $2)}
        | qualifier_plus                                                        {`Generic (nelist $1)}

element:
        | IDENT                                                         {`Tag $1}
        | ASTERISK                                                      {`Universal}

qualifier:
        | HASH                                                          {`Id $1}
        | PERIOD IDENT                                                  {`Class $2}
        | OPEN_SQUARE IDENT attr_operation CLOSE_SQUARE                 {`Attr ($2, $3)}
        | COLON IDENT                                                   {`Pseudo_class $2}
        | DOUBLE_COLON IDENT                                            {`Pseudo_element $2}
        | SEL_FUNC function_args CLOSE_ROUND                            {`Sel_func ($1, $2)}

function_args:
        | qualifier_plus                                                        {`Qualified $1}
        | NTH                                                           {`Nth $1}
        | IDENT                                                         {`Nth $1}

attr_operation:
        | /* empty */                                                   {`Attr_exists}
        | ATTR_EQUALS attr_operand                                      {`Attr_equals $2}
        | ATTR_INCLUDES attr_operand                                    {`Attr_includes $2}
        | ATTR_DASHMATCH attr_operand                                   {`Attr_dashmatch $2}
        | ATTR_PREFIX attr_operand                                      {`Attr_prefix $2}
        | ATTR_SUFFIX attr_operand                                      {`Attr_suffix $2}
        | ATTR_SUBSTRING attr_operand                                   {`Attr_substring $2}

attr_operand:
        | IDENT                                                         {$1}
        | STRING                                                        {$1}


/********************************************************************************/
/* Declarations.                                                                */
/********************************************************************************/

declaration_block:
        | OPEN_CURLY declaration_plus CLOSE_CURLY                               {$2}

declaration:
        | IDENT COLON expr boption_IMPORTANT SEMICOLON                  {($1, $3, $4)}

expr:
        | sentence_separated_nonempty_list_COMMA                        {$1}

sentence:
        | term_separated_nonempty_list_sopt                             {$1}

term:
        | calc                                                          {`Calc $1}
        | STRING                                                        {`String $1}
        | IDENT                                                         {`Ident $1}
        | URI STRING CLOSE_ROUND                                        {`Uri $2}
        | HASH                                                          {`Hash $1}
        | TERM_FUNC expr CLOSE_ROUND                                    {`Term_func ($1, $2)}
        | SLASH                                                         {`Slash}

calc:
        | VAR
            {raise Todo (* `Varref ($startpos($1), $1) *)}
        | QUANTITY                                                      {`Quantity $1}

/*

        | calc ASTERISK calc                                            {`Mul ($startpos($2), $1, $3)}
        | calc QUOTIENT calc                                            {`Div ($startpos($2), $1, $3)}
        | calc PLUS calc                                                {`Sum ($startpos($2), $1, $3)}
        | calc MINUS calc                                               {`Sub ($startpos($2), $1, $3)}
        | OPEN_ROUND calc CLOSE_ROUND                                   {$2}
*/


s_star: S { }
charset_opt: S { }
statement_star: S { }
s_opt: S { }
media_list_opt: S { }
rule_plus: S { }
pseudo_page_opt: S { }

medium_separated_nonempty_list_COMMA: S { }
selector_separated_nonempty_list_COMMA: S { }
combination_star: S { }
qualifier_star: S { }
qualifier_plus: S { }
declaration_plus: S { }
boption_IMPORTANT: S { }
sentence_separated_nonempty_list_COMMA: S { }
term_separated_nonempty_list_sopt: S { }



