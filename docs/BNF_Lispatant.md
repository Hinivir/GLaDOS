# Backus-Naur Form of Lispatant

Please find here the **Backus-Naur Form** (BNF) of our **Lispatant** language.
This document explains the grammar of our language.
As an example, you can find the BNF of C [here](BNF_C.md)

```BNF
<argument-list> ::= <literal> (<literal>)*

<assignment> ::= "Lipbe" <identifier> ":" <literal>

<assignment-operator> ::= "+" 
                        | "-" 
                        | "*" 
                        | "/" 
                        | "==" 
                        | "!=" 
                        | "<" 
                        | ">" 
                        | "<=" 
                        | ">="

<block> ::= <instruction>* "Lipdo" <identifier> ":"

<condition> ::= <identifier> <comparison-operator> <literal>

<digit> ::= "0"-"9" #???

<integer> ::= (0-9)+ #???

<identifier> ::= (<letter> 
                | "_") (<letter> 
                | <digit> 
                | "_")*

<letter> ::= "a"-"z" 
            | "A"-"Z"

<letter_digit> ::= <letter> 
                | <digit>

<number> ::= <integer>

<value> ::= <number>
              | "True"
              | "False"

<expression> ::= <value>
              | <identifier>
              | "(" <expression> ")"
              | <expression> <operator> <expression>
              | "if" "(" <expression> ")" <expression> "else" <expression>

<function-call> ::= <identifier> (<argument-list>)*

<function-definition> ::= "Lipdo" <identifier> (<parameter-list>)* ":" <expression>

<if-statement> ::= "if" <condition> <expression> ("else" <expression>)?

<instruction> ::= "Lipdo" <identifier> ":" <block>
              | "Lipbe" <identifier> ":" <expression>

<literal> ::= <integer> 
            | <string> 
            | <identifier>

<parameter-list> ::= <identifier> (<identifier>)*

<program> ::= (<expression>)*

//<type-specifier> ::= char
//                   | int
//                   | str
```
