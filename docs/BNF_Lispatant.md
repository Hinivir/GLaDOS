# Backus-Naur Form of Lispatant

Please find here the **Backus-Naur Form** (BNF) of our **Lispatant** language.
This document explains the grammar of our language.
As an example, you can find the BNF of C [here](https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm)

```BNF
<argument-list> ::= <literal> (<literal>)*

<assignment> ::= "Lipbe" <identifier> ":" <literal>

<assignment-operator> ::= "+" 
                        | "-" 
                        | "*" 
                        | "div"
                        | "mod" 
                        | "==" 
                        | "<" 
                        | ">" 
                        | "<=" 
                        | ">="
                        | "!=" 

<block> ::= <instruction>* "Lipdo" <identifier> ":"

<condition> ::= <identifier> <comparison-operator> <literal>

<digit> ::= "0"-"9"

<integer> ::= "0"-"9"

<number> ::= <integer>

<letter_digit> ::= <letter> 
                | <digit>

<letter> ::= "a"-"z" 
            | "A"-"Z"

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

<literal> ::= <integer> 
            | <string> 
            | <identifier>

<newline> ::= '|'
            | ';'

<parameter-list> ::= <identifier> (<identifier>)*

<program> ::= (<expression>)*

<type-specifier> ::= "Lipbe"
```
