# Backus-Naur Form of Lispatant

Please find here the **Backus-Naur Form** (BNF) of our **Lispatant** language.
This document explains the grammar of our language.
As an example, you can find the BNF of C [here]()

```BNF
<argument-list> ::= <literal> (<literal>)*

<assignment> ::= "Lipbe" <identifier> ":" <literal>

<assignment-operator> ::= =

<condition> ::= <identifier> <comparison-operator> <literal>

#<comparison-operator> ::= "==" | "<" | ">" | "<=" | ">="

<digit> ::= "0"-"9"

<expression> ::= (<function-definition> | <function-call> | <assignment> | <if-statement>)

<function-call> ::= <identifier> (<argument-list>)*

<function-definition> ::= "Lipdo" <identifier> (<parameter-list>)* ":" <expression>

<identifier> ::= (<letter> | "_") (<letter> | <digit> | "_")*

<if-statement> ::= "if" <condition> <expression> ("else" <expression>)?

<integer> ::= (0-9)+

<letter> ::= "a"-"z" | "A"-"Z"

<literal> ::= <integer> | <string> | <identifier>

<parameter-list> ::= <identifier> (<identifier>)*

<program> ::= (<expression>)*

<type-specifier> ::= char
                   | int
                   | str

<unary-operator> ::= +
                   | -
                   | *
```
