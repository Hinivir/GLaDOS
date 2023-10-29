# Documentation about Lispatant

---  

Récap des informations sur le Lispatant
```
Print: lipsaid
Return: ret
Les types: int & uint, bool, str, char, decim
List [int]
Func lipdo _(création d'un warning sans lipdo ?)_
Dico [Int, Str]
Pas de for ou de while
Fichier => .lip
```

---  

1) Exemple du Lisp au Lispatant:
```lisp
(define (add a b)
    (+ a b))
(add 3 4)
```

```python
Lipdo add a<Any> b<Any>:
  a + b
Lipdo main:
  add (3, 4)
```

---  

2) Exemple du Lisp au Lispatant:
```lisp
(define (fact x)
    (if (eq? x 1)
        1
        (* x (fact (- x 1)))))
(fact 10)
```
```python
Lipdo fact x<Int>:
  if (x == 1)
    1
  else
    x * (fact (x - 1))
Lipdo main:
  fact (10)
```

---  

3) Exemple du Lisp au Lispatant:
```
(define foo 42)
(if (< foo 10)
    (* foo 3)
    (div foo 2))
```
```python
Lipbe foo:
  42
Lipdo main:
  if (foo < 10)
    foo * 3
  else
    foo / 2
```

---  

Autre proposition:
```python
Lipbe foo:|42|Lipdo main:|if (foo < 10)|foo * 3|else|foo / 2
```

Autre proposition:
```python
Lipbe foo:
  42
Lipdo main:
  if (foo < 10)
    foo \
      * 3
  else
    foo \
      / 2
```
