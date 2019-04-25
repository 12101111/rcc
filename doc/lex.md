# Lexical Rules of C programing language

## implemented in rcc

- **Token** := **Ident** | **KeyWord** | **Symbol** | **Constant** | **Literal**
- **Ident** := **L** { **L** | **D** }
- **Constant** := **Hex** | **Oct** | **Dec** | **Float** | **Char**
- **Hex** := 0[xX]**H**+
- **Oct** := 0**O**+
- **Dec** := [1-9]**D***
- **Float** := (**D**+)\\.(**D**+)
- **L** := [a-zA-Z_]
- **D** := [0-9]
- **O** := [0-7]
- **H** := [0-9a-fA-F]
- **Escape** := a | b | e | f | n | r | t | v | \\\\ | ' | " | ?
- **Char** := '(\\\\**Escape**|[^\\'])'
- **Literal** := "([\\\\**Escape**|^\"])*"
- **KeyWord** := char | double | float | int | long | short | signed 
| unsigned | void | enum | struct | union | break | case | continue 
| default | do | else | for | goto | if | return | switch | while 
| auto | const| extern | register | static | volatile | sizeof | typedef
- **Symbol** := \= | + | ++ | += | - | -- | -= | * | *= | / | /= | % | %= | << | <<= | >> | >>= | & | &= | \| | 
\|= | ^ | ^= | ~ | ! | && | \|\| | == | != | < | <= | > | >= | ? | : | . | -> | , | ; | ... | { | } | ( | ) | [ | ]

## TODO

- **FS** := (f|F|l|L)
- **IS** := (u|U|l|L|ll|LL)
- **E** := [eE][+-]?**D**+
- **Hex** := 0[xX]**H**+**IS**?
- **Oct** := 0**O**+**IS**?
- **Dec** := [1-9]**D*****IS**?
- **Float** := (**D***)\\.(**D**+) | (**D**+)\\.(**D***) | (**D**+)(\\.(**D***)?**E**
- **Char** := L?'(\\\\**Escape**|[^\\\\'])'
- **Literal** := L?"([\\\\**Escape**|^\\\\"])*"