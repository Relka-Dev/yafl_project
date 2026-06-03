# Type abstractions
- Person:
- File:
- Lign: 

# Prefix terms
- Person:
- File:
- Lign: 

# Universal types
- Person:
- File:
- Lign: 

# Arrow types
- Person:
- File:
- Lign: 

# Parenthesized types
- Person: Maroua
- File: scala/parser/Parser.scala
- Lign: 380-387

# Type applications
- Person:
- File:
- Lign: 

# Recursive abstractions
- Person:
- File:
- Lign: 

# Multiple parameters and arguments
- Person: Aurélie
- File: [src/main/scala/parser/Parser.scala](src/main/scala/parser/Parser.scala)
- Functions:
  - `Parser.trailingTypeArguments`
  - `Parser.typeApplication` (updated to call `trailingTypeArguments`)
  - `Parser.trailingTypeParameters`
  - `Parser.typeAbstraction` (updated to call `trailingTypeParameters`)


# Normalization
- Person: Aurélie
- File: [src/main/scala/optimizer/Optimizer.scala](src/main/scala/optimizer/Optimizer.scala)
- Functions:
  - `Optimizer.normalize`
  - `Optimizer.constantFoldRecursively` (updated to call `normalize`)

# Dead code elimination
- Person: Aurélie
- Files: 
  - [src/main/scala/optimizer/Optimizer.scala](src/main/scala/optimizer/Optimizer.scala)
  - [src/test/scala/Optimizer.scala](src/test/scala/Optimizer.scala)
- Functions:
  - `Optimizer.deadCodeEliminate`
  - `Optimizer.occursIn`
  - `Optimizer.constantFoldRecursively` (updated to call `deadCodeEliminate`)

# Built-in arithmetic and comparison
- Person: David
- Files:
  - [src/main/scala/emitter/Emitter.scala](src/main/scala/emitter/Emitter.scala)
  - [src/main/scala/optimizer/Optimizer.scala](src/main/scala/optimizer/Optimizer.scala)
  - [src/main/scala/syntax/InfixOperator.scala](src/main/scala/syntax/InfixOperator.scala)
  - [src/test/scala/Emitter.scala](src/test/scala/Emitter.scala)
- Lign: 

# Constant propagation
- Person: Maroua
- File:
    - [src/main/scala/optimizer/Optimizer.scala](src/main/scala/optimizer/Optimizer.scala)
    - [src/test/scala/Optimizer.scala](src/test/scala/Optimizer.scala)
- Functions:
    - `Optimizer.constantPropagation`
    - `Optimizer.constantFoldRecursively` (updated to call `constantPropagation`)

# Inlining
- Person:
- File:
- Lign:

# Bindings
- Person:
- File:
- Lign: 