The lab was made by Grace Naing, Maroua Zanad, Karel Svoboda, David Braz and Aurélie Pham . We divided the work as follows:

# Type abstractions

- Person: Karel
- File: [src/main/scala/parser/Parser.scala](src/main/scala/parser/Parser.scala)
- Function :
  - `Parser.typeAbstraction`
  - `Parser.SimpleType` (updated to call `typeAbstraction`)

# Prefix terms

- Person: Grace
- File: [src/main/scala/parser/Parser.scala](src/main/scala/parser/Parser.scala)
- Functions:
  - `Parser.prefixOperator`
  - `Parser.prefixTerm`

# Universal types

- Person: Grace
- File: [src/main/scala/parser/Parser.scala](src/main/scala/parser/Parser.scala)
- Lign:
  - `Parser.forAll`
  - `Parser.SimpleType` (updated to call `forAll`)

# Arrow types

- Person: David + Aurélie
- File: [src/main/scala/parser/Parser.scala](src/main/scala/parser/Parser.scala)
- Function: `Parser.typ3` (updated)

# Parenthesized types

- Person: Maroua
- File: [src/main/scala/parser/Parser.scala](src/main/scala/parser/Parser.scala)
- Function: `Parser.parenthesizedType`

# Type applications

- Person: Maroua + Grace
- File: [src/main/scala/parser/Parser.scala](src/main/scala/parser/Parser.scala)
- Function: `Parser.typeApplication` (updated)

# Recursive abstractions

- Person: David
- File: [src/main/scala/parser/Parser.scala](src/main/scala/parser/Parser.scala)
- Function:
  - `Parser.recursiveTypeAbstraction`
  - `Parser.simpleTerm` (updated to call `recursiveTypeAbstraction`)

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
- Functions:
  - `Emitter.emitAsValue` (updated to match case `infixOperatiors`)
  - `Optimizer.constantFold`
  - `InfixOperator enum`
  - `InfixOperator.unapply`
  - Tests: `Emitter` : `integer substraction`, `integer multiplication`, `integer division`, `integer equal comparison`, `integer not equal comparison`, `integer greater than comparison`, `integer less than comparison`

# Constant propagation

- Person: Maroua
- File:
  - [src/main/scala/optimizer/Optimizer.scala](src/main/scala/optimizer/Optimizer.scala)
  - [src/test/scala/Optimizer.scala](src/test/scala/Optimizer.scala)
- Functions:
  - `Optimizer.constantPropagation`
  - `Optimizer.constantFoldRecursively` (updated to call `constantPropagation`)

# Inlining

- Person: Karel + Grace
- File:
  - [src/main/scala/optimizer/Optimizer.scala](src/main/scala/optimizer/Optimizer.scala)
  - [src/test/scala/Optimizer.scala](src/test/scala/Optimizer.scala)
- Functions:
  - `Optimizer.substitute`
  - `Optimizer.constantPropagation` (updated to call all `TermTree` types)
  - `Optimizer.constantFoldRecursively` (updated to used `betaReduced`)
