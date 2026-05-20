```scala
private def conditional(using Context): Result[Syntax[TermTree.Conditional]] =
    take(Token.`if`, "'if'").and { (opener) =>
      term.and { (condition) =>
        take(Token.`then`, "'then'").and { (_) =>
          term.and { (success) =>
            take(Token.`else`, "'else'").and { (_) =>
              term.map { (failure) =>
                Syntax(TermTree.Conditional(condition, success, failure), opener.span.extendedToCover(failure.span))
              }
            }
          }
        }
      }
    }
```

`if x then y else z`

1) Récupère le premier token (`if`) qui est enregistré dans `opener`
2) term parse le x
3) le `.and` permet d'avancer dans l'expression
4) même étape que le 1)
5) le `(_)` dis que le token ou le résultat n'est pas important
6) same than 2)
7) ...
8) le `term.map` regroupe le tout
9) il faut adapter les term par rapport à ce dont on a besoin

```scala
private def simpleTerm(using Context): Result[Syntax[TermTree]] =
    peek.map((t) => t.tag) match
      case Some(Token.boolean) => booleanLiteral
      case Some(Token.integer) => integerLiteral
      case Some(Token.identifier) => termIdentifier
      case Some(Token.leftParenthesis) => lambdaOrParenthesized
      case Some(Token.`if`) => conditional
      case Some(Token.`let`) => binding
      case _ => throw expected("term")
```

/!\ NE PAS OUBLIER DE RAJOUTER LES CASES