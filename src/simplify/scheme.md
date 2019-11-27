# Target operations

This symplify part must have a clearly defined set of supported operations:

```python
ordering_of_classes = [
    # singleton numbers
    'Zero', 'One', 'NegativeOne', 'NaN', # Number, Singleton
    # numbers
    'Integer', # Rational
    'Rational', # Number
    'Float', # Number
    # singleton symbols
    'Exp1', 'Pi', # Number, Singleton, NumberSymbol
    # symbols
    'Symbol', # 
    # arithmetic operations
    'Pow', 'Mul', 'Add', # Expr, AssocOp
    # defined singleton functions
    'Abs', 'Sqrt',
    'Exp', 'Log',
    'Sin', 'Cos',  # Function
]
```

class relationships:
Basic --> Expr --> Function;
            |
      --> Atom --> AtomicExpr --> Number 

Do not consider complex number for now. 

Relies on Owl_symbolic_operator and infix 

Evalutation happens on the fly... 