# Pipe operator

See `magrittr::%>%` for details.

## Uso

``` r
lhs %>% rhs
```

## Argumentos

- lhs:

  A value or the magrittr placeholder.

- rhs:

  A function call using the magrittr semantics.

## Valor

The result of calling `rhs(lhs)`.
