# Spade Language Specification

## Syntax

### Comments

Comments start with `--`

```
-- This is a comment
```

### Number literals

Numbers are all base 10 and may have a radix seaparator.

Numeric separators are also allowed. Similarly to other languages, underscores can be used to separate digits for clarity of reading.
This has no functional effects.

```
x = 100_000
```

Underscores are only allowed .

### String literals

Strings literals start and end with the `'` character.
Double quotes are invalid.

### Statement Separation

Statements can either be separated using new lines or semi colons.
It is recommended that handwritten spade

### Identifiers

Identifiers in spade have the allow characters in regex form: `[a-zA-Z0-9_]`.
The exception to this rule is that numbers are not allowed to be the first character of an identifier.

## Types

## Statements
