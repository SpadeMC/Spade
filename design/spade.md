# Spade Language

## Hello World

```
main() {
	say('Hello world')
}
```

The top level of a file only contains constants, function definitions and

## Comments

```
-- This is a line comment!
```

## Constants & Variables

All variables are either have the integer or boolean type.

```
c = 'Constant value'

v = 3
```

## Types

### **Primitives**

| Type | Example |
| --- | --- |
| Boolean | `false`, `true` |
| Byte | `255` |
| Short | `1000` |
| Int | `124232` |
| Long | `1012353` |
| Float | `15.32` |
| Double | `12.54` |
| String | `'Hello world'` |

### Data Structures

| Type | Example |
| --- | --- |
| List | `[1,5,6]` |
| Map | `{foo: bar}` |

### Minecraft Types

| Type | Example |
| --- | --- |
| Range | `[..5]` |

## Conditionals

```
if world.block(32, 64, 12) == block.dirt {
	say('This block is dirt')
}
```

```
if 32 * 64 == 2 {

} else if true {

} else {

}
```

Case statements let you perform different actions based on the value of one expression.

```
case x {
	0: {
		say('x is 0')
	}
	1: {
		say('x is 1')
	}
	default: {
		say('x is not 0 or 1')
	}
}
```

## Loops

```
for x in [1...10] {
	say('This gets printed 10 times!')
}
```

```
i = 0
while i<10 {
	say('This gets printed 10 times!')
	i = i + 1
}
```

Use repeat for even shorter loops without counting

```
repeat 10 {
	say('This gets printed 10 times!')
}
```

## Functions

```
#add(x: Int, y: Int): Int {
	return x + y
}
```

## Lists

Similar to arrays from JSON

```
l = [1,6,8]

l2 = [{ id: 0 }, { bar: 'foo' }]
```

### List Constructor Shorthand

```
-- Make a list from with all the numbers from 1 to 10 inclusive
[1...10]
```

> Note: Make sure not to mix this up with two dots (`[1..10]`) which defines a range from 0 to 10, used for target selectors.

## Maps

Similar to objects from JSON

```
m = {
	foo: 'bar'
}
```

## Sequences

```
|> 20 {
	say('This gets printed after 20 ticks!')
}
```

```
|> 20 {
	-- ...
} 20 {
	say('This gets printed 20 ticks after the previous 20 ticks!')
}
```

## NBT Moves

```
-- WIP
some.path.here[0] <- {
	some: 'asdf'
}
```

## Commands

For functionality not available in Spade's API, you can always use plain commands by starting lines with `/`.

```
-- Runs any command with /
/tp @p 0 100 0
```

### Inserting preprocessed values

You can insert preprocessed values into commands.

```
a = 64
b = 64
c = 128

/tp @p (a) (b) (c)
```
