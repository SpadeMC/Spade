# Spade Language

## Hello World

```
main() {
	say('Hello world')
}
```

## Comments

```
-- This is a line comment!
```

## Constants & Variables

All variables are either have the integer or boolean type.

```
$c = 'Constant value'

v = 3
```

## Conditionals

```
if world.block(32, 64, 12) == block.dirt {
	say("This block is dirt")
}
```

```
if 32 * 64 == 2 {

} else if true {

} else {

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
