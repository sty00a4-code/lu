# Types

Lu is a dynamically-typed programming language. That basically means a variable can be changed into any value even if it has a different type.

```
let x = 1
print(x)
x = "a string"
print(x)
```

Here a local variable `x` is created with the initial value of `1`, which is an integer number. After printing the number, `x` is reassigned to the string `"a string"` and printed out again leaving the output to be:

```
1
a string
```

You probably already understand this when comming from languages like Python, Lua or JavaScript. Now what wouldn't be valid code is the following:

```
let x = 1
let y = "abc"
print(x + y)
```

The first two lines are fine, but the last one will throw an error and exit out of execution. The run-time error you are greeted with is `cannot perform '+' on int with string`. What this is saying is that you are trying to add an integer number to a string, *duh*.
Although Lu is dynamically-typed, it does not convert types automatically at free will like JavaScript does (`"b" + "a" + +"a" + "a" == "baNaNa"`).

```
print(tostr(x) + y)
```

This code would be valid and it would print out `1abc`. This is because with the function `tostr`, we have converted the integer number `x` to a string and than added `y` to it. Performing the add operation on two strings will concatinate them into one in Lu.


*The following table shows all the types that exist in Lu*
| Name     | Example                                                  |
| -------- | -------------------------------------------------------- |
| null     | `null`                                                   |
| int      | `0`, `1`, ...                                            |
| float    | `0.5`, `1.333`, ...                                      |
| bool     | `true` / `false`                                         |
| string   | `"hello world"`, ...                                     |
| vector   | `[1, 2, 3]`, `["a", "b", null]`, ...                     |
| object   | `{ a = 1, b = 2 }`, `{ name = "lu", version = "0.1.0" }` |
| function | `function (n) { return n * 2 }`                          |

*[Next](/docs/types.md)*