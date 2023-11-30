# Documentation for the global scope

## Functions

### `print( ... )`

Prints the arguments to the console with a new line character at the end

### `input( prefix: string? ): string`

Collects user input through the terminal. `prefix` is optional, if given it will write the string before collecting user input

### `setmeta( o: object, meta: object? ): object`

Sets the meta object for `o` to `meta` if provided.

##### Meta-Fields

All of these fields do not have to be present in a meta object, they are only optionals.

- `__name: string`: The objects *name* when casting it to a string.
- `__type: string`: The custom *type* of the object.
- `__call: function(self: object)`: This function is called when trying to *call* the object.
- `__add: function(self: object, other)`: This function is called when trying to `+` the object with another value.
- `__sub: function(self: object, other)`: This function is called when trying to `-` the object with another value.
- `__mul: function(self: object, other)`: This function is called when trying to `*` the object with another value.
- `__div: function(self: object, other)`: This function is called when trying to `/` the object with another value.
- `__mod: function(self: object, other)`: This function is called when trying to `%` the object with another value.
- `__eq: function(self: object, other)`: This function is called when trying to `==` the object with another value.
- `__ne: function(self: object, other)`: This function is called when trying to `!=` the object with another value.
- `__lt: function(self: object, other)`: This function is called when trying to `<` the object with another value.
- `__le: function(self: object, other)`: This function is called when trying to `<=` the object with another value.
- `__gt: function(self: object, other)`: This function is called when trying to `>` the object with another value.
- `__ge: function(self: object, other)`: This function is called when trying to `>=` the object with another value.
- `__and: function(self: object, other)`: This function is called when trying to `&` the object with another value.
- `__or: function(self: object, other)`: This function is called when trying to `|` the object with another value.
- `__neg: function(self: object, other)`: This function is called when trying to `-` the object.
- `__not: function(self: object, other)`: This function is called when trying to `!` the object.

### `getmeta( o: object ): object?`

Gets the meta object of `o` if any is set

### `error( msg: string )`

Throws an error with the `msg` string

### `assert( cond: bool )`

Throws an error if `cond` is *false*

### `ok( value ): result`

Returns the `value` wrapped in a *result* with the `ok` variant

### `err( value ): result`

Returns the `value` wrapped in a *result* with the `err` variant

### `type( value ): string`

Returns the *type* of the `value` as a string

##### Types

- `null`
- `int`
- `float`
- `bool`
- `string`
- `vector`
- `object`
- `function`
- `result`

### `to_int( value ): int?`

Tries to cast the `value` to an `int` if possible

### `to_float( value ): float?`

Tries to cast the `value` to a `float` if possible

### `to_bool( value ): bool?`

Tries to cast the `value` to a `bool` if possible

### `to_str( ... ): string`

Casts the arguments to `string`s and concatinates them

### `to_vec( ... ): vector`

Collects the arguments into a `vector`

### `require( path: string )`

Loads module at the `path` and returns it and throws an *error* if there is no module at that `path`

### `obj`

[To module](/docs/std/obj.md)

### `str`

[To module](/docs/std/str.md)

### `vec`

[To module](/docs/std/vec.md)

### `math`

[To module](/docs/std/math.md)

### `os`

[To module](/docs/std/os.md)

### `fs`

[To module](/docs/std/fs.md)