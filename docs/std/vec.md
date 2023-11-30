# Documentation for module *vec*

This module can be self-called on the `vector` type

## Functions

### `vec.copy( v: vector ): vector`

Returns a copy of `v`

### `vec.get( v: vector, index: int, default? )`

Returns the value of `v` at `index`, if `index` is out of range `default` is returned

### `vec.len( v: vector ): int`

Returns the length of `v`

### `vec.push( v: vector, value )`

Pushes the `value` on to `v`

### `vec.pop( v: vector )`

Pops the last value off of `v` and returns it

### `vec.remove( v: vector, index: int )`

Removes the value of `v` at `index` and returns it

### `vec.pos( v: vector, value ): int?`

Returns the index of `value` in `v` if it exists

### `vec.range( start: int, end: int ): vector`

Returns a vector of integers from `start` to `end` exclusively

### `vec.join( v: vector, sep: string ): string`

Returns `v`'s values as strings joined by `sep`

### `vec.sort( v: vector )`

Sorts `v` if possible (speed of *O(n \* log(n))*)

### `vec.contains( v: vector, value ): bool`

Returns true if `value` is in `v`