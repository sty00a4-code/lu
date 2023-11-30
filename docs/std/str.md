# Documentation for module *str*

This module can be self-called on the `string` type

## Functions

### `str.get( s: string, index: int, default: string? ): string?`

Returns the sub-string of `s` at the `index`, if the `index` is out of range `default` will be returned

### `str.len( s: string ): int`

Returns the length of `s`

### `str.char( byte: int ): string?`

Converts the `byte` to it's character version if it is between 0-255

### `str.upper( s: string ): string`

Converts all the letters in `s` to uppercase

### `str.lower( s: string ): string`

Converts all the letters in `s` to lowercase

### `str.split( s: string, sep: string ): vector`

Splits `s` on `sep` and collects the sub-strings in a vector

### `str.is_digit( s: string ): bool`

Returns *true* if the character at index 0 is a *digit*

### `str.is_radix( s: string, radix: int ): bool`

Returns *true* if the character of `s` at index 0 is a digit of the `radix` provided

### `str.is_alphabetic( s: string ): bool`

Returns *true* if the character of `s` at index 0 is an *alphabetic* character

### `str.is_alphanumeric( s: string ): bool`

Returns *true* if the character of `s` at index 0 is an *alphanumeric* character

### `str.is_whitespace( s: string ): bool`

Returns *true* if the character of `s` at index 0 is *whitespace*

### `str.is_lower( s: string ): bool`

Returns *true* if the characters in `s` are all *lowercase*

### `str.is_upper( s: string ): bool`

Returns *true* if the characters in `s` are all *uppercase*

### `str.is_punctuation( s: string ): bool`

Returns *true* if the character of `s` at index 0 is *punctuation*

### `str.format( s: string, ... ): string`

Returns a string from `s` formatted with the arguments

##### Format options

- `$s`: into string
- `$q`: quoted