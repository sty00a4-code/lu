# Lu

A light weight interpreted programming language wirtten in Rust

# Guide

## Tutorials

1. [Introduction](/docs/introduction.md)

## Standard Library

- [Global](/docs/std/global.md)
- [*obj*](/docs/std/obj.md)
- [*str*](/docs/std/str.md)
- [*vec*](/docs/std/vec.md)

# To-Do

## Features
- [x] path impl
- [x] binary and unary expressions
- [x] vector and object creation
- [x] most binary operators
- [x] control flow
  - [x] if
  - [x] while
- [x] function creation
- [x] error stack
- [x] optimizations
## Environment
- [x] function `setmeta(object, meta)`: set the meta information of an object
- [x] function `error(string)`: raise an error
- [x] module `vec`
  - len(): length of the vector
  - push(value): push on front
  - pop(idx): pop index of
  - insert(idx, value): insert into the vector at idx
  - pos(func): get postion of value
- [x] module `str`
  - len(): length of string
  - sub(start, end): create a substring from the defined range
  - char(byte): create a string of the character by the byte
  - byte(idx): get the byte if the string at idx's position
  - upper(s): puts the strings characters in upper case
  - lower(s): puts the strings characters in lower case
- [x] module `math`
  - floor(n)
  - ceil(n)
  - round(n)
  - abs(n)
  - cos(n)
  - sin(n)
  - tan(n)
  - acos(n)
  - asin(n)
  - atan(n)
  - atan2(a, b)
  - cosh(n)
  - sinh(n)
  - tanh(n)
- [x] module `fs`
  - read(path): read file at path and return content
  - write(path, content): write the content to the file at path
  - append(path, content): append the content to the file at path
- [x] module `os`
- [x] module `io`
## Use
- [ ] Install Guide
- [ ] Playground
- [ ] Tutorial