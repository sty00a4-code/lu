# Lu

A light weight interpreted programming language wirtten in Rust

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
- [ ] optimizations
## Environment
- [x] function `setmeta(object, meta)`: set the meta information of an object
- [x] function `error(string)`: raise an error
- [x] module `vec`
  - len(): 
  - push(value): push on front
  - pop(idx = vec.len()): pop index of
  - insert(idx, value): insert into the vector at idx
  - pos(func): get postion of value
- [ ] module `math`
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
- [ ] module `fs`
  - read(path): read file at path and return content
  - write(path, content): write the content to the file at path
  - append(path, content): append the content to the file at path
- [ ] module `io`
  - read(mode): read user input from stdin
  - write(content): write the content to stdout
- [ ] module `os`
- [ ] module `time`
- [ ] module `socket`