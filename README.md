# Zelkel
- Object-oriented programming language targeting a [custom virtual machine](https://github.com/johron/zelkel-vm)

## Todo:
- [ ] In the parser `parse()` function only parse for class definitions and require statements
- [ ] Constructors
  - [ ] Implement classes as types.
  - [ ] Implement the `new` statement for instantiating classes, should automatically add the `Self` parameter

## How I want some stuff to work:
### Making strings or any other type/valu
- Comment is what it basically does
```
class String {
  fn _(Self, value: __prim_str): Self {
    Self.value = value;
  }
  
  fn trim(...) { ... }
}

// val x: String = new String("hi");
val x: String = "hi";
```

## License
Licensed under the MIT License; please see the [license file](LICENSE) for terms.
