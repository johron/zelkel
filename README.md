# Zelkel
- Object-oriented programming language targeting a [custom virtual machine](https://github.com/johron/zelkel-vm)

## Todo:
- [x] In the parser `parse()` function only parse for class definitions and require statements
- [ ] Classes
  - [x] Need to implement mutable variables so it can do `Self.value = value;`, etc.
    - [x] Added mutable variables, but not rest
  - [ ] Implement the `new` statement for instantiating classes, should automatically add the `Self` parameter, if the constructor only has one other argument you can construct it without new and stuff
  - [x] Need to implement the ability to have variables in classes so that you have to define value before you can do `Self.value = value;`,
        variables in classes can only be accessed with `Self` to not confuse them with local variables
  - [ ] Super keyword
- [ ] Make it possible to call and reference functions and classes that are defined after.

## How I want some stuff to work:
### Making strings or any other type/value
- Comment is what it basically does
```
class String {
  val value: __prim_str;
  fn _(Self, value: __prim_str) -> Self {
    Self.value = value;
  }
  
  fn trim(...) { ... }
}

// val x: String = new String("hi");
val x: String = "hi";
```

## License
Licensed under the MIT License; please see the [license file](LICENSE) for terms.
