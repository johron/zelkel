# Zelkel
- Object-oriented programming language targeting a [custom virtual machine](https://github.com/johron/zelkel-vm)

## Todo:
- [] Parser
  - [x] Class instantiation with constructors with only one argument and the class as the variable type: `val x: String = new String("hi");` becomes `val x: String = "hi";`
    - [ ] Converting between the primitive type and the class type
  - [ ] Fix member reassignment bugs. Ast does not know which variable the class is from.
  - [ ] Recursive members
  - [ ] If statements, while loops, for loops, etc.
  - [ ] Arrays
  - [ ] Enums
  - [ ] Variadic arguments in functions
  - [ ] Think about how I'm going to have all the types. Since it would be very inefficient to have to copy the String class code for each string, remove unused, convert to primitve if possible, research how other OOP langs do it.
- [ ] Codegen
  - [ ] Rewrite the virtual machine to work for object-oriented programming and to refresh my memory on it. Implement arrays, I want these in the VM as primitives.
- [ ] Make a standard library
- [ ] Self-hosting compiler

## How I want some stuff to work:
### Making strings or any other type/value
- Comment at the end is what it basically does. Auto-construction
```kotlin
class! String {
  val value: _pstr; // "_pstr" is a primitive string type
  fn! _(value: _pstr) {
    this.value = value;
  }
  
  fn! trim(...) { ... }
}

// val x: String = new String("hi");
val x: String = "hi";
```
### Complete syntax
```kotlin
class Animal {
  val! name: s;

  fn! _(name: s) {
    this.name = name;
  }

  fn! speak() -> s {
    return f"{name} makes a sound";
  }
}

class! Root {
  fn! main() {
    val dog: Animal = new Animal("Rex");
    val sound: s = dog.speak();
    println(sound);
  }
}
```

## License
Licensed under the MIT License; please see the [license file](LICENSE) for terms.
