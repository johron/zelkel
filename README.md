# Zelkel
- Object-oriented programming language targeting a [custom virtual machine](https://github.com/johron/zelkel-vm)

## Todo:
- [ ] Get back to where I was before parser rewrite
-   [ ] Think about how I'm gonna have all the types. Since it would be very inefficient to have to copy the String class code for each string, remove unused, convert to primitve if possible, research how other OOP langs do it.
- [ ] Codegen

## How I want some stuff to work:
### Making strings or any other type/value
- Comment at the end is what it basically does. Auto-construction
```kotlin
class! String {
  val value: _s; // "_s" is a primitive string type
  fn! _(value: _s) {
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
