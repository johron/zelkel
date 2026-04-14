# Zelkel
- Low-level object-oriented programming language

## TODO
- [ ] Implement cut in parser for all sub-parsers
- [ ] Other TODOs in the code

## Syntax example
```cpp
require std::mem;
require std::io;

class Animal {
  val name: s{128};

  static fn! new(name: s{128}) -> *Self {
    val animal: *Self = mem.alloc(mem.sizeof(Self));
    animal.name = name;
    return animal;
  }

  fn! speak(self: *Self) -> s{128} {
    return f"{self.name} makes a sound";
  }
}

static fn! main() {
  val dog: *Animal = Animal.new("Rex");
  val sound: s{128} = dog.speak();
  io::println(sound);
}
```