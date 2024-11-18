# Zelkel
LLVM-IR targeted programming language for computers

## TODO
- [ ] Maybe move fstrings to the preprocessor to be able to have `println(f"{}", func(a, b));` so it adds `%i`, `%s`, `%f`, ... automatically after the type which is a LOT easier in the preprocessor stange, i think? 
- [ ] Standard functions like println and print need to be thought and done about
    - Remove hard coded standard functions
- [ ] Convert strings to int, float
- [ ] String concatination
- [ ] Global variables, variables in top scope
- [ ] Tables, Lists
- [/] Requiring other files, libraries: standard
    - Tokenize, parse, generate this zelkel file and combine them:
      maybe combine the zelkel code into one and compile that
    - no ending ("std"): a standard, bundled library, library in a main directory
    - with ending ("lib.zk"): user defined library, can have path and stuff
    
    - require "stdlib/io" as *;
      - println("hi");
    - require "stdlib/io" as io;
      - io.println("hi");
- [ ] Rewrite in itself
- [ ] Fix problems with TODO comment

## Docs
### Functions
```zelkel
fn greet(name: string): void {
    print("Hello, %s!\n", name);
}
```
- The `print` function may be renamed or something down the line
- Programs require a main function

### Variables
```zelkel
fn main(): void {
    let mutable: int = 5;
    const immutable: int = 10;
}
```

### Requiring files
```zelkel
require "library";
```
- The require strings do not contain the file ending and automatically append `.zk` to the end

## License
Licensed under the MIT License; please see the [license file](LICENSE.md) for terms.
