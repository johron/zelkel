# Zelkel

## TODO
- [ ] Values take the function declaration value_type in the ast for some reason
- [ ] Standard functions like println and print need to be thought and done about
    - Remove hard coded standard functions
- [ ] Parser must make sure expression value_type is same as the variable value_type
        - 1: doesnt check if given arguments to function are same type as the ones set in the function declarataion
- [ ] Convert strings to int, float
- [ ] String concatination
- [ ] Global variables, variables in top scope
- [ ] Tables, Lists
- [ ] Requiring other files, libraries: standard
    - Tokenize, parse, generate this zelkel file and combine them:
      maybe combine the zelkel code into one and compile that
    - no ending ("std"): a standard, bundled library, library in a main directory
    - with ending ("lib.zk"): user defined library, can have path and stuff
    
    - require "stdlib/io" as *;
      - println("hi");
    - require "stdlib/io" as io;
      - io.println("hi");
- [ ] Rewrite in itself
- [ ] Make sure every function has a return and it's value should be same type as function declaration return type

## License
Licensed under the MIT License; please see the [license file](LICENSE.md) for terms.
