# Zelkel

## TODO
- [ ] Function declaration args do not have correct code generated for them since mutable variable updates. Something to do with types
- [ ] Remove variable counter since no longer used
- [ ] Standard functions like println and print need to be thought and done about
    - Remove hard coded standard functions
    - Do not have string() function hard coded. have it in std library and then use sprintf for it in th
- [ ] Parser must make sure expression value_type is same as the variable value_type
        - 1: doesnt check if given arguments to function are same type as the ones set in the function declarataion
- [ ] Convert strings to int, float
- [ ] Some type conversion for: "2.5 * 5" to be converted to "2.5 * 5.0" for some usability, "5/2" should be a valid float for a variable
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