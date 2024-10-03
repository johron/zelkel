# Zelkel

## TODO
- [ ] Standard functions like println and print need to be thought and done about
    - Remove hard coded standard functions
    - Do not have string() function hard coded. have it in std library and then use sprintf for it in th
- [ ] Parser doesnt like to have big expressions with parentheses in function call arguments. Doesnt like parenthesese in args
- [ ] Parser must make sure expression value_type is same as the variable value_type
        - 1: doesnt check if given arguments to function are same type as the ones set in the function declarataion
- [ ] Convert strings to int, float
- [ ] String concatination
- [ ] If statements
- [ ] While loops
- [ ] Global variables, variables in top scope
- [ ] Tables, Lists
- [ ] Requiring other files, libraries: standard
    - Tokenize, parse, generate this zelkel file and combine them:
      maybe combine the zelkel code into one and compile that
    - no ending ("std"): a standard, bundled library, library in a main directory
    - with ending ("lib.zk"): user defined library, can have path and stuff
- [ ] Rewrite parser since it is very buggy.
- [ ] Rewrite in itself
- [ ] Make sure every function has a return and it's value should be same type as function declaration return type