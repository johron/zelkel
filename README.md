# Zelkel

## TODO
- [ ] Standard functions like println and print need to be thought and done about
    - Remove hard coded standard functions
    - Do not have string() function hard coded. have it in std library and then use sprintf for it in th
- [ ] Parser must make sure expression value_type is same as the variable value_type
        - 1: doesnt check if given arguments to function are same type as the ones set in the function declarataion
- [ ] Convert strings to int, float
- [ ] Some type conversion for: "2.5 * 5" to be converted to "2.5 * 5.0" for some usability, "5/2" should be a valid float for a variable
- [ ] String concatination
- [ ] While loops
    - The mutable variable counter has to be updated for every iteration of the loop which i am not sure how i can do
      since the counter things are done in the parser and not in codegen. So maybe this has to be moved?
    - This is actually a problem with mutable variables, I am kind of stuck on this
- [ ] Global variables, variables in top scope
- [ ] Tables, Lists
- [ ] Requiring other files, libraries: standard
    - Tokenize, parse, generate this zelkel file and combine them:
      maybe combine the zelkel code into one and compile that
    - no ending ("std"): a standard, bundled library, library in a main directory
    - with ending ("lib.zk"): user defined library, can have path and stuff
- [ ] Rewrite in itself
- [ ] Make sure every function has a return and it's value should be same type as function declaration return type