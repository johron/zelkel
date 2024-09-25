# Zelkel

## TODO
- [ ] Rewrite in itself
- [ ] Finish parser
- [ ] Make llvm emitter, codegen
- [ ] Fix this:
```zk
fn main(): void {
    const x = 1 + 2 + 3;

};
test.zk:3: Unexpected token found while parsing statement: '}'
```
```
fn main(): void {
    const x = 1 + 2 + 3;
};
This works
```
-[ ] Fix line number not being counted correctly, skipping