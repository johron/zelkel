# Zelkel

## TODO
- [ ] Rewrite in itself
- [ ] Finish parser
- [ ] Make llvm emitter, codege
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