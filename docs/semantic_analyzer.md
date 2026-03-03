# Rules in the Semantic Analyzer
1. No Boolean Arithmetic (e.g. Int + Bool, Float + Bool, Bool + Bool)
2. No implicit conversions between data types (float (+ | - | / | * | =) int is disallowed)
3. Can compare 2 booleans (e.g. Bool < Bool)
4. Single Pass. So all functions must be declared *before* use. This means the following is not allowed:
```
func foo(): i32{
    let baz = bar() + 69;
    return baz;
}
func bar(): i32{
    return 420;
}
```
5. 