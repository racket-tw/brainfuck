# compare imperative and functional style

The key point of this dir was figuring out how functional style be optimized. Thanks @sleepnova for providing this example.

### Benchmark

The benchmarking simply rely on `time` function, using:

```sh
# functional style
PLT_CS_COMPILE_LIMIT=100000 racket ./functional.rkt ../mandel.b
# imperative style
racket ./imperative.rkt ../mandel.b
```
