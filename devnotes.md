# Parse_Args

The following are some notes that may be useful for development.

## Coverage testing with `gcov`

To check the coverage achieved by the test suite, build with:

```
gprbuild -Pparse_args_tests.gpr -j0 -s -g -cargs -fprofile-arcs -ftest-coverage -largs --coverage
```

Coverage can then be checked with a command such as `gcov -o obj/ src/parse_args.adb`.

## Memory leak testing with `gnatmem`

Memory leaks can be checked for by using `-g -O0 -largs -lgmem` when building. The test and example programs will generate `gmem.out` files which can be analysed with the gnatmem tool. 

This should not produce much output, given that the package uses `Ada.Finalization.Limited_Controlled` types which should prevent leaks. To show more information on memory allocations and deallocations, use a command line such as `gnatmem -b 1 -m 0 generic_example` to show more information on the non-leaking parts of the library.
