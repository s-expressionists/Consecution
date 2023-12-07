# Consecution: An implementation of the Common Lisp sequence functions.

This library contains a complete implementation of the Common Lisp
sequence functions.  The code can be used both intrinsically and
extrinsically.  By "intrinsically", we mean that it can be used for
the native implementation of the sequence functions in a Common Lisp
system that either does not have such an implementation, or that has
an implementation but that is deemed inferior to this one.  By
"extrinsically", we mean that it can be used in an existing Common
Lisp system without affecting the native implementation of the
sequence functions in that system.

## License

See [LICENSE.md](LICENSE.md).

## Testing

The sequence tests of [ansi-test][] can be run on [SBCL][] via the
following shell command. Consecution has not been tested on other Lisp
implementations yet.

```
sbcl --eval "(ql:quickload :consecution-extrinsic/test)" --eval "(asdf:test-system :consecution-extrinsic")
```

Following the first invocation of the tests the command may be
shortened to the following.

```
sbcl --eval "(asdf:test-system :consecution-extrinsic")
```

[ansi-test]: https://gitlab.common-lisp.net/ansi-test/ansi-test
[SBCL]: https://sbcl.org/
