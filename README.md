# ISO 3166-2:CA Province/Territory Codes

This is a Haskell package that provides a data type for Canadian ISO 3166-2
Province and Territory codes, as well as functions for converting codes into
their English subdivision names.


## Building

Use `stack` to build this for local development:

```sh
stack build --pedantic --test --haddock --file-watch 
```


## Prior Art

The API for this package is based off of the
[state-codes](https://hackage.haskell.org/package/state-codes) and
[iso3166-country-codes](https://hackage.haskell.org/package/iso3166-country-codes)
packages.


## License

BSD-3, exceptions available.
