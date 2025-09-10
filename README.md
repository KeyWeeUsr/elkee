# elkee
[![CI][ci-badge]][ci-workflow]
[![Coverage Status][cover-badge]][cover-link]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

Native KeePass client for Emacs.

TODO:
- [x] Argon2, soon, fallback to `argon2` binary currently
- [ ] Other non-default algorithms available in KDBX4
- [ ] KDBX3

## How to

Clone and install manually, then:

1. Ensure [Kaesar][kaesar] Emacs library is installed
2. `(require 'elkee)`
3. `(elkee-find-creds "file.kdbx" "password" "keyfile-or-nil" :title "Item")`

For more visit function documentation for:
- `elkee-find-creds`
- `elkee-list-creds`

or use other public funcs for reading the data manually.

[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
[ci-badge]: https://github.com/KeyWeeUsr/elkee/actions/workflows/test.yml/badge.svg
[ci-workflow]: https://github.com/KeyWeeUsr/elkee/actions/workflows/test.yml
[cover-badge]: https://coveralls.io/repos/github/KeyWeeUsr/elkee/badge.svg?branch=master
[cover-link]: https://coveralls.io/github/KeyWeeUsr/elkee?branch=master
[kaesar]: https://melpa.org/#/kaesar
