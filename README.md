# elkee
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![CI][ci-badge]][ci-workflow]
[![Coverage Status][cover-badge]][cover-link]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

Native KeePass client for Emacs

Elkee, unlike other KeePass clients, strives for requiring zero third-party
binaries, and working wherever Emacs works due to being written in ELisp. No
C#, no C++, no linking against libs, no shell exec, just pure ELisp. Working
Emacs (and optionally a good package manager) should be the only thing needed.

It's not there yet. For the first clean implementation it needs three critical
parts: ChaCha20 (RFC7539), Blake2 (RFC7693) and Argon2 (RFC9106).  Currently
only Argon2 remains and its implementation is already in progress, soon
replacing the last requirement for the binary.

Fortunately, argon2 binary is present on all large platforms Emacs is available
on including Android (via Termux and/or copy-paste out of it), so the only
requirement is to have it on $PATH.

The targeted container is KeePass 2.x KDBX file, that means:

| KeePass | KDBX |
|---------|------|
| 2.00    | 1.0  |
| 2.07    | 1.1  |
| 2.08    | 1.2  |
| 2.09    | 2.0  |
| 2.11    | 2.4  |
| 2.15    | 3.0  |
| 2.20    | 3.1  |
| 2.35    | 4.0  |
| 2.48    | 4.1  |

of which KDBX 4.x is implemented at the moment. Writing to the KDBX file is
currently not supported and is of a lesser priority than supporting the reading
of other KDBX versions due to other means available for Windows, Linux, MacOS
and Android including various synchronization methods across machines where
KDBX editing is faster and more comfortable.

Out of all KDBX 4.x algorithms available, only the chain ChaCha20-Blake2-Argon2
is supported at the moment, but contributions are welcome.

TODO:
- [x] Argon2, soon, fallback to `argon2` binary currently
- [ ] Other non-default algorithms available in KDBX4
- [ ] KDBX3

## How to

Install it from [Melpa](https://melpa.org/#/getting-started) or clone and
install manually, then:

1. Ensure [Kaesar][kaesar] Emacs library is installed
2. `(require 'elkee)`
3. `(elkee-find-creds "file.kdbx" "password" "keyfile-or-nil" :title "Item")`

For more visit function documentation for:
- `elkee-find-creds`
- `elkee-list-creds`

or use other public funcs for reading the data manually.

[melpa-badge]: http://melpa.org/packages/elkee-badge.svg
[melpa-package]: http://melpa.org/#/elkee
[melpa-stable-badge]: http://stable.melpa.org/packages/elkee-badge.svg
[melpa-stable-package]: http://stable.melpa.org/#/elkee
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
