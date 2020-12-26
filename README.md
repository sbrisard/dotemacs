# dotemacs

My emacs configuration files

## Variables that must be custom-set

- `ediff-diff-program`
- `ediff-diff3-program`
- `epg-gpg-program`
- `lsp-clients-clangd-executable`
- `maxima-command`
- `url-proxy-services`
- `sb-maxima-mode-path'
- `TeX-view-program-list`
- `TeX-view-program-selection`

## lsp-mode

### C++

Install [https://releases.llvm.org/download.html](clangd).

## Sending emails with Emacs

- Define a `~\.authinfo` file (see
  https://www.gnu.org/software/emacs/manual/html_node/smtpmail/Authentication.html).
- Change the location of this file with the `auth-sources` variable.
- Encrypt this file! (`~/.authinfo` → `~/.authinfo.gpg`). Alternatively, do not
  define the `password` entry (you will then be prompted for a password for each
  new email).
- Use `mail-mode` to write and send emails (`M-x describe-mode`).

<!-- Local Variables: -->
<!-- fill-column: 80 -->
<!-- End: -->
