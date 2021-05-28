[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![Release Tag](https://img.shields.io/github/v/release/jcs090218/setup-emacs-windows.svg)](https://github.com/jcs090218/setup-emacs-windows/releases/latest)
[![Build Status](https://github.com/jcs090218/setup-emacs-windows/workflows/CI/badge.svg)](https://github.com/jcs090218/setup-emacs-windows/actions)
[![dependencies Status](https://status.david-dm.org/gh/jcs090218/setup-emacs-windows.svg)](https://david-dm.org/jcs090218/setup-emacs-windows)

# Set up Emacs in Windows
> A Github Action that installs a specific emacs version

## Usage:

```yaml
uses: jcs090218/setup-emacs-windows@master
with:
  version: 24.5
```

The `emacs` executable on the path will then be the requested
version. For a list of available versions, please see the
[main GNU FTP server](https://ftp.gnu.org/gnu/emacs/windows/).

#### Note about compiling binary emacs modules

[Here's an example](https://github.com/xuchunyang/strptime.el) of a project which
compiles binary modules against an Emacs installed with this method.

## License

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
