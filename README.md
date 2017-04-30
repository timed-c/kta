
# KTA 

KTA (KTH's Timing Analyzer) is a tool for static analysis of C and machine code. Currently, only the MIPS32 ISA is supported. The current version of KTA can perform two types of analysis:

* **Interactive Timing Analysis:** The tool takes as input a `.ta` timing analysis file that contains information about what kind of analysis that should be done. The interactive timing analysis is intended to be used together with the [KIELER project](http://www.rtsys.informatik.uni-kiel.de/en/research/kieler/). The interactive timing analysis is experimental and performs an exhaustive search. For more information, see the following [paper](https://people.kth.se/~dbro/papers/fuhrmann-et-al-2016-time-for-reactive-modeling.pdf).  

* **Abstract Search-based WCET Analysis:** This analysis is under development. The goal is to provide efficient optimal WCET analysis for simple cache-based RISC processors. 

## Installation

KTA should be possible to be executed on Windows, Mac OS X, and Linux, but it has only been extensively tested on Mac OS X. To install the program, perform the following:

1. Install the latest version of the OCaml compiler. See  [ocaml.org](https://ocaml.org/docs/install.html) for installation instructions. 

2. Install the [MCB32 tool chain](https://github.com/is1200-example-projects/mcb32tools/releases/). Follow the instruction on the link, but do not test the environment according to the instructions since it requires the use of a PIC32 hardware platform.

3. Launch the MCB32 command line terminal (see the link in step 2). If you have started the terminal correctly, the command line should show `[mcb32]`. 

4. Clone this repository: `git clone https://github.com/timed-c/kta.git`

5. From the root path, run: `make`. An executable named `kta` should now be available in the directory `/bin`. Add this directory to your path environment.

## Usage

* Please see the following [getting started guide](doc/gettingstarted.md) for an informal introduction on how to use the tool.

* Please see the [design overview](doc/design.md) document for a brief overview of the design and implementation of KTA.

## Benchmarks

### MDH Benchmarks
The [MDH benchmarks](http://www.mrtc.mdh.se/projects/wcet/benchmarks.html) are included in the test/wcet\_tests/mdh folder.

### TACLe Benchmarks
The [TACLe Benchmark Suite](http://www.tacle.eu/index.php/activities/taclebench) is available at the git repository: [tacle-bench.git](https://github.com/tacle/tacle-bench.git). In the current version of KTA, git commit fcabf4630cb239f34f37a03ce7d93c563b65c897 is used.

## MIT License 
KTA is licensed under the MIT license.

Copyright 2013-2017 David Broman

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND the. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


