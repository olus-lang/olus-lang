Compiler:

* Parser
  * [x] Parse Syntax to AST
  * [x] Desugar
  * [x] Bind variables
  * [ ] Bind parameters only to following block (if any)
  * [ ] Allow forward looking bindings
  * [x] Extract constants (replace by reference)
  * [x] Extract intrinsics
  * [x] Extract Program
  * [ ] Store symbol names and sugar-type
  * [ ] Store source locations

* Bytecode files
  * [x] Renumber references consecutively
  * [x] Store Program in binary `.olus.bin`
  * [x] Cache compiled source files
  * [ ] Debug info in auxiliary file  `.olus.debug.bin`
  * [ ] All symbol names in debug file
  * [ ] Source file reference in debug file
  * [ ] Allow compression using `.olus.bin.gz` and `.olus.debug.bin.gz`

* Compiler passes
  * [x] Remove dead code
  * [ ] Deduplicate declarations
  * [ ] Closure analysis
  * [ ] Extract constant closures
  * [ ] Inline constant closures
  * [ ] Converting arguments to closure (for register pressure)
  * [ ] Converting closure to arguments (for register promotion)
  * [ ] Statically allocate singleton closures (TODO: thread local)

* Type system
  * [ ] Simple type system
  * [ ] Support/resolve cyclical types
  * [ ] Pure Type System
  * [ ] Unique types
  * [ ] Singleton closures (at most one exists, can static allocate)

* Builtins
  * [ ] Basic exit, print, read
  * [ ] Linux Syscall style open, read, write, stat, etc.
  * [ ] Sockets
  * [ ] Threads
  * [ ] Mmap

* Execution
  * [ ] Interpreter
  * [ ] x64 assembler
  * [ ] Direct jumps for known closures
  * [ ] WebAssembly

* Runtime memory management
  * [ ] Simple bump allocator
  * [ ] Optimal compacting storage with only back-references?
  * [ ] RC based memory management
  * [ ] Static allocating singletons
  * [ ] Storage re-use patterns

Website:

* Look at Wren's it is very nice. https://github.com/munificent/wren


Reading list:

* [ ] https://groups.google.com/forum/#!topic/homotopytypetheory/WhMsxFlek5I
* [ ] https://wenkokke.github.io/sf/
* [ ] https://www.cs.cmu.edu/~rwh/pfpl/2nded.pdf
* [ ] https://en.wikipedia.org/wiki/Nuprl
* [ ] https://news.ycombinator.com/item?id=16996335
