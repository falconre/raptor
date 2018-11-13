raptor is a higher-level il over falcon il

## Unsoundness

### Unknown calls and trashed variables

When raptor detects a call, it sets the trashed variables according to the calling convention. This allows the constants-propagation analysis to propagate certain variables across the call, which is required for recovering future calls. 

Imagine in MIPS we save the `$gp` register on the stack. To determine the locations of functions, we load `$gp` from the stack, add an offset to it, and load a function pointer from that address into `$t9`. We then branch to `$t9`. If we have several calls in a loop, and set all variables to Top after unknown function calls, the stack variable from which `$gp` is read will become `Top`, and we will fail to further propagate constants and resolve function calls.

### Certain pre-defined functions

Some functions, such as printf/sprintf, take a variable number of arguments. In these cases, 8 arguments are assumed. If more arguments are passed to these functions, this would be a source of unsoundness.