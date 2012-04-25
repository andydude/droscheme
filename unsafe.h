/*
 * Droscheme - a Scheme implementation
 * Copyright © 2012 Andrew Robbins, Daniel Connelly
 *
 * This program is free software: it is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
 */
#include <runtime.h>

typedef struct cont_s cont_t;

struct cont_s {
    Slice st; // stack
    Slice gr; // goroutine
};

// for memory
void* droscheme·malloc(uintptr size);
void  droscheme·free(void* ptr);

// for bytes (no GC)
Slice droscheme·makeBytes(byte* p, int32 n);
void  droscheme·freeBytes(Slice sl);
void  droscheme·dumpBytes(Slice s);
void  droscheme·dumpHex(byte b);

// for continuations
cont_t* droscheme·makeCont(Slice st, Slice gr);
void    droscheme·GetCC(byte* dummy, cont_t* cont);
void    droscheme·SetCC(byte* dummy, cont_t* cont);
void    droscheme·freeCont(cont_t* cont);
void    droscheme·dumpCont(cont_t* cont);

// for debugging
void droscheme·dumpdefer(Defer* p);
void droscheme·dumppanic(Panic* p);
void droscheme·dumpsched(Gobuf* p);
void droscheme·dumpg(G* g);
void droscheme·dumpm(M* m);
void droscheme·DumpInternals();
