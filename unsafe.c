/*
 * Droscheme - a Scheme implementation
 * Copyright © 2012 Andrew Robbins, Daniel Connelly
 *
 * This program is free software: it is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. You can redistribute it and/or modify it under the
 * terms of the GNU Lesser General Public License (LGPLv3): <http://www.gnu.org/licenses/>.
 */
#include "unsafe.h"

void*
droscheme·malloc(uintptr size) {
    //runtime·mallocgc(size, 1, 1, 0);
    return runtime·malloc(size);
}

void
droscheme·free(void* p) {
    runtime·free(p);
}

Slice
droscheme·makeBytes(byte* p, int32 n) {
    Slice sl;
    sl.array = droscheme·malloc(n);
    sl.len = n;
    sl.cap = n;
    runtime·memmove(sl.array, p, n);
    return sl;
}

void
droscheme·freeBytes(Slice sl) {
    droscheme·free(sl.array);
}

void
droscheme·dumpHex(byte b) {
    static int8 *dig = "0123456789ABCDEF";
    int8 buf[3];

    buf[0] = dig[b / 16];
    buf[1] = dig[b % 16];
    buf[2] = '\0';

    runtime·prints(buf);
}

void
droscheme·dumpBytes(Slice s) {
    byte* start = s.array - 1;
    int32 i;
    for(i = 0; i < s.len; i++) {
        droscheme·dumpHex(start[i]);
        //if ((uintptr)&start[i] % 4 == 0) {
        if (i % 4 == 0) {
            runtime·prints(" ");
        }
        //if ((uintptr)&start[i] % 32 == 0) {
        if (i % 32 == 0) {
            runtime·prints("\n");
        }
    }
}

cont_t*
droscheme·makeCont(Slice st, Slice gr) {
    cont_t* cont = droscheme·malloc(sizeof(cont_t));
    cont->st = st;
    cont->gr = gr;
    return cont;
}

void
droscheme·freeCont(cont_t* cont) {
    droscheme·freeBytes(cont->st);
    droscheme·freeBytes(cont->gr);
    droscheme·free(cont);
}

void
droscheme·dumpCont(cont_t* cont) {
    runtime·printf("cont=%p\n", cont);
    //droscheme·dumpBytes(cont->st);
    droscheme·dumpg((G*)cont->gr.array);
}

cont_t*
droscheme·saveg(G* gp) {
    byte* sg = gp->stackguard;
    byte* sb = gp->stackbase;

    return droscheme·makeCont(droscheme·makeBytes(sg, sb - sg),
                              droscheme·makeBytes((byte*)gp, sizeof(G)));
}

void
droscheme·GetCC(byte* dummy, cont_t* cont) {
    G* gp = g; // runtime·allg
    //droscheme·dumpg(gp);
    //runtime·gosave(&gp->sched);
    //gp->sched.pc = runtime·getcallerpc(dummy);

    runtime·prints("continuation·Get()\n");
    droscheme·dumpg(gp);

    //droscheme·dumpg((G*)gr.array);
    cont = droscheme·saveg(gp);
    //droscheme·dumpg((G*)cont->gr.array);
    droscheme·dumpCont(cont);

    FLUSH(&cont);
    USED(dummy);
}

void
droscheme·SetCC(byte* dummy, cont_t* cont) {
    G* gp = g; // runtime·allg

    runtime·prints("continuation·Set()\n");
    droscheme·dumpCont(cont);
    //droscheme·dumpg(fp);
    //droscheme·dumpg(gp);
    runtime·gosave(&gp->sched);
    droscheme·dumpg(gp);
    runtime·memmove((byte*)gp, cont->gr.array, cont->gr.len);
    runtime·memmove(gp->stackguard, cont->st.array, cont->st.len);
    droscheme·dumpg(gp);

    runtime·setcallerpc(dummy, gp->sched.pc);
    //runtime·gogo(&gp->sched, 0);

    USED(dummy);
}
/*
void
droscheme·contcapture(byte* dummy, Eface proc, String prompt) {
    G* gp = g;//runtime·allg
    byte* sg = gp->stackguard;
    byte* sb = gp->stackbase;
    byte* sp = gp->sched.sp;
    byte* pc = gp->sched.pc;
    Slice stack = runtime·gobytes(sb, sb - sg);
    void *cont = droscheme·contmake(stack, gp->gopc);
}

void // never returns
droscheme·contreturn(byte *dummy, Eface cont, Slice values) {
}
*/

/*
void
droscheme·dumpstack(byte *dummy, int32 n) {
    runtime·prints("droscheme·dumpstack()\n");
    runtime·dump(dummy, n);
}

Slice
droscheme·getstack(byte *dummy, int32 n) {
    runtime·prints("droscheme·getstack()\n");
    return runtime·gobytes(dummy, n);
}
*/

void
droscheme·dumpdefer(Defer* p) {
    if (p == nil) {
        runtime·prints("nil,\n");
        return;
    }
    runtime·printf("/* %p */ &Defer{\n", p);
    runtime·printf("\t\tsiz: %d,\n", p->siz);
    runtime·printf("\t\tnofree: %d,\n", p->nofree);
    runtime·printf("\t\targp: %p,\n", p->argp);
    runtime·printf("\t\tpc: %p,\n", p->pc);
    runtime·printf("\t\tfn: %p,\n", p->fn);
    runtime·printf("\t\tlink: %p,\n", p->link);
    //runtime·printf("\t\targs: %p,\n", p->args);
    runtime·prints("\t},\n");
}
void
droscheme·dumppanic(Panic* p) {
    if (p == nil) {
        runtime·prints("nil,\n");
        return;
    }
    runtime·printf("/* %p */ &Panic{\n", p);
    runtime·prints("\t\targ: ");
    runtime·printany(p->arg);
    runtime·prints(",\n");
    runtime·printf("\t\tstackbase: %p,\n", p->stackbase);
    runtime·printf("\t\tlink: %p,\n", p->link);
    runtime·printf("\t\trecovered: %d,\n", p->recovered);
    runtime·prints("\t},\n");
}
void
droscheme·dumpsched(Gobuf* p) {
    if (p == nil) {
        runtime·prints("nil,\n");
        return;
    }
    runtime·printf("/* %p */ Gobuf{\n", p);
    runtime·printf("\t\tsp: %p,\n", p->sp);
    runtime·printf("\t\tpc: %p,\n", p->pc);
    runtime·printf("\t\tg: %p,\n", p->g);
    runtime·prints("\t},\n");
}

void
droscheme·dumpg(G* g) {
    if (g == nil) {
        runtime·prints("droscheme·dumpg(nil)\n");
        return;
    }

    runtime·printf("\n/* %p */\nvar g *G = &G{\n", g);
    runtime·printf("\tstackguard: %p,\n", g->stackguard);
    runtime·printf("\tstackbase: %p,\n", g->stackbase);
    //runtime·printf("\tdefer: "); droscheme·dumpdefer(g->defer);
    //runtime·printf("\tpanic: "); droscheme·dumppanic(g->panic);
    runtime·printf("\tsched: "); droscheme·dumpsched(&g->sched);
    //runtime·printf("\tgcstack: %p,\n", g->gcstack);
    //runtime·printf("\tgcsp: %p,\n", g->gcsp);
    //runtime·printf("\tgcguard: %p,\n", g->gcguard);
    //runtime·printf("\tstack0: %p,\n", g->stack0);
    //runtime·printf("\tentry: %p,\n", g->entry);
    runtime·printf("\talllink: %p,\n", g->alllink);
    //runtime·printf("\tparam: %p,\n", g->param);
    //runtime·printf("\tstatus: %d,\n", g->status);
    runtime·printf("\tgoid: %d,\n", g->goid);
    //runtime·printf("\tselgen: %d,\n", g->selgen);
    //runtime·printf("\twaitreason: %p,\n", g->waitreason);
    //runtime·printf("\tschedlink: %p,\n", g->schedlink);
    //runtime·printf("\treadyonstop: %d,\n", g->readyonstop);
    //runtime·printf("\tispanic: %d,\n", g->ispanic);
    runtime·printf("\tm: %p,\n", g->m);
    //runtime·printf("\tlockedm: %p,\n", g->lockedm);
    //runtime·printf("\tidlem: %p,\n", g->idlem);
    //runtime·printf("\tsig: %d,\n", g->sig);
    //runtime·printf("\twritenbuf: %d,\n", g->writenbuf);
    //runtime·printf("\twritebuf: %p,\n", g->writebuf);
    //runtime·printf("\tsigcode0: %p,\n", g->sigcode0);
    //runtime·printf("\tsigcode1: %p,\n", g->sigcode1);
    //runtime·printf("\tsigpc: %p,\n", g->sigpc);
    runtime·printf("\tgopc: %p,\n", g->gopc);
    runtime·prints("}\n");
}

void
droscheme·dumpm(M* m) {
    if (m == nil) {
        runtime·prints("droscheme·dumpm(nil)\n");
        return;
    }

    runtime·printf("\n/* %p */\nvar m *M = &M{\n", m);
    runtime·printf("\tg0: %p,\n", m->g0);
    runtime·printf("\tmorepc: %p,\n", m->morepc);
    runtime·printf("\tmoreargp: %p,\n", m->moreargp);
    runtime·printf("\tmorebuf: "); droscheme·dumpsched(&m->morebuf);
    runtime·printf("\tmoreframesize: %d,\n", m->moreframesize);
    runtime·printf("\tmoreargsize: %d,\n", m->moreargsize);
    //runtime·printf("\tcret: %d,\n", m->cret);
    //runtime·printf("\tprocid: %d,\n", m->procid);
    runtime·printf("\tgsignal: %p, // *G\n", m->gsignal);
    //runtime·printf("\ttls: &p,\n", m->tls);
    runtime·printf("\tcurg: %p,\n", m->curg);
    runtime·printf("\tid: %d,\n", m->id);
    //runtime·printf("\tmallocing: %d,\n", m->mallocing);
    //runtime·printf("\tgcing: %d,\n", m->gcing);
    //runtime·printf("\tlocks: %d,\n", m->locks);
    //runtime·printf("\tnomemprof: %d,\n", m->nomemprof);
    //runtime·printf("\twaitnextg: %d,\n", m->waitnextg);
    //runtime·printf("\tdying: %d,\n", m->dying);
    //runtime·printf("\tprofilehz: %d,\n", m->profilehz);
    //runtime·printf("\thelpgc: %d,\n", m->helpgc);
    //runtime·printf("\tfastrand: %d,\n", m->fastrand);
    //runtime·printf("\tncgocall: %d,\n", m->ncgocall);
    //runtime·printf("\thavenextg: %p,\n", m->havenextg);
    runtime·printf("\tnextg: %p,\n", m->nextg);
    runtime·printf("\talllink: %p,\n", m->alllink);
    //runtime·printf("\tschedlink: %p,\n", m->schedlink);
    //runtime·printf("\tmachport: %d,\n", m->machport);
    //runtime·printf("\tmcache: %p,\n", m->mcache);
    //runtime·printf("\tstackalloc: %p,\n", m->stackalloc);
    //runtime·printf("\tlockedg: %p,\n", m->lockedg);
    runtime·printf("\tidleg: %p,\n", m->idleg);
    //runtime·printf("\tcreatestack: %p,\n", m->createstack);
    //runtime·printf("\tfreglo: %p,\n", m->freglo);
    //runtime·printf("\tfreghi: %p,\n", m->freghi);
    //runtime·printf("\tfflag: %d,\n", m->fflag);
    //runtime·printf("\tnextwaitm: %p,\n", m->nextwaitm);
    //runtime·printf("\twaitsema: %d,\n", m->waitsema);
    //runtime·printf("\twaitsemacount: %d,\n", m->waitsemacount);
    //runtime·printf("\twaitsemalock: %d,\n", m->waitsemalock);
    runtime·prints("}\n");
}

void
droscheme·DumpInternals() {
    runtime·prints("---DumpInternals() begin\n");
    G* gp;
    M* mp;

    for(gp = runtime·allg; gp != nil; gp = gp->alllink) {
        droscheme·dumpg(gp);
    }

    for(mp = runtime·allm; mp != nil; mp = mp->alllink) {
        droscheme·dumpm(mp);
    }

    runtime·prints("---DumpInternals() end\n");
}
/*
byte   *gDummy;
uintptr gPointer;
Gobuf  *gSched;

void
droscheme·setcall(G*g) {
    runtime·printf("SP=%X\n", (uintptr)gSched->sp);
    runtime·printf("PC=%X\n", (uintptr)gSched->pc);
    runtime·printf("droscheme·setcall(%X)\n", (uintptr)g);
    runtime·printf("SP=%X\n", (uintptr)g->sched.sp);
    runtime·printf("PC=%X\n", (uintptr)g->sched.pc);
    runtime·memmove(&g->sched, gSched, sizeof(Gobuf));
    runtime·printf("SP=%X\n", (uintptr)g->sched.sp);
    runtime·printf("PC=%X\n", (uintptr)g->sched.pc);
    runtime·gogo(&g->sched, 0);
}

Gobuf *
droscheme·newcontext(byte *dummy) {
    runtime·prints("droscheme·newcontext(");
    runtime·printf("0x%X ", (uintptr)dummy);
    runtime·printf("0x%X ", sizeof(Gobuf));
    runtime·prints(")\n");
    Gobuf *buf = runtime·malloc(sizeof(Gobuf));
    return buf;
}

byte *
droscheme·getcontext(byte *dummy) {
    byte * uc = runtime·getcallerpc(dummy);
    runtime·printf("droscheme·getcontext(%p) = %p\n", dummy, uc);
    return uc;
}

void
droscheme·setcontext(byte *dummy, byte * uc) {
    //uc += 16;
    runtime·printf("droscheme·setcontext(%p, %p)\n", dummy, uc);
    runtime·setcallerpc(dummy, (byte *)uc);
    runtime·printf("--- leave droscheme·setcontext()\n");
}
*/
