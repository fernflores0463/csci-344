#include "all.h"

int lengthPL(Parlist ps) {
    int n;

    for (n = 0; ps != NULL; n++)
         ps = ps->tl;
    return n;
}

Parlist mkPL(Par p, Parlist ps) {
    Parlist new_ps;

    new_ps = malloc(sizeof *new_ps);
    assert(new_ps != NULL);
    new_ps->hd = p;
    new_ps->tl = ps;
    return new_ps;
}

Parlist popPL(Parlist ps) {
    Parlist original = ps;

    assert(ps);
    ps = ps->tl;
    free(original);
    return ps;
}

Par nthPL(Parlist ps, unsigned n) {
    unsigned i;

    for(i=0; ps && i<n; i++)
        ps=ps->tl;

    assert(ps != NULL);
    return ps->hd;
}

void printparlist(FILE *output, va_list_box *box) {
    for (Parlist ps = va_arg(box->ap, Parlist); ps != NULL; ps = ps->tl)
        fprint(output, "%p%s", ps->hd, ps->tl ? " " : "");
}

int lengthUL(UnitTestlist us) {
    int n;

    for (n = 0; us != NULL; n++)
         us = us->tl;
    return n;
}

UnitTestlist mkUL(UnitTest u, UnitTestlist us) {
    UnitTestlist new_us;

    new_us = malloc(sizeof *new_us);
    assert(new_us != NULL);
    new_us->hd = u;
    new_us->tl = us;
    return new_us;
}

UnitTestlist popUL(UnitTestlist us) {
    UnitTestlist original = us;

    assert(us);
    us = us->tl;
    free(original);
    return us;
}

UnitTest nthUL(UnitTestlist us, unsigned n) {
    unsigned i;

    for(i=0; us && i<n; i++)
        us=us->tl;

    assert(us != NULL);
    return us->hd;
}

void printunittestlist(FILE *output, va_list_box *box) {
    for (UnitTestlist us = va_arg(box->ap, UnitTestlist); us != NULL; us = us->
                                                                             tl)
        fprint(output, "%u%s", us->hd, us->tl ? " " : "");
}

int lengthEL(Explist es) {
    int n;

    for (n = 0; es != NULL; n++)
         es = es->tl;
    return n;
}

Explist mkEL(Exp e, Explist es) {
    Explist new_es;

    new_es = malloc(sizeof *new_es);
    assert(new_es != NULL);
    new_es->hd = e;
    new_es->tl = es;
    return new_es;
}

Explist popEL(Explist es) {
    Explist original = es;

    assert(es);
    es = es->tl;
    free(original);
    return es;
}

Exp nthEL(Explist es, unsigned n) {
    unsigned i;

    for(i=0; es && i<n; i++)
        es=es->tl;

    assert(es != NULL);
    return es->hd;
}

void printexplist(FILE *output, va_list_box *box) {
    for (Explist es = va_arg(box->ap, Explist); es != NULL; es = es->tl)
        fprint(output, "%e%s", es->hd, es->tl ? " " : "");
}

int lengthNL(Namelist ns) {
    int n;

    for (n = 0; ns != NULL; n++)
         ns = ns->tl;
    return n;
}

Namelist mkNL(Name n, Namelist ns) {
    Namelist new_ns;

    new_ns = malloc(sizeof *new_ns);
    assert(new_ns != NULL);
    new_ns->hd = n;
    new_ns->tl = ns;
    return new_ns;
}

Namelist popNL(Namelist ns) {
    Namelist original = ns;

    assert(ns);
    ns = ns->tl;
    free(original);
    return ns;
}

Name nthNL(Namelist ns, unsigned n) {
    unsigned i;

    for(i=0; ns && i<n; i++)
        ns=ns->tl;

    assert(ns != NULL);
    return ns->hd;
}

void printnamelist(FILE *output, va_list_box *box) {
    for (Namelist ns = va_arg(box->ap, Namelist); ns != NULL; ns = ns->tl)
        fprint(output, "%n%s", ns->hd, ns->tl ? " " : "");
}

int lengthVL(Valuelist vs) {
    int n;

    for (n = 0; vs != NULL; n++)
         vs = vs->tl;
    return n;
}

Valuelist mkVL(Value v, Valuelist vs) {
    Valuelist new_vs;

    new_vs = malloc(sizeof *new_vs);
    assert(new_vs != NULL);
    new_vs->hd = v;
    new_vs->tl = vs;
    return new_vs;
}

Valuelist popVL(Valuelist vs) {
    Valuelist original = vs;

    assert(vs);
    vs = vs->tl;
    free(original);
    return vs;
}

Value nthVL(Valuelist vs, unsigned n) {
    unsigned i;

    for(i=0; vs && i<n; i++)
        vs=vs->tl;

    assert(vs != NULL);
    return vs->hd;
}

void printvaluelist(FILE *output, va_list_box *box) {
    for (Valuelist vs = va_arg(box->ap, Valuelist); vs != NULL; vs = vs->tl)
        fprint(output, "%v%s", vs->hd, vs->tl ? " " : "");
}

int lengthFL(Funlist fs) {
    int n;

    for (n = 0; fs != NULL; n++)
         fs = fs->tl;
    return n;
}

Funlist mkFL(Fun f, Funlist fs) {
    Funlist new_fs;

    new_fs = malloc(sizeof *new_fs);
    assert(new_fs != NULL);
    new_fs->hd = f;
    new_fs->tl = fs;
    return new_fs;
}

Funlist popFL(Funlist fs) {
    Funlist original = fs;

    assert(fs);
    fs = fs->tl;
    free(original);
    return fs;
}

Fun nthFL(Funlist fs, unsigned n) {
    unsigned i;

    for(i=0; fs && i<n; i++)
        fs=fs->tl;

    assert(fs != NULL);
    return fs->hd;
}

void printfunlist(FILE *output, va_list_box *box) {
    for (Funlist fs = va_arg(box->ap, Funlist); fs != NULL; fs = fs->tl)
        fprint(output, "%f%s", fs->hd, fs->tl ? " " : "");
}

