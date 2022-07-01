#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include "cel.h"

Lattice *lat_new(uint32_t n) {
    Lattice *l = NULL;

    l = (Lattice *)calloc(1, sizeof(Lattice));
    if(!l) exit(1);
    l->m = (uint8_t *)calloc(n, sizeof(uint8_t));
    if(!l->m) exit(1);
    l->n = n;
    return l;
}

void lat_free(Lattice *l) {
    free(l->m);
    free(l);
    return ;
}

Lattice *lat_update(Lattice *l, Sigma f, Eta n, void *p) {
    uint32_t i = 0;
    Lattice *cel = NULL, *new = NULL;

    new = lat_new(l->n);
    for(; i < l->n; ++i) {
        cel = n(l, i, p);
        new->m[i] = f(cel, p);
        lat_free(cel);
    }

    return new;
}

uint32_t zn_add(uint32_t a, uint32_t b, uint32_t n) {
    return (a + b) % n;
}

uint32_t zn_sub(uint32_t a, uint32_t b, uint32_t n) {
    return a >= b ? (a - b) % n : (n - ((b - a) % n)) % n;
}

uint32_t zn_mul(uint32_t a, uint32_t b, uint32_t n) {
    return (a * b) % n;
}
