#ifndef __CEL_H_
#define __CEL_H_

typedef struct Lattice {
    uint8_t *m;
    uint32_t n;
} Lattice;

typedef uint8_t (*Sigma)(Lattice *, void *);
typedef Lattice *(*Eta)(Lattice *, uint32_t, void *);

Lattice *lat_new(uint32_t);
Lattice *lat_update(Lattice *, Sigma, Eta, void *);
void lat_free(Lattice *);

uint32_t zn_add(uint32_t, uint32_t, uint32_t);
uint32_t zn_sub(uint32_t, uint32_t, uint32_t);
uint32_t zn_mul(uint32_t, uint32_t, uint32_t);

#endif
