#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>
#include <termios.h>
#include <unistd.h>
#include <signal.h>
#include "cel.h"

enum {
    ROWS = 50,
    COLS = 80,
    DELAY = 300000,
    RULES = 8
};

#define POS(i, j) ((i)*ROWS + (j))
#define AR(x) zn_add((x), 1, ROWS)
#define SR(x) zn_sub((x), 1, ROWS)
#define AC(x) zn_add((x), 1, COLS)
#define SC(x) zn_sub((x), 1, COLS)

static Lattice *rnd(void);
static Lattice *eta(Lattice *, uint32_t, void *);
static uint8_t  sigma(Lattice *, void *);
static void     print(Lattice *);
static void     run(void);
static void     clear(void);
static void     catch(int);

static Lattice *Life = NULL;
static uint32_t Rule = 0;
static uint32_t Gen  = 0;

static Lattice *rnd(void) {
    Lattice *l = NULL;
    uint32_t i = 0;

    l = lat_new(ROWS*COLS);
    srandom(time(NULL));
    for(; i < ROWS*COLS; ++i) l->m[i] = (uint8_t) (random() % 2);
    return l;
}

static uint8_t sigma(Lattice *l, void *p) {
    uint8_t i = 1, n = 0, c = 0;
    uintptr_t x = (uintptr_t)p;

    for(; i < 9; ++i) n += l->m[i];
    switch(x) {
        case 1:
            c = (!l->m[0] && (n == 3 || n == 6)) || (l->m[0] && (n == 2 || n == 3));
        break;

        case 2:
            c = (!l->m[0] && n == 2) || (l->m[0] && !n);
        break;

        case 3:
            c = n % 2;
        break;

        case 4:
            c = !(!l->m[0] && n != 3);
        break;

        case 5: 
            c = (!l->m[0] && n == 3) || (l->m[0] && (n >= 1 && n <= 5));
        break;

        case 6:
            c = (!l->m[0] && n == (time(NULL) % 6)) || (l->m[0] && (n == 2 || n == 3));
        break;

        case 7:
            c = (!l->m[0] && (n == 0 || n == 3)) || (l->m[0] && n == (random() % 6));
        break;

        default:
            c = (!l->m[0] && n == 3) || (l->m[0] && (n == 2 || n == 3));
        break;
    }

    return c ? 1 : 0;
}

static Lattice *eta(Lattice *l, uint32_t p, void *unused) {
    Lattice *n = NULL;
    uint32_t i = p / ROWS, j = p % ROWS;

    n = lat_new(9);
    n->m[0] = l->m[p];
    n->m[1] = l->m[POS(SR(i), j)];
    n->m[2] = l->m[POS(SR(i), AC(j))];
    n->m[3] = l->m[POS(SR(i), SC(j))];
    n->m[4] = l->m[POS(AR(i), j)];
    n->m[5] = l->m[POS(AR(i), AC(j))];
    n->m[6] = l->m[POS(AR(i), SC(j))];
    n->m[7] = l->m[POS(i, AC(j))];
    n->m[8] = l->m[POS(i, SC(j))];
    return n;
}

static void clear(void) {
    printf("\033[2J\033[0;0f");
    printf("\033[%d;%df", 0, 0);
    return ;
}

static void print(Lattice *l) {
    uint32_t i = 0, j = 0;

    clear();
    printf("%u-th rule, %u-th generation\n\n", Rule, Gen);
    for(; i < ROWS; ++i) {
        for(j = 0; j < COLS; ++j) {
            printf("%c ", l->m[POS(i, j)] ? '.' : ' ');
        }

        putchar('\n');
    }

    return ;
}

static void run(void) {
    Lattice *old = NULL;
    
    Life = rnd();
    print(Life);
    usleep(DELAY);
    while(1) {
        old = Life;
        Life = lat_update(Life, sigma, eta, (void *)Rule);
        lat_free(old);
        print(Life);
        usleep(DELAY);
        ++Gen;
    }

    return ;
}

static void catch(int signal) {
    if(signal == SIGINT) {
        clear();
        puts("...");
        if(Life) free(Life);
        exit(0);
    }

    return ;
}

int main(int argc, char **argv) {
    signal(SIGINT, catch);
    if(argc > 1) {
        Rule = strtol(argv[1], NULL, 10) % RULES;
    }

    run();
    return 0;
}
