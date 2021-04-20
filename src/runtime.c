#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>
#include <gc.h>

struct NatList{
	uint64_t x;
	struct NatList *xs;
};

uint64_t pcf_print(uint64_t x) {
	printf("%" PRIu64 "\n", x);
	return x;
}

void *pcf_mkclosure(void *fun, int amt, ...)
{
	int i;
	va_list valist;
	uint64_t** res = GC_MALLOC(sizeof (uint64_t*) * (amt + 1));

	va_start(valist, amt);

	res[0] = fun;
	i = 0;
	while (i++ < amt) {
		uint64_t *a = va_arg(valist, uint64_t*);
		res[i] = a;
	}

	va_end(valist);

	return res;
}

struct NatList * pcf_mklist(uint64_t *x, struct NatList *xs)
{
	struct NatList *res = GC_MALLOC(sizeof (struct NatList));
	*res = (struct NatList){x, xs};
	return res;
}



extern struct NatList* pcfmain(void);

void pcf_print_list(struct NatList *l) {
	printf("[");
	for(; l; l = l->xs)
		printf("%" PRIu64 "%s", l->x, l->xs ? ", " : "");
	printf("]");
}

struct NatList * pcf_tail(struct NatList *l) {
	return l->xs;
}

uint64_t pcf_head(struct NatList *l) {
	return l->x;
}

int main (int argc, char **argv) {
	/*uint64_t* rp = pcfmain();
	uint64_t r = (uint64_t)rp;
	pcf_print(r);*/

	struct NatList *r = pcfmain();
	pcf_print_list(r);
	puts("");

	return (int) r;
}
