
struct Param {
};

typedef struct Param Param;

typedef int FooFn(Param);

static FooFn foofn, barfn;

static int foofn(Param x) {
}
