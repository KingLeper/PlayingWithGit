void *r__t__rator, *r__t__rand, *r__t__expr, *r__t__env, *r__t__a, *r__t__v, *r__t__x, *r__t__k;

void (*r__t__pc)();

struct exp;
typedef struct exp exp;
struct exp {
  enum {
    _const_exp,
    _var_exp,
    _if_exp,
    _mult_exp,
    _subr1_exp,
    _zero_exp,
    _letcc_exp,
    _throw_exp,
    _lambda_exp,
    _app_exp
  } tag;
  union {
    struct { void *_n; } _const;
    struct { void *_v; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_randr1; void *_randr2; } _mult;
    struct { void *_rand; } _subr1;
    struct { void *_rand; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_rator; void *_rand; } _throw;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *expr_const(void *n);
void *expr_var(void *v);
void *expr_if(void *test, void *conseq, void *alt);
void *expr_mult(void *randr1, void *randr2);
void *expr_subr1(void *rand);
void *expr_zero(void *rand);
void *expr_letcc(void *body);
void *expr_throw(void *rator, void *rand);
void *expr_lambda(void *body);
void *expr_app(void *rator, void *rand);

struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _empty_kt,
    _appr__m__outer_kt,
    _appr__m__inner_kt,
    _punchr__m__out_kt,
    _ifr__m__k_kt,
    _multr__m__outer_kt,
    _multr__m__inner_kt,
    _zeror__m__k_kt,
    _subr1r__m__k_kt
  } tag;
  union {
    struct { void *_jumpout; } _empty;
    struct { void *_rand; void *_env; void *_k; } _appr__m__outer;
    struct { void *_v; void *_k; } _appr__m__inner;
    struct { void *_rand; void *_env; } _punchr__m__out;
    struct { void *_test; void *_conseq; void *_alt; void *_env; void *_k; } _ifr__m__k;
    struct { void *_randr2; void *_env; void *_k; } _multr__m__outer;
    struct { void *_xr1; void *_k; } _multr__m__inner;
    struct { void *_k; } _zeror__m__k;
    struct { void *_k; } _subr1r__m__k;
  } u;
};

void *ktr_empty(void *jumpout);
void *ktr_appr__m__outer(void *rand, void *env, void *k);
void *ktr_appr__m__inner(void *v, void *k);
void *ktr_punchr__m__out(void *rand, void *env);
void *ktr_ifr__m__k(void *test, void *conseq, void *alt, void *env, void *k);
void *ktr_multr__m__outer(void *randr2, void *env, void *k);
void *ktr_multr__m__inner(void *xr1, void *k);
void *ktr_zeror__m__k(void *k);
void *ktr_subr1r__m__k(void *k);

void evalr__m__expr();
void applyr__m__k();
struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _empty_envr,
    _extend_envr
  } tag;
  union {
    struct { char dummy; } _empty;
    struct { void *_arg; void *_env; } _extend;
  } u;
};

void *envrr_empty();
void *envrr_extend(void *arg, void *env);

void applyr__m__env();
struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closure_clos
  } tag;
  union {
    struct { void *_code; void *_env; } _closure;
  } u;
};

void *closr_closure(void *code, void *env);

void applyr__m__proc();
void ee();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

