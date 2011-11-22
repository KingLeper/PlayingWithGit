#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a10b.h"

void *expr_const(void *n) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_exp;
  _data->u._const._n = n;
  return (void *)_data;
}

void *expr_var(void *v) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_exp;
  _data->u._var._v = v;
  return (void *)_data;
}

void *expr_if(void *test, void *conseq, void *alt) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_exp;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *expr_mult(void *randr1, void *randr2) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_exp;
  _data->u._mult._randr1 = randr1;
  _data->u._mult._randr2 = randr2;
  return (void *)_data;
}

void *expr_subr1(void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_exp;
  _data->u._subr1._rand = rand;
  return (void *)_data;
}

void *expr_zero(void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_exp;
  _data->u._zero._rand = rand;
  return (void *)_data;
}

void *expr_letcc(void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_exp;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *expr_throw(void *rator, void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_exp;
  _data->u._throw._rator = rator;
  _data->u._throw._rand = rand;
  return (void *)_data;
}

void *expr_lambda(void *body) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_exp;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *expr_app(void *rator, void *rand) {
exp* _data = (exp*)malloc(sizeof(exp));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_exp;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

void *ktr_empty(void *jumpout) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _empty_kt;
  _data->u._empty._jumpout = jumpout;
  return (void *)_data;
}

void *ktr_appr__m__outer(void *rand, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__outer_kt;
  _data->u._appr__m__outer._rand = rand;
  _data->u._appr__m__outer._env = env;
  _data->u._appr__m__outer._k = k;
  return (void *)_data;
}

void *ktr_appr__m__inner(void *v, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__inner_kt;
  _data->u._appr__m__inner._v = v;
  _data->u._appr__m__inner._k = k;
  return (void *)_data;
}

void *ktr_punchr__m__out(void *rand, void *env) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _punchr__m__out_kt;
  _data->u._punchr__m__out._rand = rand;
  _data->u._punchr__m__out._env = env;
  return (void *)_data;
}

void *ktr_ifr__m__k(void *test, void *conseq, void *alt, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _ifr__m__k_kt;
  _data->u._ifr__m__k._test = test;
  _data->u._ifr__m__k._conseq = conseq;
  _data->u._ifr__m__k._alt = alt;
  _data->u._ifr__m__k._env = env;
  _data->u._ifr__m__k._k = k;
  return (void *)_data;
}

void *ktr_multr__m__outer(void *randr2, void *env, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__outer_kt;
  _data->u._multr__m__outer._randr2 = randr2;
  _data->u._multr__m__outer._env = env;
  _data->u._multr__m__outer._k = k;
  return (void *)_data;
}

void *ktr_multr__m__inner(void *xr1, void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__inner_kt;
  _data->u._multr__m__inner._xr1 = xr1;
  _data->u._multr__m__inner._k = k;
  return (void *)_data;
}

void *ktr_zeror__m__k(void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zeror__m__k_kt;
  _data->u._zeror__m__k._k = k;
  return (void *)_data;
}

void *ktr_subr1r__m__k(void *k) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1r__m__k_kt;
  _data->u._subr1r__m__k._k = k;
  return (void *)_data;
}

void *envrr_empty() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _empty_envr;
  return (void *)_data;
}

void *envrr_extend(void *arg, void *env) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extend_envr;
  _data->u._extend._arg = arg;
  _data->u._extend._env = env;
  return (void *)_data;
}

void *closr_closure(void *code, void *env) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closure_clos;
  _data->u._closure._code = code;
  _data->u._closure._env = env;
  return (void *)_data;
}

void evalr__m__expr()
{
exp* _c = (exp*)r__t__expr;
switch (_c->tag) {
case _const_exp: {
void *n = _c->u._const._n;
r__t__a = (void *)n;
r__t__pc = &applyr__m__k;
break; }
case _var_exp: {
void *v = _c->u._var._v;
r__t__x = (void *)v;
r__t__pc = &applyr__m__env;
break; }
case _if_exp: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
r__t__k = (void *)ktr_ifr__m__k(test,conseq,alt,r__t__env,r__t__k);
r__t__expr = (void *)test;
r__t__pc = &evalr__m__expr;
break; }
case _mult_exp: {
void *randr1 = _c->u._mult._randr1;
void *randr2 = _c->u._mult._randr2;
r__t__k = (void *)ktr_multr__m__outer(randr2,r__t__env,r__t__k);
r__t__expr = (void *)randr1;
r__t__pc = &evalr__m__expr;
break; }
case _subr1_exp: {
void *rand = _c->u._subr1._rand;
r__t__k = (void *)ktr_subr1r__m__k(r__t__k);
r__t__expr = (void *)rand;
r__t__pc = &evalr__m__expr;
break; }
case _zero_exp: {
void *rand = _c->u._zero._rand;
r__t__k = (void *)ktr_zeror__m__k(r__t__k);
r__t__expr = (void *)rand;
r__t__pc = &evalr__m__expr;
break; }
case _letcc_exp: {
void *body = _c->u._letcc._body;
r__t__env = (void *)envrr_extend(r__t__k,r__t__env);
r__t__expr = (void *)body;
r__t__pc = &evalr__m__expr;
break; }
case _throw_exp: {
void *rator = _c->u._throw._rator;
void *rand = _c->u._throw._rand;
r__t__k = (void *)ktr_punchr__m__out(rand,r__t__env);
r__t__expr = (void *)rator;
r__t__pc = &evalr__m__expr;
break; }
case _lambda_exp: {
void *body = _c->u._lambda._body;
r__t__a = (void *)closr_closure(body,r__t__env);
r__t__pc = &applyr__m__k;
break; }
case _app_exp: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
r__t__k = (void *)ktr_appr__m__outer(rand,r__t__env,r__t__k);
r__t__expr = (void *)rator;
r__t__pc = &evalr__m__expr;
break; }
}
}

void applyr__m__k()
{
kt* _c = (kt*)r__t__k;
switch (_c->tag) {
case _empty_kt: {
void *jumpout = _c->u._empty._jumpout;
_trstr *trstr = (_trstr *)jumpout;
longjmp(*trstr->jmpbuf, 1);
break; }
case _appr__m__outer_kt: {
void *rand = _c->u._appr__m__outer._rand;
void *env = _c->u._appr__m__outer._env;
void *k = _c->u._appr__m__outer._k;
r__t__k = (void *)ktr_appr__m__inner(r__t__a,k);
r__t__expr = (void *)rand;
r__t__env = (void *)env;
r__t__pc = &evalr__m__expr;
break; }
case _appr__m__inner_kt: {
void *v = _c->u._appr__m__inner._v;
void *k = _c->u._appr__m__inner._k;
r__t__k = (void *)k;
r__t__v = (void *)v;
r__t__pc = &applyr__m__proc;
break; }
case _punchr__m__out_kt: {
void *rand = _c->u._punchr__m__out._rand;
void *env = _c->u._punchr__m__out._env;
r__t__expr = (void *)rand;
r__t__env = (void *)env;
r__t__k = (void *)r__t__a;
r__t__pc = &evalr__m__expr;
break; }
case _ifr__m__k_kt: {
void *test = _c->u._ifr__m__k._test;
void *conseq = _c->u._ifr__m__k._conseq;
void *alt = _c->u._ifr__m__k._alt;
void *env = _c->u._ifr__m__k._env;
void *k = _c->u._ifr__m__k._k;
if(r__t__a) {
  r__t__k = (void *)k;
r__t__expr = (void *)conseq;
r__t__pc = &evalr__m__expr;

} else {
  r__t__k = (void *)k;
r__t__expr = (void *)alt;
r__t__pc = &evalr__m__expr;

}
break; }
case _multr__m__outer_kt: {
void *randr2 = _c->u._multr__m__outer._randr2;
void *env = _c->u._multr__m__outer._env;
void *k = _c->u._multr__m__outer._k;
r__t__k = (void *)ktr_multr__m__inner(r__t__a,k);
r__t__env = (void *)env;
r__t__expr = (void *)randr2;
r__t__pc = &evalr__m__expr;
break; }
case _multr__m__inner_kt: {
void *xr1 = _c->u._multr__m__inner._xr1;
void *k = _c->u._multr__m__inner._k;
r__t__k = (void *)k;
r__t__a = (void *)(void *)((int)xr1 * (int)r__t__a);
r__t__pc = &applyr__m__k;
break; }
case _zeror__m__k_kt: {
void *k = _c->u._zeror__m__k._k;
r__t__k = (void *)k;
r__t__a = (void *)(r__t__a == 0);
r__t__pc = &applyr__m__k;
break; }
case _subr1r__m__k_kt: {
void *k = _c->u._subr1r__m__k._k;
r__t__k = (void *)k;
r__t__a = (void *)(void *)((int)r__t__a - 1);
r__t__pc = &applyr__m__k;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)r__t__env;
switch (_c->tag) {
case _empty_envr: {
return(r__t__a);
break; }
case _extend_envr: {
void *arg = _c->u._extend._arg;
void *env = _c->u._extend._env;
if((r__t__x == 0)) {
  r__t__a = (void *)arg;
r__t__pc = &applyr__m__k;

} else {
  r__t__env = (void *)env;
r__t__x = (void *)(void *)((int)r__t__x - 1);
r__t__pc = &applyr__m__env;

}
break; }
}
}

void applyr__m__proc()
{
clos* _c = (clos*)r__t__v;
switch (_c->tag) {
case _closure_clos: {
void *code = _c->u._closure._code;
void *env = _c->u._closure._env;
r__t__expr = (void *)code;
break; }
}
}

void ee()
{
return(evalr__m__expr);
}

int main()
{
r__t__n = (void *)(void *)5;
r__t__expr = (void *)expr_app(expr_lambda(expr_app(expr_app(expr_var((void *)0),expr_var((void *)0)),expr_const((void *)5))),expr_lambda(expr_lambda(expr_if(expr_zero(expr_var((void *)0)),expr_const((void *)1),expr_mult(expr_var((void *)0),expr_app(expr_app(expr_var((void *)1),expr_var((void *)1)),expr_subr1(expr_var((void *)0))))))));
r__t__env = (void *)envrr_empty();
r__t__pc = &ee;
mount_tram();
printf("Fact Test Yields: %d\n", (int)r__t__a);r__t__n = (void *)(void *)5;
r__t__expr = (void *)expr_letcc(expr_mult(expr_const((void *)5),expr_throw(expr_var((void *)0),expr_mult(expr_const((void *)2),expr_const((void *)6)))));
r__t__env = (void *)envrr_empty();
r__t__pc = &ee;
mount_tram();
printf("Let/CC Test Yields: %d\n", (int)r__t__a);}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
r__t__k= (void *)ktr_empty(dismount);
for(;;) {
r__t__pc();
}
}
return 0;
}
