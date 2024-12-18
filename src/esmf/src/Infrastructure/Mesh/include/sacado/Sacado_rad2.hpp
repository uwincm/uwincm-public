// $Id$ 
// @HEADER
// ***********************************************************************
//
//                           Sacado Package
//                 Copyright (2007) Sandia Corporation
//
// Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
// the U.S. Government retains certain rights in this software.
//
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// Questions? Contact David M. Gay (dmgay@sandia.gov) or Eric T. Phipps
// (etphipp@sandia.gov).
//
// ***********************************************************************
// @HEADER

// Extension of the RAD package (Reverse Automatic Differentiation) --
// a package specialized for function and gradient evaluations -- to
// Hessian-vector products.
// This variant is for Hessian-vector products of "double" variables, and
// Sacado::Rad2d::ADvar should be equivalent to Sacado::Rad2::ADvar<double>,
// but this nontemplated code may easier to understand.  It relies on ops
// implemented in Sacado_radops2.cpp.
// Written in 2007 by David M. Gay at Sandia National Labs, Albuquerque, NM.

#ifndef SACADO_RAD2_H
#define SACADO_RAD2_H

#include <stddef.h>
#include <cmath>
#include <math.h>

#ifndef SACADO_NO_NAMESPACE
namespace Sacado {
namespace Rad2d { // "2" for 2nd derivatives, "d" for "double"
#endif

// -DNO_USING_STDCC is needed, e.g., with Sun CC 5.7
#ifndef RAD_NO_USING_STDCC
  // Bring math functions into scope
  using std::exp;
  using std::log;
  using std::log10;
  using std::sqrt;
  using std::cos;
  using std::sin;
  using std::tan;
  using std::acos;
  using std::asin;
  using std::atan;
  using std::cosh;
  using std::sinh;
  using std::tanh;
  using std::abs;
  using std::atan2;
  using std::pow;
#endif //NO_USING_STDCC

 class ADvar;
 class ADvari;
 class ADvar1;
 class ADvar2;
 class ConstADvar;
 class Derp;
 class IndepADvar;

 struct
ADmemblock {	// We get memory in ADmemblock chunks and never give it back,
		// but reuse it once computations start anew after call(s) on
		// ADcontext::Gradcomp() or ADcontext::Weighted_Gradcomp().
	ADmemblock *next;
	double memblk[2000];
	};

 struct
ADvari_block {
	enum { Gulp = 1021 };
	ADvari_block *next, *prev;
	ADvari **limit;
	ADvari *pADvari[Gulp];
	};

 class
ADcontext {	// A singleton class: one instance in radops.c
	ADmemblock *Busy, *Free;
	char *Mbase;
	size_t Mleft;
	ADvari **Ailimit, **Ainext;
	ADvari_block *Aibusy, *Aifree;
	ADmemblock First;
	ADvari_block AiFirst;
	void *new_ADmemblock(size_t);
	void new_ADvari_block();
 public:
	ADcontext();
	void *Memalloc(size_t len);
	static void Gradcomp();
	static void Hvprod(int, ADvar**, double*, double*);
	static void Weighted_Gradcomp(int, ADvar**, double*);
	inline void ADvari_record(ADvari *x) {
		if (Ainext >= Ailimit)
			new_ADvari_block();
		*Ainext++ = x;
		}
	};

inline void *ADcontext::Memalloc(size_t len) {
		if (Mleft >= len)
			return Mbase + (Mleft -= len);
		return new_ADmemblock(len);
		}

 class
CADcontext: public ADcontext {
 protected:
	bool fpval_implies_const;
 public:
	friend class ADvar;
	CADcontext(): ADcontext() { fpval_implies_const = false; }
	static const double One, negOne;
	};

 class
Derp {		// one derivative-propagation operation
 public:
	friend class ADvarn;
	static Derp *LastDerp;
	Derp *next;
	const double *a;
	const ADvari *b;
	mutable ADvari *c;
	void *operator new(size_t);
	void operator delete(void*) {} /*Should never be called.*/
	inline Derp(){};
	Derp(const ADvari *);
	Derp(const double *, const ADvari *);
	Derp(const double *, const ADvari *, const ADvari *);
	/* c->aval += a * b->aval; */
	};

inline Derp::Derp(const ADvari *c1): c((ADvari*)c1) {
		next = LastDerp;
		LastDerp = this;
		}

inline Derp::Derp(const double *a1, const ADvari *c1):
		a(a1), c((ADvari*)c1) {
		next = LastDerp;
		LastDerp = this;
		}

inline Derp::Derp(const double *a1, const ADvari *b1, const ADvari *c1):
		a(a1), b(b1), c((ADvari*)c1) {
		next = LastDerp;
		LastDerp = this;
		}

 enum Advari_Opclass {
	Hv_const,
	Hv_copy,
	Hv_binary,
	Hv_unary,
	Hv_negate,
	Hv_plusLR,
	Hv_minusLR,
	Hv_timesL,
	Hv_timesLR,
	Hv_quotLR,
	Hv_nary
	};

 extern ADvari& ADf1(double f, double g, double h, const ADvari &x);
 extern ADvari& ADf2(double f, double gx, double gy, double hxx,
			double hxy, double hyy, const ADvari &x, const ADvari &y);
 extern ADvari& ADfn(double f, int n, const ADvar *x, const double *g, const double *h);

 class
ADvari {	// implementation of an ADvar
 public:
	static ADcontext adc;
	Advari_Opclass opclass;
	double Val;		// result of this operation
	mutable double aval;	// adjoint -- partial of final result w.r.t. this Val
	mutable double dO;	// deriv of op w.r.t. t in x + t*p
	mutable double aO;	// adjoint (in Hv computation) of op
	mutable double adO;	// adjoint (in Hv computation) of dO
	void *operator new(size_t len) { return ADvari::adc.Memalloc(len); }
	void operator delete(void*) {} /*Should never be called.*/
	inline ADvari(Advari_Opclass oc, double t):
		opclass(oc), Val(t), aval(0.), dO(0.)
		{ if (oc != Hv_const) ADvari::adc.ADvari_record(this); }
	inline ADvari(Advari_Opclass oc, double t, double ta):
		opclass(oc), Val(t), aval(ta), dO(0.)
		{ if (oc != Hv_const) ADvari::adc.ADvari_record(this); }
 private:
	inline ADvari(): Val(0.), aval(0.), dO(0.) {}	// prevent construction without value (?)
 public:
	friend class ConstADvari;
#ifdef RAD_AUTO_AD_Const
	friend class ADcontext;
	friend class ADvar1;
	friend class ADvar;
	friend class ConstADvar;
	friend class IndepADvar;
 private:
	ADvari *Next;
	static ADvari *First_ADvari, **Last_ADvari;
 protected:
	IndepADvar *padv;
 public:
	ADvari(const IndepADvar *, double);
#endif
#define F friend
#define R ADvari&
#define Ai const ADvari&
#define T1(r,f) F r f(Ai);
#define T2(r,f) \
F r f(Ai,Ai); \
F r f(double,Ai); \
F r f(Ai,double);
	T1(R,operator+)
	T2(R,operator+)
	T1(R,operator-)
	T2(R,operator-)
	T2(R,operator*)
	T2(R,operator/)
	T1(R,abs)
	T1(R,acos)
	T1(R,acosh)
	T1(R,asin)
	T1(R,asinh)
	T1(R,atan)
	T1(R,atanh)
	T2(R,atan2)
	T2(R,max)
	T2(R,min)
	T1(R,cos)
	T1(R,cosh)
	T1(R,exp)
	T1(R,log)
	T1(R,log10)
	T2(R,pow)
	T1(R,sin)
	T1(R,sinh)
	T1(R,sqrt)
	T1(R,tan)
	T1(R,tanh)
	T1(R,fabs)
	T1(R,copy)
	T2(int,operator<)
	T2(int,operator<=)
	T2(int,operator==)
	T2(int,operator!=)
	T2(int,operator>=)
	T2(int,operator>)
#undef T2
#undef T1
#undef Ai
#undef R
#undef F

	friend ADvari& ADf1(double f, double g, double h, const ADvari &x);
	friend ADvari& ADf2(double f, double gx, double gy, double hxx,
			double hxy, double hyy, const ADvari &x, const ADvari &y);
	friend ADvari& ADfn(double f, int n, const ADvar *x, const double *g, const double *h);
	};

 inline void* Derp::operator new(size_t len) { return ADvari::adc.Memalloc(len); }


 class
ADvar1: public ADvari {	// simplest unary ops
 public:
	Derp d;
	ADvar1(Advari_Opclass oc, double val1, const double *a1, const ADvari *c1):
		ADvari(oc,val1), d(a1,this,c1) {}
#ifdef RAD_AUTO_AD_Const
	ADvar1(const IndepADvar *, const IndepADvar &);
	ADvar1(const IndepADvar *, const ADvari &);
#endif
	};

 class
ConstADvari: public ADvari {
 private:
	ConstADvari *prevcad;
	ConstADvari() {};	// prevent construction without value (?)
	static ConstADvari *lastcad;
 public:
	static CADcontext cadc;
	inline void *operator new(size_t len) { return ConstADvari::cadc.Memalloc(len); }
	inline ConstADvari(double t): ADvari(Hv_copy, t) { prevcad = lastcad; lastcad = this; }
	static void aval_reset(void);
	};

 class
IndepADvar
{
 private:
	inline IndepADvar& operator=(const IndepADvar &x) {
		/* private to prevent assignment */
#ifdef RAD_AUTO_AD_Const
		if (cv)
			cv->padv = 0;
		cv = new ADvar1(this,x);
		return *this;
#else
#ifdef RAD_EQ_ALIAS
		cv = x.cv;
		return *this;
#else
		return ADvar_operatoreq(this,*x.cv);
#endif
#endif /* RAD_AUTO_AD_Const */
		}
 protected:
	static void AD_Const(const IndepADvar&);
	ADvari *cv;
 public:
	typedef double value_type;
	friend class ADvar;
	friend class ADvar1;
	friend class ADvarn;
	friend class ADcontext;
	IndepADvar(double);
	IndepADvar(int);
	IndepADvar(long);
	IndepADvar& operator=(double);
#ifdef RAD_AUTO_AD_Const
	inline IndepADvar(const IndepADvar &x) { cv = x.cv ? new ADvar1(this, x) : 0; };
	inline IndepADvar() { cv = 0; }
	inline ~IndepADvar() {
			if (cv)
				cv->padv = 0;
		}
#else
	inline IndepADvar() {
#ifndef RAD_EQ_ALIAS
		cv = 0;
#endif
		}
	inline ~IndepADvar() {}
	friend IndepADvar& ADvar_operatoreq(IndepADvar*, const ADvari&);
#endif

	friend void AD_Const(const IndepADvar&);

	inline operator ADvari&() { return *cv; }
	inline operator ADvari&() const { return *(ADvari*)cv; }

	inline double val() const { return cv->Val; }
	inline double adj() const { return cv->aval; }
	static inline void Gradcomp() { ADcontext::Gradcomp(); }
	static inline void Hvprod(int n, ADvar **vp, double *v, double *hv)
				{ ADcontext::Hvprod(n, vp, v, hv); }
	static inline void aval_reset() { ConstADvari::aval_reset(); }
	static inline void Weighted_Gradcomp(int n, ADvar **v, double *w)
				{ ADcontext::Weighted_Gradcomp(n, v, w); }


#define Ai const ADvari&
#define AI const IndepADvar&
#define T2(r,f) \
 r f(AI,AI);\
 r f(Ai,AI);\
 r f(AI,Ai);\
 r f(double,AI);\
 r f(AI,double);
#define T1(f) friend ADvari& f(AI);

#define F friend ADvari&
T2(F, operator+)
T2(F, operator-)
T2(F, operator*)
T2(F, operator/)
T2(F, atan2)
T2(F, max)
T2(F, min)
T2(F, pow)
#undef F
#define F friend int
T2(F, operator<)
T2(F, operator<=)
T2(F, operator==)
T2(F, operator!=)
T2(F, operator>=)
T2(F, operator>)

T1(operator+)
T1(operator-)
T1(abs)
T1(acos)
T1(acosh)
T1(asin)
T1(asinh)
T1(atan)
T1(atanh)
T1(cos)
T1(cosh)
T1(exp)
T1(log)
T1(log10)
T1(sin)
T1(sinh)
T1(sqrt)
T1(tan)
T1(tanh)
T1(fabs)
T1(copy)

#undef T1
#undef T2
#undef F
#undef Ai
#undef AI

	};

 class
ADvar: public IndepADvar {		// an "active" variable
	void ADvar_ctr(double d);
 public:
	inline ADvar() { /* cv = 0; */ }
	inline ADvar(double d) { ADvar_ctr(d); }
	inline ADvar(int i)	{ ADvar_ctr((double)i); }
	inline ADvar(long i)	{ ADvar_ctr((double)i); }
	inline ~ADvar() {}
#ifdef RAD_AUTO_AD_Const
	friend class ADvar1;
	inline ADvar(const IndepADvar &x) { cv = x.cv ? new ADvar1(this, x) : 0; }
	inline ADvar(ADvari &x) { cv = &x; x.padv = this; }
	inline ADvar& operator=(const IndepADvar &x) {
		if (cv)
			cv->padv = 0;
		cv = new ADvar1(this,x);
		return *this;
		}
	inline ADvar& operator=(const ADvari &x) {
		if (cv)
			cv->padv = 0;
		cv = new ADvar1(this, x);
		return *this;
		}
#else
	friend ADvar& ADvar_operatoreq(ADvar*, const ADvari&);
#ifdef RAD_EQ_ALIAS
	/* allow aliasing v and w after "v = w;" */
	inline ADvar(const IndepADvar &x) { cv = x.cv; }
	inline ADvar(const ADvari &x) { cv = (ADvari*)&x; }
	inline ADvar& operator=(const ADvari &x) { cv = (ADvari*)&x;   return *this; }
	inline ADvar& operator=(const IndepADvar &x)  { cv = (ADvari*)x.cv; return *this; }
#else
	ADvar(const IndepADvar &x) { cv = x.cv ?
		new ADvar1(Hv_copy, x.cv->Val, &CADcontext::One, x.cv) : 0; }
	ADvar(const ADvari &x) { cv = new ADvar1(Hv_copy, x.Val, &CADcontext::One, &x); }
	inline ADvar& operator=(const ADvari &x) { return ADvar_operatoreq(this,x); }
	inline ADvar& operator=(const IndepADvar &x)    { return ADvar_operatoreq(this,*x.cv); }
#endif /* RAD_EQ_ALIAS */
#endif /* RAD_AUTO_AD_Const */
	ADvar& operator=(double);
	ADvar& operator+=(const ADvari&);
	ADvar& operator+=(double);
	ADvar& operator-=(const ADvari&);
	ADvar& operator-=(double);
	ADvar& operator*=(const ADvari&);
	ADvar& operator*=(double);
	ADvar& operator/=(const ADvari&);
	ADvar& operator/=(double);
	inline static bool get_fpval_implies_const(void)
		{ return ConstADvari::cadc.fpval_implies_const; }
	inline static void set_fpval_implies_const(bool newval)
		{ ConstADvari::cadc.fpval_implies_const = newval; }
	inline static bool setget_fpval_implies_const(bool newval) {
		bool oldval = ConstADvari::cadc.fpval_implies_const;
		ConstADvari::cadc.fpval_implies_const = newval;
		return oldval;
		}
	static inline void Gradcomp() { ADcontext::Gradcomp(); }
	static inline void Hvprod(int n, ADvar **vp, double *v, double *hv)
				{ ADcontext::Hvprod(n, vp, v, hv); }
	static inline void aval_reset() { ConstADvari::aval_reset(); }
	static inline void Weighted_Gradcomp(int n, ADvar **v, double *w)
				{ ADcontext::Weighted_Gradcomp(n, v, w); }
	};

 inline void AD_Const(const IndepADvar&v) { IndepADvar::AD_Const(v); }

 class
ConstADvar: public ADvar {
 private: // disable op=
	ConstADvar& operator+=(const ADvari&);
	ConstADvar& operator+=(double);
	ConstADvar& operator-=(const ADvari&);
	ConstADvar& operator-=(double);
	ConstADvar& operator*=(const ADvari&);
	ConstADvar& operator*=(double);
	ConstADvar& operator/=(const ADvari&);
	ConstADvar& operator/=(double);
	void ConstADvar_ctr(double);
 public:
	inline ConstADvar(double d)	{ ConstADvar_ctr(d); }
	inline ConstADvar(int i)	{ ConstADvar_ctr((double)i); }
	inline ConstADvar(long i)	{ ConstADvar_ctr((double)i); }
	ConstADvar(const ADvari &x);
#ifdef RAD_AUTO_AD_Const
	ConstADvar(const IndepADvar &x) { cv = new ADvar1(this,x); }
#endif
	inline ~ConstADvar(){}
#ifdef RAD_NO_CONST_UPDATE
 private:
#endif
	ConstADvar();
	inline ConstADvar& operator=(double d) { cv->Val = d; return *this; }
	inline ConstADvar& operator=(const IndepADvar& d) { cv->Val = d.val(); return *this; }
 };

 class
ADvar1s: public ADvar1 { // unary ops with partials
 public:
	double pL;	// deriv of op w.r.t. left operand L
	ADvar1s(double val1, double d1, const ADvari *c1):
		ADvar1(Hv_timesL,val1,&pL,c1), pL(d1) {}
	};

 class
ADvar1g: public ADvar1 { // unary ops with partials
 public:
	double pL;	// deriv of op w.r.t. left operand L
	double pL2;	// partial of op w.r.t. L,L
	ADvar1g(double val1, double d1, double d2, const ADvari *c1):
		ADvar1(Hv_unary,val1,&pL,c1), pL(d1), pL2(d2) {}
	};

 class
ADvar2: public ADvari {	// basic binary ops
 public:
	Derp dL, dR;
	ADvar2(Advari_Opclass oc, double val1, const ADvari *Lcv, const double *Lc,
			const ADvari *Rcv, const double *Rc): ADvari(oc,val1) {
		dR.next = Derp::LastDerp;
		dL.next = &dR;
		Derp::LastDerp = &dL;
		dL.a = Lc;
		dL.c = (ADvari*)Lcv;
		dR.a = Rc;
		dR.c = (ADvari*)Rcv;
		dL.b = dR.b = this;
		}
	};

 class
ADvar2q: public ADvar2 { // binary ops with partials
 public:
	double pL;	// deriv of op w.r.t. left operand L
	double pR;	// deriv of op w.r.t. right operand R
	double pLR;	// second partial w.r.t. L,R
	double pR2;	// second partial w.r.t. R,R
	ADvar2q(double val1, double Lp, double Rp, double LR, double R2,
		const ADvari *Lcv, const ADvari *Rcv);
	};

 class
ADvar2g: public ADvar2 { // general binary ops with partials
 public:
	double pL;	// deriv of op w.r.t. left operand L
	double pR;	// deriv of op w.r.t. right operand R
	double pL2;	// second partial w.r.t. L,L
	double pLR;	// second partial w.r.t. L,R
	double pR2;	// second partial w.r.t. R,R
	ADvar2g(double val1, double Lp, double Rp, double L2, double LR, double R2,
		const ADvari *Lcv, const ADvari *Rcv);
	};

 class
ADvarn: public ADvari { // n-ary ops with partials g and 2nd partials h (lower triangle, rowwise)
 public:
	int n;
	double *G, *H;
	Derp *D;
	ADvarn(double val1, int n1, const ADvar *x, const double *g, const double *h);
	};

inline ADvari &operator+(ADvari &T) { return T; }
inline ADvari &operator+(const ADvari &T) { return (ADvari&) T; }

inline int operator<(const ADvari &L, const ADvari &R) { return L.Val < R.Val; }
inline int operator<(const ADvari &L, double R) { return L.Val < R; }
inline int operator<(double L, const ADvari &R) { return L < R.Val; }

inline int operator<=(const ADvari &L, const ADvari &R) { return L.Val <= R.Val; }
inline int operator<=(const ADvari &L, double R) { return L.Val <= R; }
inline int operator<=(double L, const ADvari &R) { return L <= R.Val; }

inline int operator==(const ADvari &L, const ADvari &R) { return L.Val == R.Val; }
inline int operator==(const ADvari &L, double R) { return L.Val == R; }
inline int operator==(double L, const ADvari &R) { return L == R.Val; }

inline int operator!=(const ADvari &L, const ADvari &R) { return L.Val != R.Val; }
inline int operator!=(const ADvari &L, double R) { return L.Val != R; }
inline int operator!=(double L, const ADvari &R) { return L != R.Val; }

inline int operator>=(const ADvari &L, const ADvari &R) { return L.Val >= R.Val; }
inline int operator>=(const ADvari &L, double R) { return L.Val >= R; }
inline int operator>=(double L, const ADvari &R) { return L >= R.Val; }

inline int operator>(const ADvari &L, const ADvari &R) { return L.Val > R.Val; }
inline int operator>(const ADvari &L, double R) { return L.Val > R; }
inline int operator>(double L, const ADvari &R) { return L > R.Val; }

inline ADvari& copy(const IndepADvar &x)
{ return *(new ADvar1(Hv_copy, x.cv->Val, &CADcontext::One, x.cv)); }

inline ADvari& copy(const ADvari &x)
{ return *(new ADvar1(Hv_copy, x.Val, &CADcontext::One, &x)); }

inline ADvari& abs(const ADvari &x)
{ return fabs(x); }

#define A (ADvari*)
#define T inline
#define F ADvari&
#define Ai const ADvari&
#define AI const IndepADvar&
#define D double
#define T2(r,f) \
 T r f(Ai L, AI R) { return f(L, *A R.cv); }\
 T r f(AI L, Ai R) { return f(*A L.cv, R); }\
 T r f(AI L, AI R) { return f(*A L.cv, *A R.cv); }\
 T r f(AI L, D R) { return f(*A L.cv, R); }\
 T r f(D L, AI R) { return f(L, *A R.cv); }

T2(F, operator+)
T2(F, operator-)
T2(F, operator*)
T2(F, operator/)
T2(F, atan2)
T2(F, pow)
T2(F, max)
T2(F, min)
T2(int, operator<)
T2(int, operator<=)
T2(int, operator==)
T2(int, operator!=)
T2(int, operator>=)
T2(int, operator>)

#undef T2
#undef D

#define T1(f)\
 T F f(AI x) { return f(*A x.cv); }

T1(operator+)
T1(operator-)
T1(abs)
T1(acos)
T1(acosh)
T1(asin)
T1(asinh)
T1(atan)
T1(atanh)
T1(cos)
T1(cosh)
T1(exp)
T1(log)
T1(log10)
T1(sin)
T1(sinh)
T1(sqrt)
T1(tan)
T1(tanh)
T1(fabs)

#undef T1
#undef AI
#undef Ai
#undef F
#undef T
#undef A

#ifndef SACADO_NO_NAMESPACE
} // namespace Rad2d
} // namespace Sacado
#endif // SACADO_NAMESPACE
#endif // SACADO_RAD2_H
