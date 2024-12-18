// $Id$ 
// @HEADER
// ***********************************************************************
// 
//                           Sacado Package
//                 Copyright (2006) Sandia Corporation
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

#ifndef SACADO_TRAD_TRAITS_HPP
#define SACADO_TRAD_TRAITS_HPP

#include "Sacado_Traits.hpp"

// Forward declarations
namespace Sacado {
  namespace Rad {
    template <typename T> class ADvar;
    template <typename T> class ADvari;
  }
}

namespace Sacado {

  //! Specialization of %Promote to ADvar types
  template <typename T>
  class Promote< Rad::ADvar<T>, Rad::ADvar<T> > {
  public:

    typedef Rad::ADvar<T> type;
  };

  //! Specialization of %Promote to ADvar types
  template <typename L, typename R>
  class Promote< Rad::ADvar<L>, R > {
  public:

    typedef typename ValueType< Rad::ADvar<L> >::type value_type_l;
    typedef typename ValueType<R>::type value_type_r;
    typedef typename Promote<value_type_l,value_type_r>::type value_type;

    typedef Rad::ADvar<value_type> type;
  };

  //! Specialization of %Promote to ADvar types
  template <typename L, typename R>
  class Promote< L, Rad::ADvar<R> > {
  public:

    typedef typename ValueType<L>::type value_type_l;
    typedef typename ValueType< Rad::ADvar<R> >::type value_type_r;
    typedef typename Promote<value_type_l,value_type_r>::type value_type;

    typedef Rad::ADvar<value_type> type;
  };

  //! Specialization of %ScalarType to ADvar types
  template <typename T>
  struct ScalarType< Rad::ADvar<T> > {
    typedef T type;
  };

  //! Specialization of %ScalarType to ADvari types
  template <typename T>
  struct ScalarType< Rad::ADvari<T> > {
    typedef T type;
  };

  //! Specialization of %ValueType to ADvar types
  template <typename T>
  struct ValueType< Rad::ADvar<T> > {
    typedef T type;
  };

  //! Specialization of %ValueType to ADvari types
  template <typename T>
  struct ValueType< Rad::ADvari<T> > {
    typedef T type;
  };

  //! Specialization of %ScalarValueType to ADvar types
  template <typename T>
  struct ScalarValueType< Rad::ADvar<T> > {
    typedef typename ScalarValueType< T >::type type;
  };

  //! Specialization of %ScalarValueType to ADvari types
  template <typename T>
  struct ScalarValueType< Rad::ADvari<T> > {
    typedef typename ScalarValueType< T >::type type;
  };

  //! Specialization of %IsADType to ADvar types
  template <typename T>
  struct IsADType< Rad::ADvar<T> > {
    static const bool value = true;
  };

  //! Specialization of %IsADType to ADvari types
  template <typename T>
  struct IsADType< Rad::ADvari<T> > {
    static const bool value = true;
  };

  //! Specialization of %IsADType to ADvar types
  template <typename T>
  struct IsScalarType< Rad::ADvar<T> > {
    static const bool value = false;
  };

  //! Specialization of %IsADType to ADvari types
  template <typename T>
  struct IsScalarType< Rad::ADvari<T> > {
    static const bool value = false;
  };

  //! Specialization of %Value to ADvar types
  template <typename T>
  struct Value< Rad::ADvar<T> > {
    typedef typename ValueType< Rad::ADvar<T> >::type value_type;
    static const value_type& eval(const Rad::ADvar<T>& x) { 
      return x.val(); }
  };

  //! Specialization of %Value to ADvari types
  template <typename T>
  struct Value< Rad::ADvari<T> > {
    typedef typename ValueType< Rad::ADvari<T> >::type value_type;
    static const value_type& eval(const Rad::ADvari<T>& x) { 
      return x.val(); }
  };

  //! Specialization of %MarkConstant to ADvar types
  template <typename T> 
  struct MarkConstant< Rad::ADvar<T> > {
    static void eval(Rad::ADvar<T>& x) { AD_Const(x); }
  };

  //! Specialization of %MarkConstant to ADvari types
  template <typename T> 
  struct MarkConstant< Rad::ADvari<T> > {
    static void eval(Rad::ADvari<T>& x) { AD_Const(x); }
  };

} // namespace Sacado

#endif // SACADO_TRAD_TRAITS_HPP
