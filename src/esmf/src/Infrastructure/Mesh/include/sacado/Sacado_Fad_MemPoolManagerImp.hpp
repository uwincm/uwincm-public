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

template <typename T>
inline
Sacado::Fad::MemPoolManager<T>::MemPoolManager(unsigned int nfad) : 
  num_fad(nfad), 
  poolMap() 
{
}

template <typename T>
inline
Sacado::Fad::MemPoolManager<T>::~MemPoolManager() 
{
  for (typename MapType::iterator i = poolMap.begin(); 
       i != poolMap.end(); ++i)
    delete i->second;
}

template <typename T>
inline Sacado::Fad::MemPool*
Sacado::Fad::MemPoolManager<T>::getMemoryPool(unsigned int dim) {

  // Find the memory pool
  typename MapType::iterator i = poolMap.find(dim);
  if (i != poolMap.end())
    return i->second;

  // Create a new pool
  unsigned int pre_alloc = 0;
  if (poolMap.begin() != poolMap.end())
    pre_alloc = poolMap.begin()->second->numChunks();
  MemPool* pool = new MemPool(sizeof(T)*dim, num_fad, pre_alloc);
  poolMap[dim] = pool;

  return pool;
}
