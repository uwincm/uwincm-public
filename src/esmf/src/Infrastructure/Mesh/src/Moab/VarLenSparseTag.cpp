/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#include <memory.h>
#include <algorithm>

#include "VarLenSparseTag.hpp"
#include "moab/Range.hpp"
#include "TagCompare.hpp"
#include "SequenceManager.hpp"
#include "moab/Error.hpp"
#include "moab/CN.hpp"

namespace moab {

static ErrorCode not_found( Error* error, std::string name, EntityHandle h )
{
  if (error) {
    if (h)
      error->set_last_error( "No tag %s value for %s %lu", 
                             name.c_str(),
                             CN::EntityTypeName(TYPE_FROM_HANDLE(h)), 
                             (unsigned long)ID_FROM_HANDLE(h));
    else
      error->set_last_error( "No tag %s value for root set", name.c_str());
  }
  return MB_TAG_NOT_FOUND;
}

static ErrorCode not_var_len( Error* error, std::string name )
{
  error->set_last_error( "No size specified for variable-length tag %s data", name.c_str());
  return MB_VARIABLE_DATA_LENGTH;
}

VarLenSparseTag::VarLenSparseTag( const char* name,
                                  DataType type,
                                  const void* default_value,
                                  int default_value_bytes )
  : TagInfo( name, MB_VARIABLE_LENGTH, type, default_value, default_value_bytes )
  { }

VarLenSparseTag::~VarLenSparseTag()
  { release_all_data(0,0,true); }

TagType VarLenSparseTag::get_storage_type() const 
  { return MB_TAG_SPARSE; }

ErrorCode VarLenSparseTag::release_all_data( SequenceManager*, Error*, bool )
{
  mData.clear();
  return MB_SUCCESS;
}

ErrorCode VarLenSparseTag::get_data_ptr( Error* error,
                                         EntityHandle entity_handle, 
                                         const void*& ptr,
                                         int& length ) const
{
  MapType::const_iterator iter = mData.find(entity_handle);

  if (iter != mData.end()) {
    ptr = iter->second.data();
    length = iter->second.size();
  }
  else if (get_default_value()) {
    ptr = get_default_value();
    length = get_default_value_size();
  }
  else 
    return not_found(error, get_name(), entity_handle);
  
  return MB_SUCCESS;
}

ErrorCode VarLenSparseTag::get_data( const SequenceManager*,
                                     Error* error,
                                     const EntityHandle* ,
                                     size_t ,
                                     void*  ) const
{
  return not_var_len(error, get_name());
}

ErrorCode VarLenSparseTag::get_data( const SequenceManager*,
                                     Error* error,
                                     const Range& /*entities*/,
                                     void* /*data */) const
{
  return not_var_len(error, get_name());
}

ErrorCode VarLenSparseTag::get_data( const SequenceManager* ,
                                     Error* error,
                                     const EntityHandle* entities,
                                     size_t num_entities,
                                     const void** pointers,
                                     int* lengths ) const
{
  if (!lengths)
    return not_var_len(error, get_name());

  ErrorCode rval;
  for (size_t i = 0; i < num_entities; ++i)
    if (MB_SUCCESS != (rval = get_data_ptr(error, entities[i], pointers[i], lengths[i])))
      return rval;
  return MB_SUCCESS;
}
 
ErrorCode VarLenSparseTag::get_data( const SequenceManager*,
                                     Error* error,
                                     const Range& entities,
                                     const void** pointers,
                                     int* lengths ) const
{
  if (!lengths)
    return not_var_len(error, get_name());

  ErrorCode rval;
  Range::const_iterator i;
  for (i = entities.begin(); i != entities.end(); ++i, ++pointers, ++lengths)
    if (MB_SUCCESS != (rval = get_data_ptr(error, *i, *pointers, *lengths)))
      return rval;
  return MB_SUCCESS;
}

ErrorCode VarLenSparseTag::set_data( SequenceManager* /*seqman*/,
                                     Error* error,
                                     const EntityHandle* /*entities*/,
                                     size_t /*num_entities*/,
                                     const void* /*data*/ )
{
  return not_var_len(error, get_name());
}

ErrorCode VarLenSparseTag::set_data( SequenceManager* /*seqman*/,
                                     Error* error,
                                     const Range& /*entities*/,
                                     const void* /*data */)
{
  return not_var_len(error, get_name());
}

ErrorCode VarLenSparseTag::set_data( SequenceManager* seqman,
                                     Error* error,
                                     const EntityHandle* entities,
                                     size_t num_entities,
                                     void const* const* pointers,
                                     const int* lengths )
{
  ErrorCode rval = validate_lengths( error, lengths, num_entities );
  if (MB_SUCCESS != rval)
    return rval;
    
  rval = seqman->check_valid_entities( error, entities, num_entities, true );
  if (MB_SUCCESS != rval)
    return rval;
  
  for (size_t i = 0; i < num_entities; ++i) {
    if (lengths[i])
      mData[entities[i]].set( pointers[i], lengths[i] );
    else {
      MapType::iterator iter = mData.find(entities[i]);
      if (iter != mData.end()) {
        iter->second.clear();
        mData.erase(iter);
      }
    }
  }
  return MB_SUCCESS;
}

ErrorCode VarLenSparseTag::set_data( SequenceManager* seqman,
                                     Error* error,
                                     const Range& entities,
                                     void const* const* pointers,
                                     const int* lengths )
{
  ErrorCode rval = validate_lengths( error, lengths, entities.size() );
  if (MB_SUCCESS != rval)
    return rval;
    
  rval = seqman->check_valid_entities( error, entities );
  if (MB_SUCCESS != rval)
    return rval;
  
  Range::const_iterator i;
  for (i = entities.begin(); i != entities.end(); ++i, ++pointers, ++lengths) {
    if (*lengths)
      mData[*i].set( *pointers, *lengths );
    else {
      MapType::iterator iter = mData.find(*i);
      if (iter != mData.end()) {
        iter->second.clear();
        mData.erase(iter);
      }
    }
  }
  return MB_SUCCESS;
}

ErrorCode VarLenSparseTag::clear_data( SequenceManager* seqman,
                                       Error* error,
                                       const EntityHandle* entities,
                                       size_t num_entities,
                                       const void* value_ptr,
                                       int value_len )
{
  if (0 == value_len) {
    remove_data( seqman, 0, entities, num_entities );
    return MB_SUCCESS;
  }

  ErrorCode rval = validate_lengths( error, &value_len, 1 );
  if (MB_SUCCESS != rval)
    return rval;

  rval = seqman->check_valid_entities( error, entities, num_entities, true );
  if (MB_SUCCESS != rval)
    return rval;
  
  for (size_t i = 0; i < num_entities; ++i)
    mData[entities[i]].set( value_ptr, value_len );
  return MB_SUCCESS;
}

ErrorCode VarLenSparseTag::clear_data( SequenceManager* seqman,
                                       Error* error,
                                       const Range& entities,
                                       const void* value_ptr,
                                       int value_len )
{
  if (0 == value_len) {
    remove_data( seqman, 0, entities );
    return MB_SUCCESS;
  }

  ErrorCode rval = validate_lengths( error, &value_len, 1 );
  if (MB_SUCCESS != rval)
    return rval;
    
  rval = seqman->check_valid_entities( error, entities );
  if (MB_SUCCESS != rval)
    return rval;
  
  Range::const_iterator i;
  for (i = entities.begin(); i != entities.end(); ++i)
    mData[*i].set( value_ptr, value_len );

  return MB_SUCCESS;
}

ErrorCode VarLenSparseTag::remove_data( SequenceManager*,
                                        Error* error,
                                        const EntityHandle* entities,
                                        size_t num_entities )
{
  ErrorCode result = MB_SUCCESS;
  for (size_t i = 0; i < num_entities; ++i) {
    MapType::iterator p = mData.find(entities[i]);
    if (p == mData.end())
      result = not_found(error, get_name(), entities[i]);
    else {
      p->second.clear();
      mData.erase(p);
    }
  }
  return result;
}

ErrorCode VarLenSparseTag::remove_data( SequenceManager*,
                                        Error* error,
                                        const Range& entities )
{
  ErrorCode result = MB_SUCCESS;
  for (Range::iterator i = entities.begin(); i != entities.end(); ++i) {
    MapType::iterator p = mData.find(*i);
    if (p == mData.end())
      result = not_found(error, get_name(), *i);
    else {
      p->second.clear();
      mData.erase(p);
    }
  }
  return result;
}

ErrorCode VarLenSparseTag::tag_iterate( SequenceManager*,
                                        Error* error,
                                        Range::iterator&,
                                        const Range::iterator&,
                                        void*&,
                                        bool)
{
  error->set_last_error( "Cannot iterate over variable-length tag data" );
  return MB_VARIABLE_DATA_LENGTH;
}


template <class Container> static inline
void get_tagged( const VarLenSparseTag::MapType& mData,
                 EntityType type,
                 Container& output_range )
{
  VarLenSparseTag::MapType::const_iterator iter;
  typename Container::iterator hint = output_range.begin();
  if (MBMAXTYPE == type) {
    for (iter = mData.begin(); iter != mData.end(); ++iter)
      hint = output_range.insert( hint, iter->first );
  }
  else {
#ifdef HAVE_UNORDERED_MAP
    for (iter = mData.begin(); iter != mData.end(); ++iter)
      if (TYPE_FROM_HANDLE(iter->first) == type)
        hint = output_range.insert( hint, iter->first );    
#else
    iter = mData.lower_bound( FIRST_HANDLE(type) );
    VarLenSparseTag::MapType::const_iterator end = mData.lower_bound( LAST_HANDLE(type)+1 );
    for (; iter != end; ++iter)
      hint = output_range.insert( hint, iter->first );
#endif
  }
}

template <class Container> static inline
void get_tagged( const VarLenSparseTag::MapType& mData,
                 Range::const_iterator begin,
                 Range::const_iterator end,
                 Container& output_range )
{
  VarLenSparseTag::MapType::const_iterator iter;
  typename Container::iterator hint = output_range.begin();
  for (Range::const_iterator i = begin; i != end; ++i)
    if (mData.find(*i) != mData.end())
      hint = output_range.insert( hint, *i );
}

template <class Container> static inline 
void get_tagged( const VarLenSparseTag::MapType& mData,
                 Container& entities,
                 EntityType type,
                 const Range* intersect )

{
  if (!intersect)
    get_tagged<Container>( mData, type, entities );
  else if (MBMAXTYPE == type)
    get_tagged<Container>( mData, intersect->begin(), intersect->end(), entities );
  else {
    std::pair<Range::iterator,Range::iterator> r = intersect->equal_range(type);
    get_tagged<Container>( mData, r.first, r.second, entities );
  }
}


//! gets all entity handles that match a type and tag
ErrorCode VarLenSparseTag::get_tagged_entities( const SequenceManager*,
                                                Range& entities,
                                                EntityType type,
                                                const Range* intersect ) const
{
  get_tagged( mData, entities, type, intersect );
  return MB_SUCCESS;
}

//! gets all entity handles that match a type and tag
ErrorCode VarLenSparseTag::num_tagged_entities( const SequenceManager*,
                                                size_t& output_count,
                                                EntityType type,
                                                const Range* intersect ) const
{
  InsertCount counter( output_count );
  get_tagged( mData, counter, type, intersect );
  output_count = counter.end();
  return MB_SUCCESS;
}

ErrorCode VarLenSparseTag::find_entities_with_value( 
                              const SequenceManager* seqman,
                              Error*,
                              Range& output_entities,
                              const void* value,
                              int value_bytes,
                              const EntityType type,
                              const Range* intersect_entities ) const
{
  if (value_bytes && value_bytes != get_size())
    return MB_INVALID_SIZE;
  
  MapType::const_iterator iter, end;
#ifdef HAVE_UNORDERED_MAP
  if (intersect_entities) {
    std::pair<Range::iterator,Range::iterator> r;
    if (type == MBMAXTYPE) {
      r.first = intersect_entities->begin();
      r.second = intersect_entities->end();
    }
    else {
      r = intersect_entities->equal_range( type );
    }
    
    
    find_map_varlen_values_equal( *this, value, get_size(), 
                                  r.first, r.second,
                                  mData, output_entities );
  }
  else if (type == MBMAXTYPE) {
    find_tag_varlen_values_equal( *this, value, get_size(), 
                                  mData.begin(), mData.end(), 
                                  output_entities );
  }
  else {
    Range tmp;
    seqman->get_entities( type, tmp );
    find_map_varlen_values_equal( *this, value, get_size(), 
                                  tmp.begin(), tmp.end(),
                                  mData, output_entities );
  }
#else
  if (intersect_entities) {
    for (Range::const_pair_iterator p = intersect_entities->begin();
         p != intersect_entities->end(); ++p) {
      iter = mData.lower_bound( p->first );
      end = mData.upper_bound( p->second );
      find_tag_varlen_values_equal( *this, value, get_size(), iter, end, 
                                    output_entities);
    }
  }
  else {
    if (type == MBMAXTYPE) {
      iter = mData.begin();
      end = mData.end();
    }
    else {
      iter = mData.lower_bound( CREATE_HANDLE( type, MB_START_ID ) );
      end = mData.upper_bound( CREATE_HANDLE( type, MB_END_ID ) );
    }
    find_tag_varlen_values_equal( *this, value, get_size(), iter, end, 
                                  output_entities);
  }
#endif
  
  return MB_SUCCESS;
}

bool VarLenSparseTag::is_tagged( const SequenceManager*, EntityHandle h ) const
{
  return mData.find(h) != mData.end();
}
  
ErrorCode VarLenSparseTag::get_memory_use( const SequenceManager*,
                                           unsigned long& total,
                                           unsigned long& per_entity ) const

{
  total = mData.size() * (3*sizeof(void*) + sizeof(VarLenTag));
  for (MapType::const_iterator i = mData.begin(); i != mData.end(); ++i)
    total += i->second.mem();
  if (mData.size())
    per_entity = total / mData.size();
  total += sizeof(*this) + TagInfo::get_memory_use();
      
  return MB_SUCCESS;
}

} // namespace moab




