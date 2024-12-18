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

#include "FileTokenizer.hpp"
#include "moab/ReadUtilIface.hpp"
#include <cstring>
#include <cctype>
#include <string>
#include <cstdlib>

namespace moab {

using namespace std;

FileTokenizer::FileTokenizer( FILE* file_ptr, ReadUtilIface* rif_ptr )
  : filePtr( file_ptr ),
    readUtilPtr( rif_ptr ),
    nextToken( buffer ),
    bufferEnd( buffer ),
    lineNumber( 1 ),
    lastChar( '\0' )
  {}
  
FileTokenizer::~FileTokenizer() 
  { fclose( filePtr ); }

bool FileTokenizer::eof() const
  { return nextToken == bufferEnd && feof(filePtr); }

const char* FileTokenizer::get_string( )
{
    // If the whitepsace character marking the end of the
    // last token was a newline, increment the line count.
  if (lastChar == '\n')
    ++lineNumber;
  
    // Loop until either found the start of a token to return or have
    // reached the end of the file.
  for (;;)
  {
      // If the buffer is empty, read more.
    if (nextToken == bufferEnd)
    {
      size_t count = fread( buffer, 1, sizeof(buffer) - 1, filePtr );
      if (!count)
      {
        if (feof(filePtr))
          readUtilPtr->report_error( "File truncated at line %d\n", line_number() );
        else
          readUtilPtr->report_error( "I/O Error\n" );
        return NULL;
      }
      
      nextToken = buffer;
      bufferEnd = buffer + count;
    }
    
      // If the current character is not a space, we've found a token.
    if (!isspace(*nextToken))
      break;
      
      // If the current space character is a newline,
      // increment the line number count.
    if (*nextToken == '\n')
      ++lineNumber;
    ++nextToken;
  }
  
    // Store the start of the token in "result" and
    // advance "nextToken" to one past the end of the
    // token.
  char* result = nextToken;
  while (nextToken != bufferEnd && !isspace(*nextToken))
    ++nextToken;
  
    // If we have reached the end of the buffer without finding
    // a whitespace character terminating the token, we need to
    // read more from the file.  Only try once.  If the token is
    // too large to fit in the buffer, give up.
  if (nextToken == bufferEnd)
  {
      // Shift the (possibly) partial token to the start of the buffer.
    size_t remaining = bufferEnd - result;
    memmove( buffer, result, remaining );
    result = buffer;
    nextToken =  result + remaining;
    
      // Fill the remainder of the buffer after the token.
    size_t count = fread( nextToken, 1, sizeof(buffer) - remaining - 1, filePtr );
    if (!count && !feof(filePtr))
    {
      readUtilPtr->report_error( "I/O Error\n" );
      return NULL;
    }
    bufferEnd = nextToken + count;
    
      // Continue to advance nextToken until we find the space
      // terminating the token.
    while (nextToken != bufferEnd && !isspace(*nextToken))
      ++nextToken;
  
    if (nextToken == bufferEnd) // EOF
    {
      *bufferEnd = '\0';
      ++bufferEnd;
    }
  }
  
    // Save terminating whitespace character (or NULL char if EOF).
  lastChar = *nextToken;
    // Put null in buffer to mark end of current token.
  *nextToken = '\0';
    // Advance nextToken to the next character to search next time.
  ++nextToken;
  return result;
}

bool FileTokenizer::get_double_internal( double& result )
{
    // Get a token
  const char *token_end, *token = get_string( );
  if (!token)
    return false;
  
    // Check for hex value -- on some platforms (e.g. Linux), strtod
    // will accept hex values, on others (e.g. Sun) it wil not.  Force
    // failure on hex numbers for consistancy.
  if (token[0] && token[1] && token[0] == '0' && toupper(token[1]) == 'X')
  {
    readUtilPtr->report_error(
      "Syntax error at line %d: expected number, got \"%s\"",
      line_number(), token );
    return false;
  }
  
  
    // Parse token as double
  result = strtod( token, (char**)&token_end );

    // If the one past the last char read by strtod is
    // not the NULL character terminating the string,
    // then parse failed.
  if (*token_end)
  {
    readUtilPtr->report_error(
      "Syntax error at line %d: expected number, got \"%s\"",
      line_number(), token );
    return false;
  }
  
  return true;
}

bool FileTokenizer::get_float_internal( float& result )
{
  double d;
  if (!get_double_internal( d ))
    return false;
  
  result = (float)d;
  return true;
}

bool FileTokenizer::get_long_int_internal( long& result )
{
    // Get a token
  const char *token_end, *token = get_string( );
  if (!token)
    return false;
  
    // Parse token as long
  result = strtol( token, (char**)&token_end, 0 );

    // If the one past the last char read by strtol is
    // not the NULL character terminating the string,
    // then parse failed.
  if (*token_end)
  {
    readUtilPtr->report_error(
      "Syntax error at line %d: expected integer, got \"%s\"",
      line_number(), token );
    return false;
  }

  return true;
}

bool FileTokenizer::get_byte_internal( unsigned char& result )
{
  long i;
  if (!get_long_int_internal( i ))
    return false;
  
  result = (unsigned char)i;
  if (i != (long)result)
  {
    readUtilPtr->report_error( "Numberic overflow at line %d.", line_number() );
    return false;
  }
  
  return true;
}

bool FileTokenizer::get_short_int_internal( short& result )
{
  long i;
  if (!get_long_int_internal( i ))
    return false;
  
  result = (short)i;
  if (i != (long)result)
  {
    readUtilPtr->report_error( "Numberic overflow at line %d.", line_number() );
    return false;
  }
  
  return true;
}

bool FileTokenizer::get_integer_internal( int& result )
{
  long i;
  if (!get_long_int_internal( i ))
    return false;
  
  result = (int)i;
  if (i != (long)result)
  {
    readUtilPtr->report_error( "Numberic overflow at line %d.", line_number() );
    return false;
  }
  
  return true;
}

bool FileTokenizer::get_boolean_internal( bool& result )
{
    // Get a token
  const char *token = get_string( );
  if (!token)
    return false;
  
  if (token[1] || (token[0] != '0' && token[0] != '1'))
  {
    readUtilPtr->report_error( 
      "Syntax error at line %d: expected 0 or 1, got \"%s\"",
      line_number(), token );
    return false;
  }

  result = token[0] == '1';
  return true;
}

bool FileTokenizer::get_floats( size_t count, float* array )
{
  for (size_t i = 0; i < count; ++i)
  {
    if (!get_float_internal( *array ))
      return false;
    ++array;
  }
  return true;
}

bool FileTokenizer::get_doubles( size_t count, double* array )
{
  for (size_t i = 0; i < count; ++i)
  {
    if (!get_double_internal( *array ))
      return false;
    ++array;
  }
  return true;
}

bool FileTokenizer::get_bytes( size_t count, unsigned char* array )
{
  for (size_t i = 0; i < count; ++i)
  {
    if (!get_byte_internal( *array ))
      return false;
    ++array;
  }
  return true;
}

bool FileTokenizer::get_short_ints( size_t count, short* array )
{
  for (size_t i = 0; i < count; ++i)
  {
    if (!get_short_int_internal( *array ))
      return false;
    ++array;
  }
  return true;
}


bool FileTokenizer::get_integers( size_t count, int* array )
{
  for (size_t i = 0; i < count; ++i)
  {
    if (!get_integer_internal( *array ))
      return false;
    ++array;
  }
  return true;
}

bool FileTokenizer::get_long_ints( size_t count, long* array )
{
  for (size_t i = 0; i < count; ++i)
  {
    if (!get_long_int_internal( *array ))
      return false;
    ++array;
  }
  return true;
}

bool FileTokenizer::get_booleans( size_t count, bool* array )
{
  for (size_t i = 0; i < count; ++i)
  {
    if (!get_boolean_internal( *array ))
      return false;
    ++array;
  }
  return true;
}

void FileTokenizer::unget_token()
{
  if (nextToken - buffer < 2)
    return;
  
  --nextToken;
  *nextToken = lastChar;
  --nextToken;
  while (nextToken > buffer && *nextToken)
    --nextToken;
    
  if (!*nextToken)
    ++nextToken;
    
  lastChar = '\0';
}

bool FileTokenizer::match_token( const char* str, bool print_error )
{
    // Get a token
  const char *token = get_string( );
  if (!token)
    return false;

    // Check if it matches
  if (0 == strcmp( token, str ))
    return true;
  
    // Construct error message
  if (print_error)
    readUtilPtr->report_error( "Syntax error at line %d: expected \"%s\", got \"%s\"",
                                line_number(), str, token );
  return false;
}  // namespace Mesquite


int FileTokenizer::match_token( const char* const* list, bool print_error )
{
    // Get a token
  const char *token = get_string( );
  if (!token)
    return false;

    // Check if it matches any input string
  const char* const* ptr;
  for (ptr = list; *ptr; ++ptr)
    if (0 == strcmp( token, *ptr ))
      return ptr - list + 1;
  
  if (!print_error)
    return false;
  
    // No match, constuct error message
  std::string message( "Parsing error at line " );
  char lineno[16];
  sprintf( lineno, "%d", line_number() );
  message += lineno;
  message += ": expected one of {";
  for (ptr = list; *ptr; ++ptr)
  {
    message += " ";
    message += *ptr;
  }
  message += " } got \"";
  message += token;
  message += "\"";
  readUtilPtr->report_error( message );
  return false;
}

bool FileTokenizer::get_newline( )
{
  if (lastChar == '\n')
  {
    lastChar = ' ';
    ++lineNumber;
    return true;
  }
  
    // Loop until either we a) find a newline, b) find a non-whitespace
    // character or c) reach the end of the file.
  for (;;)
  {
      // If the buffer is empty, read more.
    if (nextToken == bufferEnd)
    {
      size_t count = fread( buffer, 1, sizeof(buffer), filePtr );
      if (!count)
      {
        if (eof())
          readUtilPtr->report_error( "File truncated at line %d.", line_number() );
        else
          readUtilPtr->report_error( "I/O Error" );
        break;
      }
      
      nextToken = buffer;
      bufferEnd = buffer + count;
    }
    
      // If the current character is not a space, the we've failed.
    if (!isspace(*nextToken))
    {
      readUtilPtr->report_error( "Expected newline at line %d.", line_number() );
      break;
    }
      
      // If the current space character is a newline,
      // increment the line number count.
    if (*nextToken == '\n')
    {
      ++lineNumber;
      ++nextToken;
      lastChar = ' ';
      return true;
    }
    ++nextToken;
  }
  
  return false;
}

bool FileTokenizer::get_binary( size_t size, void* mem )
{
    // if data in buffer
  if (nextToken != bufferEnd) {
      // if requested size is less than buffer contents,
      // just pass back part of the buffer
    if (bufferEnd - nextToken <= (int)size) {
      memcpy( mem, nextToken, size );
      nextToken += size;
      return true;
    }
    
      // copy buffer contents into memory and clear buffer
    memcpy( mem, nextToken, bufferEnd - nextToken );
    size -= bufferEnd - nextToken;
    mem = reinterpret_cast<char*>(mem) + (bufferEnd - nextToken);
    nextToken = bufferEnd;
  }
  
    // read any additional data from file
  return size == fread( mem, 1, size, filePtr );
}


} // namespace moab
