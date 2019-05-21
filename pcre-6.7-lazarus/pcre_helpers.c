// $Id: pcre_helpers.c,v 1.2 2005/06/27 13:03:08 Renato Exp $

#include "pcre_internal.h"
#include "pcre_helpers.h"
#include <ctype.h>
#include <locale.h>
#include <memory.h>

#define STRING(a)  # a
#define XSTRING(s) STRING(s)

void* pcre_malloc_ex( size_t n )
{
    return pcre_malloc( n );
}

void pcre_free_ex( void* p )
{
    pcre_free( p );
}

PCRE_MALLOC pcre_set_malloc( PCRE_MALLOC proc )
{
	PCRE_MALLOC ret = pcre_malloc;
    pcre_malloc = proc;
	return ret;
}


PCRE_FREE pcre_set_free( PCRE_FREE proc )
{
	PCRE_FREE ret = pcre_free;
	pcre_free = proc;
	return ret;
}

PCRE_STACK_MALLOC pcre_set_stack_malloc( PCRE_STACK_MALLOC proc )
{
	PCRE_STACK_MALLOC ret = pcre_stack_malloc;
	pcre_stack_malloc = proc;
	return ret;
}

PCRE_STACK_FREE pcre_set_stack_free( PCRE_STACK_FREE proc )
{
	PCRE_STACK_FREE ret = pcre_stack_free;
	pcre_stack_free = proc;
	return ret;
}

char const* pcre_setlocale( int category, char const* locale )
{
    return setlocale( category, locale );
}

int pcre_isalnum( int c )
{
	return isalnum( c );
}

int pcre_isalpha( int c )
{
	return isalpha( c );
}

int pcre_iscntrl( int c )
{
	return iscntrl( c );
}

int pcre_isdigit( int c )
{
	return isdigit( c );
}	

int pcre_isgraph( int c )
{
	return isgraph( c );
}

int pcre_islower( int c )
{
	return islower( c );
}

int pcre_isprint( int c )
{
	return isprint( c );
}

int pcre_ispunct( int c )
{
	return ispunct( c );
}

int pcre_isspace( int c )
{
	return isspace( c );
}

int pcre_isupper( int c )
{
	return isupper( c );
}

int pcre_isxdigit( int c )
{
	return isxdigit( c );
}

int pcre_major( void )
{
    return PCRE_MAJOR;
}

int pcre_minor( void )
{
    return PCRE_MINOR;
}

char const* pcre_date( void )
{
    static char const* sz_date = XSTRING( PCRE_DATE );
    return sz_date;
}

PCRE_CALLOUT_CALLBACK pcre_set_callout_handler( PCRE_CALLOUT_CALLBACK handler ) 
{
    PCRE_CALLOUT_CALLBACK result = pcre_callout;
    pcre_callout = handler;
    return result;
}

static pcre_extra* create_pcre_extra()
{
    pcre_extra* p = ( pcre_extra* ) pcre_malloc( sizeof( pcre_extra ) );

    if ( NULL != p )
        memset( p, 0, sizeof( pcre_extra ) );
    
    return p;
}

int pcre_extra_set_match_limit( pcre_extra** extraptr, long value )
{
    if ( NULL == extraptr )
        return PCRE_ERROR_NULL;

    if ( NULL == *extraptr )
        *extraptr = create_pcre_extra();

    if ( NULL == *extraptr )
        return PCRE_ERROR_NOMEMORY;

    (*extraptr)->flags |= PCRE_EXTRA_MATCH_LIMIT;
    (*extraptr)->match_limit = value;

    return 0;
}

int pcre_extra_set_callout_data( pcre_extra** extraptr, void* data )
{
    if ( NULL == extraptr )
        return PCRE_ERROR_NULL;

    if ( NULL == *extraptr )
        *extraptr = create_pcre_extra();

    if ( NULL == *extraptr )
        return PCRE_ERROR_NOMEMORY;

    (*extraptr)->flags |= PCRE_EXTRA_CALLOUT_DATA;
    (*extraptr)->callout_data = data;

    return 0;
}
