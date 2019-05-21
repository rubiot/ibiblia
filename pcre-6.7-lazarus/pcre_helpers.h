// $Id: pcre_helpers.h,v 1.1 2005/06/27 11:35:17 Renato Exp $

#pragma once

#ifdef __cplusplus
    extern "C" {
#endif

extern int pcre_major( void );
extern int pcre_minor( void );
extern char const* pcre_date( void );

extern void* pcre_malloc_ex( size_t n );
extern void pcre_free_ex( void* p );

typedef void* ( *PCRE_MALLOC )( size_t );
typedef void  ( *PCRE_FREE )( void* );
typedef void* ( *PCRE_STACK_MALLOC )( size_t );
typedef void  ( *PCRE_STACK_FREE )( void * );

extern PCRE_MALLOC pcre_set_malloc( PCRE_MALLOC proc );
extern PCRE_FREE pcre_set_free( PCRE_FREE proc );
extern PCRE_STACK_MALLOC pcre_set_stack_malloc( PCRE_STACK_MALLOC proc );
extern PCRE_STACK_FREE pcre_set_stack_free( PCRE_STACK_FREE proc );

// locale manipulation

extern char const* pcre_setlocale( int category, char const* locale );

extern int pcre_isalnum( int c );
extern int pcre_isalpha( int c );
extern int pcre_iscntrl( int c );
extern int pcre_isdigit( int c );
extern int pcre_isgraph( int c );
extern int pcre_islower( int c );
extern int pcre_isprint( int c );
extern int pcre_ispunct( int c );
extern int pcre_isspace( int c );
extern int pcre_isupper( int c );
extern int pcre_isxdigit( int c );


// callout facility
typedef int ( *PCRE_CALLOUT_CALLBACK )( pcre_callout_block * );

extern PCRE_CALLOUT_CALLBACK pcre_set_callout_handler( PCRE_CALLOUT_CALLBACK handler ); 
extern int pcre_extra_set_match_limit( pcre_extra** extraptr, long value );
extern int pcre_extra_set_callout_data( pcre_extra** extraptr, void* data ); 

#ifdef __cplusplus
    }  /* extern "C" */
#endif
