#include <iostream>
#include <stddef.h>

#include "pcre.h"
#include "pcreposix.h"
#include "pcre_helpers.h"

using namespace std;


int main()
{
	//
	// Version Info
	//
	cout << "PCRE_MAJOR = " << PCRE_MAJOR << endl;
	cout << "PCRE_MINOR = " << PCRE_MINOR << endl;
	cout << "PCRE_DATE = "  << pcre_date() << endl;
	cout << endl;

	//
	// Options
	//
	cout << "PCRE_CASELESS = " << PCRE_CASELESS << endl;
	cout << "PCRE_MULTILINE = " << PCRE_MULTILINE << endl;
	cout << "PCRE_DOTALL = " << PCRE_DOTALL << endl;
	cout << "PCRE_ANCHORED = " << PCRE_ANCHORED << endl;
	cout << "PCRE_DOLLAR_ENDONLY = " << PCRE_DOLLAR_ENDONLY << endl;
	cout << "PCRE_EXTRA = " << PCRE_EXTRA << endl;
	cout << "PCRE_NOTBOL = " << PCRE_NOTBOL << endl;
	cout << "PCRE_NOTEOL = " << PCRE_NOTEOL << endl;
	cout << "PCRE_UNGREEDY = " << PCRE_UNGREEDY << endl;
	cout << "PCRE_NOTEMPTY = " << PCRE_NOTEMPTY << endl;
	cout << "PCRE_UTF8 = " << PCRE_UTF8 << endl;
	cout << "PCRE_NO_AUTO_CAPTURE = " << PCRE_NO_AUTO_CAPTURE << endl;
	cout << "PCRE_NO_UTF8_CHECK = " << PCRE_NO_UTF8_CHECK << endl;
	cout << "PCRE_AUTO_CALLOUT = " << PCRE_AUTO_CALLOUT << endl;
	cout << "PCRE_PARTIAL = " << PCRE_PARTIAL << endl;
    cout << "PCRE_DFA_SHORTEST = " << PCRE_DFA_SHORTEST << endl;
    cout << "PCRE_DFA_RESTART = " << PCRE_DFA_RESTART << endl;
    cout << "PCRE_FIRSTLINE = " << PCRE_FIRSTLINE << endl;
	cout << "PCRE_DUPNAMES = " << PCRE_DUPNAMES << endl;
	cout << "PCRE_NEWLINE_CR = " << PCRE_NEWLINE_CR << endl;
	cout << "PCRE_NEWLINE_LF = " << PCRE_NEWLINE_LF << endl;
	cout << "PCRE_NEWLINE_CRLF = " << PCRE_NEWLINE_CRLF << endl;
	cout << endl;

	//
	// Exec-time and get/set-time error codes
	//
	cout << "PCRE_ERROR_NOMATCH = " << PCRE_ERROR_NOMATCH << endl;
	cout << "PCRE_ERROR_NULL = " << PCRE_ERROR_NULL << endl;
	cout << "PCRE_ERROR_BADOPTION = " << PCRE_ERROR_BADOPTION << endl;
	cout << "PCRE_ERROR_BADMAGIC = " << PCRE_ERROR_BADMAGIC << endl;
	cout << "PCRE_ERROR_UNKNOWN_NODE = " << PCRE_ERROR_UNKNOWN_NODE << endl;
	cout << "PCRE_ERROR_NOMEMORY = " << PCRE_ERROR_NOMEMORY << endl;
	cout << "PCRE_ERROR_NOSUBSTRING = " << PCRE_ERROR_NOSUBSTRING << endl;
	cout << "PCRE_ERROR_MATCHLIMIT = " << PCRE_ERROR_MATCHLIMIT << endl;
	cout << "PCRE_ERROR_CALLOUT = " << PCRE_ERROR_CALLOUT << endl;
	cout << "PCRE_ERROR_BADUTF8 = " << PCRE_ERROR_BADUTF8 << endl;
	cout << "PCRE_ERROR_BADUTF8_OFFSET = " << PCRE_ERROR_BADUTF8_OFFSET << endl;
	cout << "PCRE_ERROR_PARTIAL = " << PCRE_ERROR_PARTIAL << endl;
	cout << "PCRE_ERROR_BADPARTIAL = " << PCRE_ERROR_BADPARTIAL << endl;
	cout << "PCRE_ERROR_INTERNAL = " << PCRE_ERROR_INTERNAL << endl;
	cout << "PCRE_ERROR_BADCOUNT = " << PCRE_ERROR_BADCOUNT << endl;
    cout << "PCRE_ERROR_DFA_UITEM = " << PCRE_ERROR_DFA_UITEM << endl;
    cout << "PCRE_ERROR_DFA_UCOND = " << PCRE_ERROR_DFA_UCOND << endl;
    cout << "PCRE_ERROR_DFA_UMLIMIT = " << PCRE_ERROR_DFA_UMLIMIT << endl;
    cout << "PCRE_ERROR_DFA_WSSIZE = " << PCRE_ERROR_DFA_WSSIZE << endl;
    cout << "PCRE_ERROR_DFA_RECURSE = " << PCRE_ERROR_DFA_RECURSE << endl;
	cout << "PCRE_ERROR_RECURSIONLIMIT = " << PCRE_ERROR_RECURSIONLIMIT << endl;
	cout << endl;

	//
	// Request types for pcre_fullinfo()
	//
	cout << "PCRE_INFO_OPTIONS = " << PCRE_INFO_OPTIONS << endl;
	cout << "PCRE_INFO_SIZE = " << PCRE_INFO_SIZE << endl;
	cout << "PCRE_INFO_CAPTURECOUNT = " << PCRE_INFO_CAPTURECOUNT << endl;
	cout << "PCRE_INFO_BACKREFMAX = " << PCRE_INFO_BACKREFMAX << endl;
	cout << "PCRE_INFO_FIRSTBYTE = " << PCRE_INFO_FIRSTBYTE << endl;
	cout << "PCRE_INFO_FIRSTCHAR = " << PCRE_INFO_FIRSTCHAR << endl;
	cout << "PCRE_INFO_FIRSTTABLE = " << PCRE_INFO_FIRSTTABLE << endl;
	cout << "PCRE_INFO_LASTLITERAL = " << PCRE_INFO_LASTLITERAL << endl;
	cout << "PCRE_INFO_NAMEENTRYSIZE = " << PCRE_INFO_NAMEENTRYSIZE << endl;
	cout << "PCRE_INFO_NAMECOUNT = " << PCRE_INFO_NAMECOUNT << endl;
	cout << "PCRE_INFO_NAMETABLE = " << PCRE_INFO_NAMETABLE << endl;
	cout << "PCRE_INFO_STUDYSIZE = " << PCRE_INFO_STUDYSIZE << endl;
	cout << "PCRE_INFO_DEFAULT_TABLES = " << PCRE_INFO_DEFAULT_TABLES << endl;
	cout << endl;

	//
	// Request types for pcre_config()
	//
	cout << "PCRE_CONFIG_UTF8 = " << PCRE_CONFIG_UTF8 << endl;
	cout << "PCRE_CONFIG_NEWLINE = " << PCRE_CONFIG_NEWLINE << endl;
	cout << "PCRE_CONFIG_LINK_SIZE = " << PCRE_CONFIG_LINK_SIZE << endl;
	cout << "PCRE_CONFIG_POSIX_MALLOC_THRESHOLD = " << PCRE_CONFIG_POSIX_MALLOC_THRESHOLD << endl;
	cout << "PCRE_CONFIG_MATCH_LIMIT = " << PCRE_CONFIG_MATCH_LIMIT << endl;
	cout << "PCRE_CONFIG_STACKRECURSE = " << PCRE_CONFIG_STACKRECURSE << endl;
	cout << "PCRE_CONFIG_UNICODE_PROPERTIES = " << PCRE_CONFIG_UNICODE_PROPERTIES << endl;
	cout << "PCRE_CONFIG_MATCH_LIMIT_RECURSION = " << PCRE_CONFIG_MATCH_LIMIT_RECURSION << endl;
	cout << endl;

	//	
	// Bit flags for the pcre_extra structure 
	//
	cout << "PCRE_EXTRA_STUDY_DATA = " << PCRE_EXTRA_STUDY_DATA << endl;
	cout << "PCRE_EXTRA_MATCH_LIMIT = " << PCRE_EXTRA_MATCH_LIMIT << endl;
	cout << "PCRE_EXTRA_CALLOUT_DATA = " << PCRE_EXTRA_CALLOUT_DATA << endl;
	cout << "PCRE_EXTRA_TABLES = " << PCRE_EXTRA_TABLES << endl;
	cout << "PCRE_EXTRA_MATCH_LIMIT_RECURSION = " << PCRE_EXTRA_MATCH_LIMIT_RECURSION << endl;
	cout << endl;

	//
	// The structure for passing additional data to pcre_exec(). This is defined in
	//	such as way as to be extensible.
	//
	cout << "sizeof(pcre_extra) = " << sizeof(pcre_extra) << endl;
	cout << "  offset of pcre_extra.flags = " << offsetof(pcre_extra, flags) << endl;
	cout << "  offset of pcre_extra.study_data = " << offsetof(pcre_extra, study_data) << endl;
	cout << "  offset of pcre_extra.match_limit = " << offsetof(pcre_extra, match_limit) << endl;
	cout << "  offset of pcre_extra.callout_data = " << offsetof(pcre_extra, callout_data) << endl;
	cout << "  offset of pcre_extra.tables = " << offsetof(pcre_extra, tables) << endl;
	cout << "  offset of pcre_extra.match_limit_recursion = " << offsetof(pcre_extra, match_limit_recursion) << endl;
	cout << endl;

	//
	// The structure for passing out data via the pcre_callout_function. We use a
	// structure so that new fields can be added on the end in future versions,
	// without changing the API of the function, thereby allowing old clients to work
	// without modification.
	//
	cout << "sizeof(pcre_callout_block) = " << sizeof(pcre_callout_block) << endl;
	cout << "  offset of pcre_callout_block.version = " << offsetof(pcre_callout_block, version) << endl;
	cout << "  offset of pcre_callout_block.callout_number = " << offsetof(pcre_callout_block, callout_number) << endl;
	cout << "  offset of pcre_callout_block.offset_vector = " << offsetof(pcre_callout_block, offset_vector) << endl;
	cout << "  offset of pcre_callout_block.subject = " << offsetof(pcre_callout_block, subject) << endl;
	cout << "  offset of pcre_callout_block.subject_length = " << offsetof(pcre_callout_block, subject_length) << endl;
	cout << "  offset of pcre_callout_block.start_match = " << offsetof(pcre_callout_block, start_match) << endl;
	cout << "  offset of pcre_callout_block.current_position = " << offsetof(pcre_callout_block, current_position) << endl;
	cout << "  offset of pcre_callout_block.capture_top = " << offsetof(pcre_callout_block, capture_top) << endl;
	cout << "  offset of pcre_callout_block.capture_last = " << offsetof(pcre_callout_block, capture_last) << endl;
	cout << "  offset of pcre_callout_block.callout_data = " << offsetof(pcre_callout_block, callout_data) << endl;	
	cout << "  offset of pcre_callout_block.pattern_position = " << offsetof(pcre_callout_block, pattern_position) << endl;		
	cout << "  offset of pcre_callout_block.next_item_length = " << offsetof(pcre_callout_block, next_item_length) << endl;	
	cout << endl;

	//
	// Options defined by POSIX.
	//
	cout << "REG_ICASE = " << REG_ICASE << endl;
	cout << "REG_NEWLINE = " << REG_NEWLINE << endl;
	cout << "REG_NOTBOL = " << REG_NOTBOL << endl;
	cout << "REG_NOTEOL = " << REG_NOTEOL << endl;

	//
	// These are not used by PCRE, but by defining them we make it easier
	// to slot PCRE into existing programs that make POSIX calls.
	//
	cout << "REG_EXTENDED = " << REG_EXTENDED << endl;
	cout << "REG_NOSUB = " << REG_NOSUB << endl;

	//
	// Error values. Not all these are relevant or used by the wrapper.
	//
	cout << "REG_ASSERT = " << REG_ASSERT << endl;
	cout << "REG_BADBR = " << REG_BADBR << endl;
	cout << "REG_BADPAT = " << REG_BADPAT << endl;
	cout << "REG_BADRPT = " << REG_BADRPT << endl;
	cout << "REG_EBRACE = " << REG_EBRACE << endl;
	cout << "REG_EBRACK = " << REG_EBRACK << endl;
	cout << "REG_ECOLLATE = " << REG_ECOLLATE << endl;
	cout << "REG_ECTYPE = " << REG_ECTYPE << endl;
	cout << "REG_EESCAPE = " << REG_EESCAPE << endl;
	cout << "REG_EMPTY = " << REG_EMPTY << endl;
	cout << "REG_EPAREN = " << REG_EPAREN << endl;
	cout << "REG_ERANGE = " << REG_ERANGE << endl;
	cout << "REG_ESIZE = " << REG_ESIZE << endl;
	cout << "REG_ESPACE = " << REG_ESPACE << endl;
	cout << "REG_ESUBREG = " << REG_ESUBREG << endl;
	cout << "REG_INVARG = " << REG_INVARG << endl;
	cout << "REG_NOMATCH = " << REG_NOMATCH << endl;
	cout << endl;

	//
	// The structure representing a compiled regular expression.
	//
	cout << "sizeof(regex_t) = " << sizeof(regex_t) << endl;
	cout << "  offset of regex_t.re_pcre = " << offsetof(regex_t, re_pcre) << endl;
	cout << "  offset of regex_t.re_nsub = " << offsetof(regex_t, re_nsub) << endl;
	cout << "  offset of regex_t.re_erroffset = " << offsetof(regex_t, re_erroffset) << endl;
	cout << endl;

	//
	// The structure in which a captured offset is returned.
	//
	cout << "sizeof(regmatch_t) = " << sizeof(regmatch_t) << endl;
	cout << "  offset of regmatch_t.rm_so = " << offsetof(regmatch_t, rm_so) << endl;
	cout << "  offset of regmatch_t.rm_eo = " << offsetof(regmatch_t, rm_eo) << endl;
	cout << endl;
}