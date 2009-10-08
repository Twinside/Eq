#include <Windows.h>
#include <stdio.h>
#include "HsFFI.h"

#define EQ_API __attribute__((dllexport))

#include "formulaDll_stub.h"

extern "C" void __stginit_FormulaDll(void);

BOOL APIENTRY DllMain( HANDLE hModule
                     , DWORD  ul_reason_for_call
                     , LPVOID lpReserved
                     )
    { return TRUE; }

char *internArgv[] = { "formulaDll", "Bleh" };
char* lastResult = NULL;


/* Why you ask? Why on earth?
 * Well .net doesn't use the same heap, and if
 * we gladly give back a string allocated with
 * malloc (C Heap), things crash.
 * So we got to manage :
 * - Haskell heap
 * - C Heap
 * - .net heap
 * I'm glad you've asked.
 */
LPVOID CoTaskMemAlloc( /* __in */  SIZE_T cb );

wchar_t*    dotNetizeW( /*out*/ wchar_t *cHeapString )
{
    size_t length = wcslen( cHeapString );
    wchar_t *out = static_cast<wchar_t*>(::CoTaskMemAlloc( (length + 1) * sizeof( wchar_t ) ));

    for (int i = 0; i < length; ++i)
        out[i] = cHeapString[i];
    // just to be sure...
    out[ length ] = 0;
    eqFreeHaskellString( cHeapString );

    return out;
}

char*    dotNetize( /*out*/ char* cHeapString )
{
    size_t length = strlen( cHeapString );
    char *out = static_cast<char*>(::CoTaskMemAlloc( (length + 1) * sizeof( char ) ));

    for (int i = 0; i < length; ++i)
        out[i] = cHeapString[i];

    // just to be sure...
    out[ length ] = 0;
    eqFreeHaskellString( cHeapString );

    return out;
}

extern "C"
{
    EQ_API HsBool eq_begin_runtime()
    {
        char **argv = internArgv;
        int argc = 1;
        lastResult = NULL;

        hs_init(&argc, &argv);
        hs_add_root(__stginit_FormulaDll);
        return HS_BOOL_TRUE;
    }

    EQ_API wchar_t* eq_evalW( wchar_t *in )
        { return dotNetizeW( (wchar_t*)eqWEval( (HsPtr)in ) ); }

    EQ_API char* eq_eval( char *in )
        { return dotNetize( (char*)eqEval( (HsPtr)in ) ); }

    EQ_API void eq_end_runtime()
        { hs_exit(); }
}

