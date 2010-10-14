#include <Windows.h>
#include <stdio.h>
#include "HsFFI.h"

#include "formulaDll_stub.h"

#define EQ_API extern __attribute__((dllexport))
extern void __stginit_FormulaDll(void);

BOOL APIENTRY DllMain( HANDLE hModule
                     , DWORD  ul_reason_for_call
                     , LPVOID lpReserved
                     )
    { return TRUE; }

char *internArgv[] = { "formulaDll", "Bleh" };
char* lastResult = NULL;

wchar_t*    dotNetizeW( /*out*/ wchar_t *cHeapString )
{
    int i;
    size_t length = wcslen( cHeapString );
    wchar_t *out = (wchar_t*)CoTaskMemAlloc( (length + 1) * sizeof( wchar_t ) );

    for (i = 0; i < length; ++i)
        out[i] = cHeapString[i];
    // just to be sure...
    out[ length ] = 0;
    eqFreeHaskellString( cHeapString );

    return out;
}

char*    dotNetize( /*out*/ char* cHeapString )
{
    int i;
    size_t length = strlen( cHeapString );
    char *out = (char*)CoTaskMemAlloc( (length + 1) * sizeof( char ) );

    for (i = 0; i < length; ++i)
        out[i] = cHeapString[i];

    // just to be sure...
    out[ length ] = 0;
    eqFreeHaskellString( cHeapString );

    return out;
}

EQ_API HsBool eq_begin_runtime()
{
    char **argv = internArgv;
    int argc = 1;
    lastResult = NULL;

    hs_init(&argc, &argv);
    hs_add_root(__stginit_FormulaDll);
    return HS_BOOL_TRUE;
}

EQ_API wchar_t* eq_translate_mathml( wchar_t *in )
    { return dotNetizeW( (wchar_t*)eqMathMLTranslate( (HsPtr)in ) ); }

EQ_API wchar_t* eq_evalW( wchar_t *in )
    { return dotNetizeW( (wchar_t*)eqWEval( (HsPtr)in ) ); }

EQ_API char* eq_eval( char *in )
    { return dotNetize( (char*)eqEval( (HsPtr)in ) ); }

EQ_API void eq_end_runtime()
    { hs_exit(); }

