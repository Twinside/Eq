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
wchar_t *lastResult = NULL;

void    checkLastResultFreeing()
{
    if ( lastResult != NULL )
    {   /* we go back to haskell to free, even if malloc
         * is used behind to avoid problem if further
         * implementation change */
        eqFreeHaskellString( lastResult );
        lastResult = NULL;
    }
}

extern "C"
{
    EQ_API HsBool eq_begin_runtime()
    {
        char **argv = internArgv;
        int argc = 1;

        // Initialize Haskell runtime
        hs_init(&argc, &argv);

        // Tell Haskell about all root modules
        hs_add_root(__stginit_FormulaDll);

        // do any other initialization here and
        // return false if there was a problem
        return HS_BOOL_TRUE;
    }

    EQ_API wchar_t* eq_eval( wchar_t *in )
    {
        wchar_t *ret = NULL;
        checkLastResultFreeing();
        lastResult = (wchar_t*)eqEval( (HsPtr)in );
        return lastResult;
    }

    EQ_API void eq_cleanup_last_result()
        { checkLastResultFreeing(); }

    EQ_API void eq_end_runtime()
    {
        checkLastResultFreeing();
        hs_exit();
    }
}

