#include <Windows.h>
#include "HsFFI.h"

#define EQ_API __attribute__((dllexport))

#include "formulaDll_stub.h"

BOOL APIENTRY DllMain( HANDLE hModule
                     , DWORD  ul_reason_for_call
                     , LPVOID lpReserved
                     )
    { return TRUE; }

extern "C"
{
    EQ_API HsBool eq_begin_runtime()
    {
        int argc = 1;
        char **argv = NULL;

        // Initialize Haskell runtime
        hs_init(&argc, &argv);

        // Tell Haskell about all root modules
        //hs_add_root(__stginit_FormulaDll);

        // do any other initialization here and
        // return false if there was a problem
        return HS_BOOL_TRUE;
    }

    EQ_API void eq_end_runtime(){
        hs_exit();
    }
}

