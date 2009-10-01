#include <iostream>
#include <Windows.h>
#include "../dllMain.h"

typedef wchar_t* (*Evaluator)( wchar_t * );

int main(int argc, char *argv[])
{
    HMODULE dllMainModule;
    FARPROC initProc;
    Evaluator evalProc;
    FARPROC endProc;

    wchar_t *inProg = L"1 + 2 + 3 + x";
    wchar_t *outProg = NULL;

    dllMainModule = LoadLibraryW( L"formulaDll.dll" );
    initProc = GetProcAddress( dllMainModule, "eq_begin_runtime" );
    endProc = GetProcAddress( dllMainModule, "eq_end_runtime" );
    evalProc = (Evaluator)GetProcAddress( dllMainModule, "eq_eval" );

    initProc();
    //eq_begin_runtime();


    outProg = evalProc( inProg );
    //outProg = eq_eval( inProg );

    std::wcout << outProg << std::endl;

    endProc();
    //eq_end_runtime();

    return 0;
}
