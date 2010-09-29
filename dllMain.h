#ifndef DLLMAIN_H
#define DLLMAIN_H

#define EQ_API
#ifdef _MSC_VER
//#define EQ_API __declspec( dllimport )
#else
#define EQ_API __attribute__((dllimport))
#endif

extern "C"
{
    EQ_API bool eq_begin_runtime();
    EQ_API wchar_t* eq_eval( wchar_t *in );
    EQ_API void eq_cleanup_last_result();
    EQ_API void eq_end_runtime();
}

#endif /* DLLMAIN_H */
