#ifndef DLLMAIN_H
#define DLLMAIN_H

#define EQ_API
#ifdef _MSC_VER
//#define EQ_API __declspec( dllimport )
#else
#define EQ_API __attribute__((dllimport))
#endif

typedef void eq_context_t;

extern "C"
{
    EQ_API bool eq_begin_runtime();

    EQ_API wchar_t* eq_eval( wchar_t *in );
    EQ_API void eq_cleanup_last_result();
    EQ_API char* eq_format( char *in );
    EQ_API wchar_t* eq_formatW( wchar_t *in );

    EQ_API eq_context_t* eq_create_context();

    EQ_API wchar_t* eq_eval_with_contextW( wchar_t *in
                                         , eq_context_t *ctxt );

    EQ_API char* eq_eval_with_context( char *in
                                     , eq_context_t *ctxt );

    EQ_API void eq_delete_context( eq_context_t* handle );

    EQ_API void eq_end_runtime();
}

#endif /* DLLMAIN_H */
