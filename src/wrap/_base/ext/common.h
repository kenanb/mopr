#ifndef MOPR_WRAP__BASE_EXT_COMMON_H
#define MOPR_WRAP__BASE_EXT_COMMON_H

#define MOPR_DECLARE_HANDLE( T )                                                         \
    typedef struct T /***/ * T##_h;                                                      \
    typedef struct T const * T##_ch;

#define MOPR_DECLARE_RAII_FUNCTIONS( T_part, N_part )                                    \
    MOPR_API                                                                             \
    T_part##_h mopr_create_##N_part( );                                                  \
                                                                                         \
    MOPR_API                                                                             \
    void mopr_delete_##N_part( T_part##_h this_h );

#define MOPR_DEFINE_RAII_FUNCTIONS( T_part, N_part )                                     \
    T_part##_h mopr_create_##N_part( )                                                   \
    {                                                                                    \
        return new T_part;                                                               \
    }                                                                                    \
                                                                                         \
    void mopr_delete_##N_part( T_part##_h this_h )                                       \
    {                                                                                    \
        delete this_h;                                                                   \
    }

#endif   // MOPR_WRAP__BASE_EXT_COMMON_H
