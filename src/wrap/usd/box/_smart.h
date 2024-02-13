#ifndef MOPR_WRAP_USD_BOX__SMART_H
#define MOPR_WRAP_USD_BOX__SMART_H

#include "wrap/_base/box/generic.h"

#include "pxr/base/tf/nullPtr.h"
#include "pxr/base/tf/refPtr.h"
#include "pxr/base/tf/weakPtr.h"

#include <iostream>

template < class T >
struct MOPR_API MoprPairedSmartPtr : public MoprGeneric
{
    using value_type = T;
    using w_ptr = pxr::TfWeakPtr< value_type >;
    using r_ptr = pxr::TfRefPtr< value_type >;

    static constexpr const pxr::TfNullPtrType n_ptr = pxr::TfNullPtr;

    MoprPairedSmartPtr( ) : wp( ), rp( ), tag( WeakPtr )
    {
    }

    void
     SetWeakPtr( w_ptr w )
    {
        rp = n_ptr;
        wp = w;
        tag = WeakPtr;
    }

    void
     SetRefPtr( r_ptr r )
    {
        wp = n_ptr;
        rp = r;
        tag = RefPtr;
    }

    bool
     IsWeak( ) const
    {
        return tag == WeakPtr;
    }

    bool
     IsEmpty( ) const
    {
        return IsWeak( ) ? bool( wp ) : bool( rp );
    }

    bool
     IsInvalid( ) const
    {
        return tag == WeakPtr ? wp.IsInvalid( ) : false;
    }

    bool
     IsCallReady( ) const
    {
        return !IsWeak( ) && rp;
    }

    bool
     AssertCallReady( ) const
    {
        if ( IsCallReady( ) )
        {
            return true;
        }
        else
        {
            std::cerr << "Non-call-ready instance!" << std::endl;
            return false;
        }
    }

    bool
     TryUpgrade( )
    {
        if ( tag != RefPtr )
        {
            if ( wp.IsInvalid( ) ) return false;

            rp = wp;
            wp = n_ptr;
            tag = RefPtr;
            return true;
        }
        else
        {
            return true;
            // TODO : Log
        }
    }

    void
     Downgrade( )
    {
        if ( tag != WeakPtr )
        {
            wp = rp;
            rp = n_ptr;
            tag = WeakPtr;
        }
        else
        {
            // TODO : Log
        }
    }

    // TODO : Turn into anonymous union.
    w_ptr wp;
    r_ptr rp;

  private:
    enum
    {
        WeakPtr,
        RefPtr,
    } tag;
};

#endif   // MOPR_WRAP_USD_BOX__SMART_H
