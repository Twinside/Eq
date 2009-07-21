#ifndef FORMULASPCEIFIC_INLINE_H
#define FORMULASPCEIFIC_INLINE_H

namespace Formula
{
    extern const char *opReprez[];
    // Scratch implementation
    // feel free to copy/paste or destroy
    template <typename Number>
    FormulaTracedNumber<Number>::FormulaTracedNumber( FormulaTracedNumber<Number> *f, Number n )
        : formula( f )
        , number( n )
        { if ( formula ) formula->acquire(); }

    template <typename Number>
    FormulaTracedNumber<Number>::FormulaTracedNumber( Number n )
        : formula( 0 )
        , number( n )
    {
        formula = new FormulaNumber<Number>( n );
        formula->acquire();
    }


    template <typename Number>
    FormulaTracedNumber<Number>::FormulaTracedNumber( const char *name, Number n )
        : formula( 0 )
        , number( n )
    {
        formula = new FormulaVar<Number>( name );
        formula->acquire();
    }

    template <typename Number>
    FormulaTracedNumber<Number>::FormulaTracedNumber( const FormulaNode<Number> *node, Number n )
        : formula( const_cast<FormulaNode<Number>*>(node) )
        , number( n )
        { formula->acquire(); }

    template <typename Number>
    FormulaTracedNumber<Number>::~FormulaTracedNumber()
        { if ( formula ) formula->release(); }

    template <typename Number>
    FormulaTracedNumber<Number>::FormulaTracedNumber( const FormulaTracedNumber<Number> &cpy )
    {
        formula = cpy.formula;
        if ( formula ) formula->acquire();
        number = cpy.number;
    }

    template <typename Number>
    FormulaTracedNumber<Number>& FormulaTracedNumber<Number>::operator =( const FormulaTracedNumber<Number> &cpy )
    {
        if ( formula ) formula->release();
        formula = cpy.formula;
        if ( formula ) formula->acquire();
        number = cpy.number;
    }

    template <typename Number> // preDecrement
    FormulaTracedNumber<Number>& FormulaTracedNumber<Number>::operator ++()
    {
        FormulaNode<Number> *one = new FormulaNumber<Number>( static_cast<Number>( 1 ) );
        FormulaNode<Number> *oldFormula = formula;
        formula = new FormulaBiNode<Number>( OpAdd, oldFormula, one );
        oldFormula->release();
        formula->acquire();
        return *new FormulaTracedNumber<Number>( formula, ++number);
    }

    template <typename Number>  // post increment
    FormulaTracedNumber<Number>& FormulaTracedNumber<Number>::operator ++( int )
    {
        FormulaTracedNumber<Number> *ret = new FormulaTracedNumber<Number>( formula, number );
        FormulaNode<Number> *one = new FormulaNumber<Number>( static_cast<Number>( 1 ) );
        formula->release();

        formula = new FormulaBiNode<Number>( OpAdd, formula, one );
        formula->acquire();
        number++;
        return *ret;
    }

    template <typename Number>
    FormulaTracedNumber<Number>& FormulaTracedNumber<Number>::operator --()
    {
        FormulaNode<Number> *one = new FormulaNumber<Number>( static_cast<Number>( 1 ) );
        FormulaNode<Number> *oldFormula = formula;
        formula = new FormulaBiNode<Number>( OpAdd, oldFormula, one );
        oldFormula->release();
        formula->acquire();
        return *new FormulaTracedNumber<Number>( formula, --number);
    }

    template <typename Number>  // post increment
    FormulaTracedNumber<Number>& FormulaTracedNumber<Number>::operator --( int )
    {
        FormulaTracedNumber<Number> *ret = new FormulaTracedNumber<Number>( formula, number );
        FormulaNode<Number> *one = new FormulaNumber<Number>( static_cast<Number>( 1 ) );
        formula->release();

        formula = new FormulaBiNode<Number>( OpSub, formula, one );
        formula->acquire();
        number--;
        return *ret;
    }

    template <typename Number>
    FormulaTracedNumber<Number>& FormulaTracedNumber<Number>::operator -() const
    {
        FormulaNode<Number> *newF = new FormulaUnode<Number>( OpNegate, formula );
        return *new FormulaTracedNumber<Number>( newF, - number );
    }

    /////////////////////////////////////////////////////////
    ////        Printers
    /////////////////////////////////////////////////////////
    template <class Number>
    void FormulaBiNode<Number>::show( std::ostream &out ) const
    {
        out << "(";
        if (left) left->show( out );
        out << opReprez[ opcode ];
        if (right) right->show( out );
        out << ")";
    }

    template <class Number>
    void FormulaNumber<Number>::show( std::ostream &out ) const
        { out << n; };

    template <class Number>
    void FormulaVar<Number>::show( std::ostream &out ) const
        { out << name; };

    template <class Number>
    void FormulaUnode<Number>::show( std::ostream &out ) const
    {
        out << opReprez[opcode] << '(';
        if ( child ) child->show( out );
        out << ')';
    };
}

#endif /* FORMULASPCEIFIC_INLINE_H */


