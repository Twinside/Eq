#ifndef FORMULA_H
#define FORMULA_H

namespace Formula
{
    template <class Number>
    class FormulaNode<Number>
    {
    }

    template <class Number>
    class FormulaNumber : FormulaNode<Number>
    {
    public:
        FormulaNumber( Number initN ) : n( initN ) {}

    private:
        Number  n;
    };

    template <class Number>
    class FormulaVar : FormulaNode<Number>
    {
    public:
        FormulaVar( const char* varName )
            : name( varName ) {};
    private:
        const char* name;
    }

    template <class Number>
    class FormulaUnode : FormulaNode<Number>
    {
    public:
        FormulaUnode( OpCode c, FormulaNode *on )
            : opcode( c ), child( on ) {}

    private:
        OpCode      opcode;
        FormulaNode *child;
    };

    template <class Number>
    class FormulaBiNode : FormulaNode<Number>
    {
    public:
        FormulaNode( OpCode c, FormulaNode *l, FormulaNode *r )
            : left( l )
            , right( r )
            , opcode( c )
        {}

        OpCode      opcode;
        FormulaNode *left;
        FormulaNode *right;
    }


    template
        <typename Number>
        class FormulaTracedNumber
        {
            public:
                FormulaTracedNumber( FormulaTracedNumber<Number> f, Number n );
                ~FormulaTracedNumber();
                FormulaTracedNumber( const FormulaTracedNumber &cpy );
                
            private:
        };

    // Scratch implementation
    // feel free to copy/paste or destroy
    template <typename Number>
        FormulaTracedNumber<Number>::FormulaTracedNumber( FormulaTracedNumber<Number> f, Number n )
        {
        }

    template <typename Number>
        FormulaTracedNumber<Number>::~FormulaTracedNumber()
        {
        }

    template <typename Number>
        FormulaTracedNumber<Number>::FormulaTracedNumber( const FormulaTracedNumber &cpy )
        {
        }
}

#endif /* FORMULA_H */


