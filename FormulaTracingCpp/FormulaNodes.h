#ifndef FORMULANODES_H
#define FORMULANODES_H

#include "enumOpCode.h"

namespace Formula
{
    /** Formula nodes are reference counted
     * to permit sharing. The design reason not
     * to use smart_ptr, or others, is to avoid 
     * any dependencies to other libraries. Not
     * every compiler got boost or TR1. Be assured
     * that I would have preferred to use them.
     */
    template <class Number>
    class FormulaNode
    {
    public:
        FormulaNode();
        virtual ~FormulaNode();
        
        void    acquire() const;
        void    release() const;

        virtual void show( std::ostream &out ) const = 0;

    private:
        mutable int usageCount;
    };

    template <class Number>
    class FormulaNumber : public FormulaNode<Number>
    {
    public:
        FormulaNumber( Number initN ) : n( initN ) {}

        virtual void show( std::ostream &out ) const;
    private:
        Number  n;
    };

    template <class Number>
    class FormulaVar : public FormulaNode<Number>
    {
    public:
        FormulaVar( const char* varName )
            : name( varName ) {};

        virtual void show( std::ostream &out ) const;
    private:
        const char* name;
    };

    template <class Number>
    class FormulaUnode : public FormulaNode<Number>
    {
    public:
        FormulaUnode( OpCode c, const FormulaNode<Number> *on )
            : opcode( c ), child( on )
            { if (child) child->acquire(); }

        ~FormulaUnode();

        virtual void show( std::ostream &out ) const;

    private:
        OpCode      opcode;
        const FormulaNode<Number> *child;
    };

    template <class Number>
    class FormulaBiNode : public FormulaNode<Number>
    {
    public:
        FormulaBiNode( OpCode c
                     , const FormulaNode<Number> *l
                     , const FormulaNode<Number> *r )
            : opcode( c )
            , left( l )
            , right( r )
        {
            if (left) left->acquire();
            if (right) right->acquire();
        }
        ~FormulaBiNode();

        virtual void show( std::ostream &out ) const;

    private:
        OpCode      opcode;
        const FormulaNode<Number> *left;
        const FormulaNode<Number> *right;
    };
}

#include "FormulaNodes.inline.h"

#endif /* FORMULANODES_H */


