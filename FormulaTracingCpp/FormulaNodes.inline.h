#ifndef FORMULANODES_INLINE_H
#define FORMULANODES_INLINE_H

namespace Formula
{
    // Scratch implementation
    // feel free to copy/paste or destroy
    template <class Number>
    FormulaNode<Number>::FormulaNode()
        : usageCount( 0 )
    {};

    template <class Number>
    FormulaNode<Number>::~FormulaNode()
    {};

    template <class Number>
    void FormulaNode<Number>::acquire() const
        { usageCount++; };

    template <class Number>
    void FormulaNode<Number>::release() const
    {
        usageCount--;
        if (usageCount <= 0)
            delete this;
    };

    template <class Number>
    FormulaUnode<Number>::~FormulaUnode()
        { if ( child ) child->release(); };

    template <class Number>
    FormulaBiNode<Number>::~FormulaBiNode()
    {
        if ( left ) left->release();
        if ( right ) right->release();
    };
}

#endif /* FORMULANODES_INLINE_H */


