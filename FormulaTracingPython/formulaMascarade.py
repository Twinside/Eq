import sys
import math

# print a formula encoded in tuples :
# (opName, left, right)
# or (opName, child)
# or number or var
def formulaPrinter( file, f ):
    if tuple == type( f ):
        size = len( f )
        if size == 3:
            file.write( "(" )
            formulaPrinter( file, f[1] )
            file.write( f[0] )
            formulaPrinter( file, f[2] )
            file.write( ")" )

        elif size == 2:
            file.write( f[0] )
            file.write( "(" )
            formulaPrinter( file, f[1] )
            file.write( ")" )
    else: # size assumed to be 1
        file.write( str( f ) )

class FormulaMascarade:
    def __init__( self , n ):
        self.number = n
        self.formula = n

    def __init__( self , newFormula, n ):
        self.number = n
        self.formula = newFormula

    def writeFormula( self, file ):
        formulaPrinter( file, self.formula )

    #/////////////////////////////////////
    #   Generic function used in mascarade
    #/////////////////////////////////////
    def apply( self, f, other ):
        if type(self) == type(other):
            return f( self.number, other.number)
        else:
            return f( self.number, other )

    def applyCollect( self, f, name, other ):
        if type(self) == type(other):
            return FormulaMascarade((name, self.formula, other.formula),f(self.number, other.number))
        else:
            return FormulaMascarade((name, self.formula, other),f(self.number, other))

    #/////////////////////////////////////
    #   Comparaison operator
    #/////////////////////////////////////
    def __lt__(self, other):
        return self.apply( (lambda a, b: a < b), other )

    def __le__(self, other):
        return self.apply( (lambda a, b: a <= b), other )

    def __eq__(self, other):
        return self.apply( (lambda a, b: a == b), other )

    def __ne__(self, other):
        return self.apply( (lambda a, b: a != b), other )

    def __gt__(self, other):
        return self.apply( (lambda a, b: a > b), other )

    def __ge__(self, other):
        return self.apply( (lambda a, b: a >= b), other )

    def __cmp__(self, other):
        return self.apply( (lambda a, b: cmp(a, b)), other )

    #/////////////////////////////////////
    #   Numerical operator
    #/////////////////////////////////////
    def __add__(self, other):
        return self.applyCollect((lambda a, b : a + b), "+", other)

    def __sub__(self, other):
        return self.applyCollect((lambda a, b : a - b), "-", other)

    def __mul__(self, other):
        return self.applyCollect((lambda a, b : a * b), "*", other)

    def __div__(self, other):
        return self.applyCollect((lambda a, b : a * b), "/", other)

    def __truediv__(self, other):
        return self.applyCollect((lambda a, b : a * b), "/", other)

    def __floordiv__(self, other):
        return self.applyCollect((lambda a, b : a * b), "/", other)

    def __mod__(self, other):
        return self.applyCollect((lambda a, b : a % b), "%", other)

    def __divmod__(self, other):
        return (self.__div__(other), self % other)

    def __pow__(self, other):
        return self.applyCollect((lambda a, b : pow(a, b)), " ^ ", other)

    def __neg__(self):
        return FormulaMascarade( ("-", self.formula), - self.number )

    def __abs__(self):
        return FormulaMascarade( ("abs", self.formula), abs(self.number) )

    #/////////////////////////////////////
    #   Reversed case, when number come first...
    #/////////////////////////////////////
    def __radd__(self, other):
        return FormulaMascarade( other ) + self

    def __rsub__(self, other):
        return FormulaMascarade( other ) - self

    def __rmul__(self, other):
        return FormulaMascarade( other ) * self

    def __rdiv__(self, other):
        return FormulaMascarade( other ).__div__( self )

    def __rtruediv__(self, other):
        return FormulaMascarade( other ).__truediv__( self )

    def __rfloordiv__(self, other):
        return FormulaMascarade( other ).__floordiv__( self )

    def __rmod__(self, other):
        return FormulaMascarade( other ) % self

    def __rdivmod__(self, other):
        return divmod( FormulaMascarade(other), self )

    def __rpow__(self, other):
        return pow(FormulaMascarade( other ), self)
    
    #/////////////////////////////////////
    #   casting...
    #/////////////////////////////////////
    def __complex__(self): return complex( self.number )
    def __int__(self): return int( self.number )
    def __long__(self): return long( self.number )
    def __float__(self): return float( self.number )

def mathFun( var, f, name ):
    if FormulaMascarade == type( var ):
        return FormulaMascarade( (name, var.formula), f( var.number ) )
    else:
        return FormulaMascarade( (name, var), f( var ) )

def acosh(x):
    return mathFun( x, (lambda x: math.acosh(x)), "acosh")
def asinh(x):
    return mathFun( x, (lambda x: math.asinh(x)), "asinh")
def atanh(x):
    return mathFun( x, (lambda x: math.atanh(x)), "atanh")
def cosh(x):
    return mathFun( x, (lambda x: math.cosh(x)), "cosh")
def sinh(x):
    return mathFun( x, (lambda x: math.sinh(x)), "sinh")
def tanh(x):
    return mathFun( x, (lambda x: math.tanh(x)), "tanh")
def sin(x):
    return mathFun( x, (lambda x: math.sin(x)), "sin")
def tan(x):
    return mathFun( x, (lambda x: math.tan(x)), "tan")
def acos(x):
    return mathFun( x, (lambda x: math.acos(x)), "acos")
def asin(x):
    return mathFun( x, (lambda x: math.asin(x)), "asin")
def atan(x):
    return mathFun( x, (lambda x: math.atan(x)), "atan")
def cos(x):
    return mathFun( x, (lambda x: math.cos(x)), "cos")
def exp(x):
    return mathFun( x, (lambda x: math.exp(x)), "exp")
def log1p(x):
    return mathFun( x, (lambda x: math.log1p(x)), "log1p")
def log10(x):
    return mathFun( x, (lambda x: math.log10(x)), "log10")
def log(x):
    return mathFun( x, (lambda x: math.log(x)), "log")

#math.atan2(y, x)
#math.pow(x, y)

def myFactorial( n ):
    if n <= 1.0:
        return n
    else:
        return n * myFactorial(n - 1.0)

if __name__ == '__main__':
    myFactorial( FormulaMascarade( "x", 7.0 ) ).writeFormula( sys.stdout )
    print( "" )
    myNum = exp(sin( FormulaMascarade( "x", 1.0 ) )) + 2
    myNum.writeFormula( sys.stdout )
    print( "" )


