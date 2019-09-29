# MathCompile

## Example

    <<MathCompile`
    CompileToCode@Function[{Typed[p,"Integer"]},Module[{f=If[p>0,#+1&,#-1&]},f[p]]]
