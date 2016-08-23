BeginPackage["RIFunction`", {"paul`", "PackageDeveloper`", "FiniteMapping`", "numerics`", "cform`", "RVarval`", "SymbolicC`"}]

ClearAll["RIFunction`*","RIFunction`Private`*"]

(* -- Purpose -- *)
(*
Represent
  f: (I -> R) -> (J -> R)

(X -> R) objects are represented by RVarval.

Alternatively, this represents vector (or matrix etc) valued and argued functions

  [f]: R^#I -> R^#J

Sometimes we just write

  f: I -> J or f: #I -> #J

for such functions.

This package abstracts away the differences between "pure functions", 
named functions (symbols with down values) and other expressions e
that evaluate to something
when used as e[args], as well as plain expressions that represent functions in certain variables
which would evaluate to numerical results when Block, With or /. is used to
provide values for the variables in them.

The package deals with functions with both named and nameless (positional) arguments.
The function can accept named arguments as a list of options (rules) or as a sequence of them,
or names can be mapped to positional arguments.

Functions can always be evaluated partways with the variables still present.

It can compute derivatives of the given function by any of its arguments.

It can call the function in different ways.

It can compile the function, both to CompiledFunction using Compil)
and to compileable (yet unreadable) C code using cform`.
Compiled functions take NumericVector arguments and give NumericVector return values.

It only handles functions with (machine-)real-valued arguments and
constant length real vectors as return values.

Default variable names are {1}, {2}, {3}, etc., as in a FiniteMapping for representing
a list.
*)


(* ^^ End ^^ *) 

(* TODO *)
(*
Don't allow indexing variables with variables (no variable should be subexpression of another)

Check generated CCode: should contain integers only within variable references (and powers?)
 certainly not in divisions (note that though CForm[x/2] gives x/2., but we handle all operators ourselves)

Support variable names with head List (problems in replacement, evaluation (Listable operations), derivatives etc.)
 or develop custom list to display like an ordered pair ()/tuple
 - in particular because FiniteMapping represents vectors with such variables, thus every ExpressionList without
  named outputs has them!

Does it make sense to support the empty function, i.e. no arguments, no ouputs?
 Just outputs might make sense (constants!)

Allow reordering inputs (and outputs)
 This might not always make sense when a specific vector is represented implicitly.
  TODO do we allow the RVarval to supply values in an order different from the one expected? What do we do in that case?

* Give special treatment to Array-Valued functions.

Allow mixed symbolic-numeric evaluation

Make composition/'concatenation' (execute after one another)
 a first-class feature (where are there performance benefits, where not?)
 - compute composition derivative according to chain-rule
 - make some symbolic example to verify, c.f. rifunction.nb
 - make use of sparsity in Selector
  remember when something is the result of composing a function with a sparse one
 - when combining this with another function, make its Jacobian sparse too
 - let the user choose whether the output should be a SparseArray.
 - store hints when e.g. derivatives are sparse
   in EvaluateDerivative (instead of making it as a function) produce SparseArray type results where meaningful
 - allow different/non-matching input/output names, rearrange, drop names or forbid?

Compute derivatives of Selector
  - Identity functions special case

Generate cform constructing matrices/(Sparse)Array's
 -> *
 - other Position like patterns might work too

Optimize the case of unnamed (purely index) variables.
*)

(* -- Purpose -- *)
(*
Stores all information connected with an RFunction.
*)

(* Implementation notes *)
(*
Valid representations:

RIFunction[ExpressionList, expr_List, vars_List (, outputs_List)]
    expr evaluates to a NumericVector when vars are inserted via any of /., With, Block
  outputs are optionally named - default names are FMMakeListDomainNames@Length@expr

RIFunction[Composition, f_RIFunction, g_RIFunction]
  Represents f ° g, i.e. h(x) = f(g(x))
  Output names of g and input names of f are ignored (do no need to match).
   If you want to force matching, do so at construction time

RIFunction[Multiple, components_FiniteMapping, pairing_]
    Represents a function

    f: (I -> R) -> (K x J -> R)

    for x defined by pairing (c.f. RVarvalMakePairIndexed),
    via #K components functions c_k,

    c_k: (I -> R) -> (J -> R)

    TODO I and J do not need to be stored with each component.
    TODO Check whether really to components all accept the same args and return the same -- or do I even use it otherwise?

    such that

    f(x)(pairing[k, j]) = c_k(x)(j)

    Note that each component function receives the same arguments.

RIFunction[Selector, a_List, b_List, saIndex_List]
  select: (B -> R) -> (A -> R)
  with
  select(x)(a) = x(s(a))
  for injective FiniteMapping s: A -> B

  saIndex stores the indices into b which will be distributed into a.
   It has the same length as a.
   It consists of integers from 1:Length@B


  TODO generalize to any input and output size, maybe even repeated outputs

RIFunction[Repeated, f_RIFunction, p_List, pairing_]
  given f: A -> B and P, make
  f: P x A -> P x B
  where x is defined by pairing

RIFunction[Constant, inputs_List, output_RVarval]
  c: (I -> R) -> (O -> R)
  where the returned object is always the same.
*)


PublicSymbols[
  RIFunction
, RIFunctionMakeConstant
, RIFunctionMakePaired
, RIFunctionMakeRepeated
, RIFunctionMakeFromExpressionList
, RIFunctionMakeSelector
, RIFunctionMakeComposition
, RIFunctionOutputs
,RIFunctionOutputExpressionMap
, RIFunctionArguments
, RIFunctionExpressionList
, RIFunctionEvaluate
, RIFunctionEvaluateNameless
, RIFunctionEvaluateDerivative
, RIFunctionEvaluateDerivativeNameless
, RIFunctionEvaluateMultiDerivative
, RIFunctionEvaluateMultiDerivativeNameless
, RIFunctionMakeMultiDerivative
, RIFunctionMakeFullJacobian
, RIFunctionMakeMultiDerivativeIndexed
, RIFunctionMakeDerivativeIndexed
  ,RIFunctionMakeDerivative
, RIFunctionCForm
, RIFunctionCFormExpressions
, RIFunctionCFormOutputArrayAssignments
, RIFunctionCFormIndexedFunction
, RIFunctionCFormAllDerivativesIndexed
, RIFunctionCFormAllDerivativesIndexedFunction
]


Begin["`Private`"]

(*TODO Hold expr to stop it from being evaluated when nothiing changed *)
(*TODO add a way to have named (not just list indexed) outputs *)
(* Name *)

RIFunction

(* Attributes *)

RIFunction~SetAttributes~HoldAllComplete

(* ^^ End ^^ *)


(* ^^ End ^^ *)

(* -- Purpose -- *)


(* ^^ End ^^ *)


(* -- Purpose -- *)


(* Name *)

DefinePublicFunction[
RIFunctionMakeFromExpressionList[
  expression_List
, arguments_List

(* Code *) ] ,
  "Create an RIFunction from an expression list with known \"variable\" names.

The expression list is assumed to
evaluate to a numeric vector of constant length for any argument values."
  ,Module[{}, Null

; RIFunction[ExpressionList, expression, arguments]

(* ^^ End ^^ *) ]
  ];


DefinePublicFunction[
RIFunctionMakeFromExpressionList[
  expression_List
  , arguments_List (* optimization *)
  , outputs_List] /; outputs == FMMakeListDomainNames@Length@expression 
  ,"with user-named arguments"
  ,RIFunctionMakeFromExpressionList[expression, arguments]
];

DefinePublicFunction[
RIFunctionMakeFromExpressionList[
    expression_List
  , arguments_List
  , outputs_List] 
  , "with user-named outputs"
  ,  RIFunction[ExpressionList, expression, arguments, outputs]
  ];

DefinePublicFunction[
RIFunctionMakeComposition[f_RIFunction, g_RIFunction] /; Length@RIFunctionOutputs@g ==
    Length@RIFunctionArguments@f 
  ,"f°g"
  ,RIFunction[Composition, f, g]
];

(* -- Purpose -- *)
(*

*)

(* Name *)
DefinePublicFunction[
RIFunctionMakeSelector[
  s_FiniteMapping?FMInjectiveQ,
  b_List] 
  ,"Create a selector function from injective s: A -> B

Note: Since s only stores A and s(A), B, the full set of argument variable names
 must be given explicitly.

A names the output variables, which may be different or the same as the input variables.

The order of b does not matter (TODO verify: doesn't b define how inputs are interpreted?)"
  ,With[{a=FMDomain@s,sa=FMEvaluateAll@s},
  {saIndex = Positions[b, sa]}~With~RIFunction[Selector, a, b, saIndex]
]
  ];

(*

*)

DefinePublicFunction[
RIFunctionMakeSelector[
  a_List,
  b_List] 
  ,"Special constructor, where each a maps to itself: This creates a

e.g. RIFunctionMakeSelector[{y, x}, {x, y}]
creates the 2 argument function that flips its arguments."
  ,    RIFunctionMakeSelector[ FiniteMappingMakeFromLists[a,a], b]
  ];

(* -- Purpose -- *)
(*
J
*)
DefinePublicFunction[
RIFunctionOutputs[f : RIFunction[ExpressionList, expr_, vars_List,___]] ,"J", FMMakeListDomainNames@Length@expr
];
DefinePublicFunction[
RIFunctionOutputs[f : RIFunction[ExpressionList, expr_, vars_List, outputs_List]] ,"J", outputs
];
DefinePublicFunction[
RIFunctionOutputs[RIFunction[Selector, a_List, b_List, saIndex_List]] ,"J", a
];
DefinePublicFunction[
RIFunctionOutputs[RIFunction[Multiple, components_FiniteMapping, pairing_]] ,"J",
    With[{k = FMDomain@components, is = RIFunctionOutputs /@ FMEvaluateAll@components},
      pairing@@@Flatten[Thread /@
          Transpose@{k, is}, 1]
    ] (* TODO can probably be simplified with Tuples[{}] *)
];
DefinePublicFunction[
RIFunctionOutputs[RIFunction[Repeated, f_RIFunction, p_List, pairing_]] ,"J", pairing@@@Tuples@{p, RIFunctionOutputs@f}
];
DefinePublicFunction[
  RIFunctionOutputs[RIFunction[Constant, inputs_List, output_RVarval]] ,"J", RVVVariables@output
];
DefinePublicFunction[
RIFunctionOutputs@RIFunction[Composition, f_RIFunction, g_RIFunction] ,"J", RIFunctionOutputs@f
];
(* ^^ End ^^ *)

(* -- Purpose -- *)
(*
I
*)

DefinePublicFunction[
RIFunctionArguments@RIFunction[Composition, f_RIFunction, g_RIFunction] ,"I", RIFunctionArguments@g
];
DefinePublicFunction[
RIFunctionArguments[RIFunction[ExpressionList, expr_, vars_List, ___]] ,"I", vars
];
DefinePublicFunction[
RIFunctionArguments[RIFunction[Selector, a_List, b_List, saIndex_List]] ,"I", b
];
DefinePublicFunction[
RIFunctionArguments[RIFunction[Multiple, components_FiniteMapping, pairing_]] ,"I",
    RIFunctionArguments@First@FMEvaluateAll@components (* TODO surely can be done more efficiently *)
];
DefinePublicFunction[
RIFunctionArguments[RIFunction[Repeated, f_RIFunction, p_List, pairing_]] ,"I", pairing@@@Tuples@{p, RIFunctionArguments@f}
];
DefinePublicFunction[
RIFunctionArguments[RIFunction[Constant, inputs_List, output_RVarval]] ,"I", inputs
];

(* ^^ End ^^ *)

(* -- Purpose -- *)
(*
Return a list of expression that, when all variables in I are defined to
real numbers, evaluates to a NumericVector of length #J

Note that this loses the names of outputs.
*)

DefinePublicFunction[
RIFunctionExpressionList@RIFunction[Composition, f_RIFunction, g_RIFunction] ,"Return a list of expression that, when all variables in I are defined to real numbers, evaluates to a NumericVector of length #J  Note that this loses the names of outputs.",
    RIFunctionExpressionList@f /. Thread@Rule[RIFunctionArguments@f, RIFunctionExpressionList@g] (* re-enforce J argument names *)
];
DefinePublicFunction[
RIFunctionExpressionList[RIFunction[ExpressionList, expr_, vars_List,___]] ,"Return a list of expression that, when all variables in I are defined to real numbers, evaluates to a NumericVector of length #J  Note that this loses the names of outputs.", expr
];
DefinePublicFunction[
RIFunctionExpressionList[RIFunction[Selector, a_List, b_List, saIndex_List]] ,"Return a list of expression that, when all variables in I are defined to real numbers, evaluates to a NumericVector of length #J  Note that this loses the names of outputs.", b[[saIndex]]
];
DefinePublicFunction[
RIFunctionExpressionList[RIFunction[Constant, inputs_List, output_RVarval]] ,"Return a list of expression that, when all variables in I are defined to real numbers, evaluates to a NumericVector of length #J  Note that this loses the names of outputs.", RVVValues@output
];
DefinePublicFunction[
RIFunctionExpressionList[RIFunction[Multiple, components_FiniteMapping, pairing_]] ,"Return a list of expression that, when all variables in I are defined to real numbers, evaluates to a NumericVector of length #J  Note that this loses the names of outputs.",
    Flatten[RIFunctionExpressionList /@ FMEvaluateAll@components, 1]
];
DefinePublicFunction[
RIFunctionExpressionList[RIFunction[Repeated, f_RIFunction, ps_List, pairing_]] ,"Return a list of expression that, when all variables in I are defined to real numbers, evaluates to a NumericVector of length #J  Note that this loses the names of outputs.",
    {el=RIFunctionExpressionList@f}~With~Flatten[
      Table[el /. Thread@Rule[RIFunctionArguments@f, (* TODO this is problematic when variable names are lists because of Listable operations*)
        (* make pair indices*)
        pairing[p,#]&/@RIFunctionArguments@f
      ], {p,ps}], 1]
];

(* TODO  -> ability to evaluate symbolically? *)

(* ^^ End ^^ *)

(* -- Purpose -- *)


DefinePublicFunction[
RIFunctionOutputExpressionMap[f_RIFunction]
  ,"Return a FiniteMapping from output variable names to expressions.

An equivalent RIFunction could be reconstructed from this, albeit less optimized."
  ,FiniteMappingMakeFromLists[RIFunctionOutputs@f, RIFunctionExpressionList@f]
];

(* ^^ End ^^ *)


(* -- Purpose -- *)
(*


TODO not all arguments need to be specified
TODO does the order in the RVarval matter?
TODO What if the amount of arguments is correct but not the names of them?

TODO some of these could be implemented using the Nameless variant, then adding the names in the end
 when will this be faster?
*)

(* Name *)

DefinePublicFunction[
RIFunctionEvaluate[RIFunction[Composition, f_RIFunction, g_RIFunction], args_RVarval]
  ,"f(args)"
  ,
    (
      f~RIFunctionEvaluate~RVarvalMake[RIFunctionArguments@f, RVVValues@RIFunctionEvaluate[g, args]]
    )
    ];


DefinePublicFunction[
RIFunctionEvaluate[RIFunction[Constant, inputs_List, output_RVarval], _] ,"", output
];

DefinePublicFunction[
RIFunctionEvaluate[

(* Arguments *) PatternSequence[]

, f : RIFunction[ExpressionList, expr_, vars_List]
, args_RVarval
(* Code *) ],"", Module[{}, Null

; RVarvalMake[expr /. RVVAsRules@args] (* TODO compare /. with other ways of inserting values, Block is probably more efficient but error prone? *)

]
  ];

DefinePublicFunction[
RIFunctionEvaluate[ f : RIFunction[ExpressionList, expr_, vars_List,outputs_List]
  , args_RVarval] 
  ,""
  ,Module[{}, Null

; RVarvalMake[outputs, expr /. RVVAsRules@args] (* TODO compare /. with other ways of inserting values, Block is probably more efficient but error prone? *)

]
  ];


(* f(x)(pairing[k, j]) = c_k(x)(j) *)

DefinePublicFunction[
RIFunctionEvaluate[

(* Arguments *) PatternSequence[]

  , f : RIFunction[Multiple, components_FiniteMapping, pairing_]
  , args_RVarval
(* Code *) ] ,"", With[{k = FMDomain@components},

  RVarvalMakePairIndexed[k, #~RIFunctionEvaluate~args & /@ FMEvaluateAll@components, pairing]

  ]
  ];


(* select(x)(a) = x(s(a))  *)
DefinePublicFunction[
RIFunctionEvaluate[

(* Arguments *) PatternSequence[]

  , RIFunction[Selector, a_List, b_List, saIndex_List]
  , args_RVarval
(* Code *) ] /; RVVLength@args == Length@b ,"", With[{},

  RVarvalMake[a, RVVLookupByVarIndices[args, saIndex]]

]
  ];


(* -- Purpose -- *)
(*

*)

(* Name *)

DefinePublicFunction[
RIFunctionEvaluate                                         [

(* Arguments *)                                            PatternSequence[]

, RIFunction[Repeated, f_RIFunction, ps_List, pairing_]
, args_RVarval

(* Conditions *)                                             ] /;

RVVLength@args == Length@ps * Length@RIFunctionArguments@f    
  ,"Evaluate f: P x A -> P x B",

(* Code *)

  RVarvalMakePairIndexed[Sequence[]
    , ps

    , Table[Sequence[]

      , f~RIFunctionEvaluate~RVarvalSlicePairIndexed[args, p, pairing]

      , {p,ps}

      ]

    , pairing
  ]
];

(* ^^ End ^^ *)


(* -- Purpose -- *)
(*

*)

(* Name *)


(* TODO allow constructing matrix output *)

DefinePublicFunction[
RIFunctionEvaluateNameless[RIFunction[Composition, f_RIFunction, g_RIFunction], vals_?NumericVectorQ] 
  ,"[f]([args])

all args need to be specified"
  ,
    (
      f~RIFunctionEvaluateNameless~(g~RIFunctionEvaluateNameless~vals)
    )
    ];

DefinePublicFunction[
RIFunctionEvaluateNameless[

(* Arguments *) PatternSequence[]

  , f : RIFunction[ExpressionList, expr_List, vars_List, ___]
  , vals_?NumericVectorQ

(* Code *) ] /; Length@vals == Length@vars,"", Module[{}, Null

; expr /. Thread@Rule[vars, vals]
]
];
DefinePublicFunction[
RIFunctionEvaluateNameless[
  RIFunction[Selector, a_List, b_List, saIndex_List]
  , vals_?NumericVectorQ
] /; Length@vals == Length@b ,"", vals[[saIndex]]
];
DefinePublicFunction[
RIFunctionEvaluateNameless[
  RIFunction[Multiple, components_FiniteMapping, pairing_]
  , vals_?NumericVectorQ
] /; Length@vals == Length@RIFunctionArguments@First@FMEvaluateAll@components ,"",
  Flatten[
  RIFunctionEvaluateNameless[#, vals] & /@ FMEvaluateAll@components

  ,1]
];
DefinePublicFunction[
RIFunctionEvaluateNameless[RIFunction[Constant, inputs_List, output_RVarval], _] ,"", RVVValues@output
];

(* -- Purpose -- *)
(*

*)

(* Name *)


DefinePublicFunction[
RIFunctionEvaluateNameless                                         [

(* Arguments *)                                            PatternSequence[]

, RIFunction[Repeated, f_RIFunction, ps_List, pairing_]
, vals_?NumericVectorQ

(* Conditions *)                                             ] /;

Length@vals == Length@ps * Length@RIFunctionArguments@f
  ,"Evaluate f: P x A -> P x B for unnamed arguments (assumed order)",
    Flatten[
      f~RIFunctionEvaluateNameless~# & /@ Partition[vals, Length@RIFunctionArguments@f]
,1
]
  ];





(* -- Purpose -- *)
(*

*)

(* TODO Check that x is one of Arguments *)
(* Name *)

(* TODO this might not always work correctly
TODO in particular, this does nto yet support named outputs, but none of the methods do except Selector (and thus Composed)...*)
DefinePublicFunction[

RIFunctionMakeDerivative[

(* Arguments *) PatternSequence[]

  , f_RIFunction
  , x_
(* Code *) ] /; RIFunctionArguments@f~Contains~x

  ,"df
--
dx

d_x f"

  ,RIFunctionMakeFromExpressionList[
  1. D[RIFunctionExpressionList@f, x]
  , RIFunctionArguments@f
  , RIFunctionOutputs@f] (* TODO instead of creating an FMMakeListDomainNames here and removing it later, don't create it *)
];

DefinePublicFunction[
RIFunctionMakeDerivative[f:RIFunction[Constant, inputs_List, output_RVarval], x_] /; RIFunctionArguments@f~Contains~x

  ,"df
--
dx

d_x f"

    ,{outputzeros = RVarvalMakeConstant[RVVVariables@output, 0.]}~With~RIFunction[Constant, inputs, outputzeros]
  ];
(*

*)

DefinePublicFunction[
RIFunctionMakeDerivative[f:RIFunction[Multiple, components_FiniteMapping, pairing_], x_] /; RIFunctionArguments@f~Contains~x
  ,"",
    {dcomponents = RIFunctionMakeDerivative[#,x]&~FMMapValues~components} ~With~ RIFunction[Multiple, dcomponents, pairing]
];
(* f: B -> A, where A are selected from B according to saIndex which has no repeated values.

Thus the derivative is a constant vector of length A with a 1. only for x
*)

DefinePublicFunction[
RIFunctionMakeDerivative[f: RIFunction[Selector, a_List, b_List, saIndex_List], x_] /; RIFunctionArguments@f~Contains~x
  ,"",
With[{indexOfXInB = First@First@Position[b, x]},

    b~RIFunctionMakeConstant~RVarvalMake[a,
      SparseArray[
        Map[1. Boole[# === indexOfXInB] &, saIndex] (* TODO could be constructed more efficiently with a single or no rule *)
      ]
    ]

]
  ];

(* -- Purpose -- *)


(* TODO indexed variants? *)
DefinePublicFunction[
RIFunctionEvaluateDerivative[f_RIFunction, i_, args_RVarval] /; RIFunctionArguments@f~Contains~i
  ,"These might be more or less efficient for just evaluating the derivative once or multiple times.",
  RIFunctionMakeDerivative[f, i]~RIFunctionEvaluate~args
  ];

DefinePublicFunction[
RIFunctionEvaluateMultiDerivative[f_RIFunction, y_List, args_RVarval] /; RIFunctionArguments@f~ContainsAll~y
  ,"",
  RIFunctionMakeMultiDerivative[f, y]~RIFunctionEvaluate~args
  ];

DefinePublicFunction[
RIFunctionEvaluateDerivativeNameless[f_RIFunction, i_, args_?NumericVectorQ] /; RIFunctionArguments@f~Contains~i
  ,"",
    RIFunctionMakeDerivative[f, i]~RIFunctionEvaluateNameless~args
  ];

DefinePublicFunction[
RIFunctionEvaluateMultiDerivativeNameless[f_RIFunction, y_List, args_?NumericVectorQ] /; RIFunctionArguments@f~ContainsAll~y
  ,"",
    RIFunctionMakeMultiDerivative[f, y]~RIFunctionEvaluateNameless~args
  ];

(* TODO indexed variant *)
DefinePublicFunction[
RIFunctionEvaluateDerivative[fg : RIFunction[Composition, f_RIFunction, g_RIFunction],
  i_,
  x_RVarval] /; RIFunctionArguments@fg~Contains~i

  ,"generalized chain rule :

Let

g: I -> J
f: J -> K

then h := f ° g

and

d_i h x = sum_j d_j f (g x) * (d_i g x)_j

Note that 'd_j f (g x)' is a K-vector, d_i g x is a J-vector

this is analoguous to how you would usually multiply the jacobian of f with the derivative vector of g
 (or the jacobian in the -Multiple case)"



  ,Module[{
  r
  , gx = (g~RIFunctionEvaluate~x)~RVVRename~RIFunctionArguments@f
  , digx = RIFunctionEvaluateDerivative[g, i, x]~RVVRename~RIFunctionArguments@f
}, (* must force this to be named according to J, cuz we don't enforce this earlier *)

  Print@gx;
  Print@digx;

  (* compute each d_jf(gx) * (digx)_j *)
  r = Table[

    Print@{j, RIFunctionEvaluateDerivative[f, j, gx], "*",digx~RVVLookupVar~j};
    RIFunctionEvaluateDerivative[f, j, gx]~RVVScale~(digx~RVVLookupVar~j)

    ,{j, RIFunctionArguments@f}

  ];

  Print@r;

  (* Dot *)
  Fold[
    RVVPlus, r
  ]
(* todo could use evaluate derivative multiple, retrieve a matrix and or do a 'matrix multiplication via RVarval *)

]
];

DefinePublicFunction[
RIFunctionEvaluateDerivativeNameless[
  fg : RIFunction[Composition, f_RIFunction, g_RIFunction]
  , i_
  ,x_?NumericVectorQ
] /; RIFunctionArguments@fg~Contains~i
  ,"",
  Module[{
  r
  , gx = g~RIFunctionEvaluateNameless~x
  , digx = RIFunctionEvaluateDerivativeNameless[g, i, x]},

  Print@gx;
  Print@digx;

  (* compute each d_jf(gx) * (digx)_j *)
  r = Table[

    RIFunctionEvaluateDerivativeNameless[f, RIFunctionArguments[f][[j]], gx] * (* RVVScale in named version *) digx[[j]] (* todo use indexed version *)

    ,{j, Length@RIFunctionArguments@f}

  ];

  Print@r;

  (* Dot *)
  Fold[
    Plus (*RVVPlus in named version *), r
  ]
(* todo could use evaluate derivative multiple, retrieve a matrix and or do a 'matrix multiplication via RVarval *)

]

];

(* ^^ End ^^ *)


(* -- Purpose -- *)
(*
Given an injective

s: A -> B

construct the selector function

select: (B -> R) -> (A -> R)

with

select(x)(a) = x(s(a))

*)



(* ^^ End ^^ *)


(* -- Purpose -- *)
(*

*)

(* Name *)


DefinePublicFunction[
RIFunctionMakeMultiDerivative[

(* Arguments *) PatternSequence[]

  , f_RIFunction
  , y_List
  , pairing_ : (Reverse@*list)

(* Code *) ] /; RIFunctionArguments@f~ContainsAll~y

  ,"For f: (I -> R) -> (J -> R) and I' subset I,
construct
d_I' f: (I -> R) -> (J x I' -> R)

With

(d_I' f x)({j, i}) = (d_i f x)(i)

for all x (multiple derivatives together, jacobian matrix)

By changing pairing, you can index the result differently. The default is (Reverse@*List), to arrive at a
row-column indexed jacobian matrix with individual derivatives by single variables in columns and gradients in rows."


  ,With[{ (* TODO Fail for y not a sublist of Arguments *)
  components = FiniteMappingMakeFromLists[y, RIFunctionMakeDerivative[f,#]&/@y]}, Null

; RIFunction[Multiple, components, pairing]

(* ^^ End ^^ *) ]
];
(* TODO Indexed version *)


DefinePublicFunction[
RIFunctionMakeFullJacobian[f_RIFunction] ,"", RIFunctionMakeMultiDerivative[f, RIFunctionArguments@f]
];


  DefinePublicFunction[
RIFunctionMakeMultiDerivativeIndexed[f_RIFunction, y: {__Integer}, pairing_ : (Reverse@*list)] ,"",
    RIFunctionMakeMultiDerivative[f, RIFunctionArguments[f][[y]], pairing]
  ];

(* -- Purpose -- *)
(*

*)

(* Name *)


DefinePublicFunction[
RIFunctionMakeDerivativeIndexed[

(* Arguments *) PatternSequence[]

  , f : RIFunction[ExpressionList, expr_, vars_List, ___]
  , i_Integer
(* Code *) ] ,
  "df
--
dx

where x is specified by index into Arguments"
  ,RIFunctionMakeDerivative[f, RIFunctionArguments[f][[i]] ]
  ];


(* -- Purpose -- *)
(*
C code with variable names generated as v_x_y,... where
x, y etc are the atoms appearing in the name of the i-th variable.
` and . in atoms are replaced by _
*)
(* TODO *)
RIFunctionCForm;

(* -- Purpose -- *)
(*


*)

DefinePublicFunction[
RIFunctionCFormExpressions[e_RIFunction]
  ,"Produce C code evaluating this function.

C code with variables x[i] (x(i), function call - uniformity - you have to define this to an array indexing operation ... TODO), 0-based

f(const double* const x, double* out)"
  ,With[
  {xrep = RuleMapIndexed[x@CIndex@#2 & , RIFunctionArguments@e]},
    cformSymbolic[#, xrep] & /@ RIFunctionExpressionList@e
  ]
  ];

DefinePublicFunction[
RIFunctionCFormOutputArrayAssignments[e_RIFunction, out_String : "out"] ,"", {
    cforms = RIFunctionCFormExpressions@e
  }~With~CBlock[
    CAssign[CArray[out,  CIndex@#2], #1]& ~MapIndexed~ cforms
  ]
];

(* TODO Allow the user to specify the input-output format more freely,
or use a general and simple enough format and provide examples of how to write wrappers *)

DefinePublicFunction[
RIFunctionCFormIndexedFunction[e_RIFunction, f_String : "f", out_String : "out"] ,"", CFunction[
    "void", f, {{{"double", "const", "*", "const"}, "x"}, {"double", out}},
    RIFunctionCFormOutputArrayAssignments[e, out]
  ]
];


DefinePublicFunction[
RIFunctionCFormAllDerivativesIndexed[e_RIFunction, out_String : "out", i_String : "i"] ,"",
    CSwitch[i, (* TODO CSwitch creates spurious error message StringTrim::strse: String or list of strings expected at position 1 in StringTrim[0]. >> even when used correctly (reported)*)
      Sequence@@Flatten[
        {CIndex@#2, {RIFunctionCFormOutputArrayAssignments[RIFunctionMakeDerivative[e, #1]],CBreak[]}}&
        ~MapIndexed~
        RIFunctionArguments@e
      ,1]
      ]
    ];


DefinePublicFunction[
RIFunctionCFormAllDerivativesIndexedFunction[e_RIFunction, df_String : "df", out_String : "out", i_String : "i"] ,"",  CFunction[
  "void", df, {{"int",i},{{"double", "const", "*", "const"}, "x"}, {"double", out}},
  RIFunctionCFormAllDerivativesIndexed[e, out, i]
]
];

(* TODO allow constructing matrix output *)

CIndex[{i_Integer}] := i-1;

End[];

EndPackage[];