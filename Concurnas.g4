grammar Concurnas;

@header{
	package com.concurnas.compiler;
	import java.util.Map;
	import java.util.HashMap;
	import java.util.List;
	import java.util.ArrayList;
	import java.util.LinkedHashSet;
	import java.util.Stack;
}

@lexer::members {
	private int defaultTransHandlerId = 0;

	boolean skipNewLine=false;
	Stack<Boolean> prevskip = new Stack<Boolean>();

	public boolean permitDollarPrefixRefName = false;
}

@parser::members{
	private Stack<Boolean> isInArrayDef = new Stack<Boolean>();
	private boolean notArrayDef(){
		if(!isInArrayDef.isEmpty()){
			return !isInArrayDef.peek();
		}

		return true;
	}

	public void setInArrayDef(){
		isInArrayDef.push(true);
	}

	public void setNotInArrayDef(){
		isInArrayDef.push(false);
	}

	public void popInArrayDef(){
		isInArrayDef.pop();
	}
}

code
	: line* EOF
 ;


line
	:
	 stmts
	| nls=NEWLINE+
	| nop=';' ';' //nop
 ;

stmts: csOrss (     (';'|NEWLINE+)  csOrss)*   (';'|NEWLINE+)? ;

csOrss: comppound_str_concat|compound_stmt|simple_stmt;//actor MyClass(12) expression reachable version takes priority over compound statement

comppound_str_concat: compound_stmt additiveOp_*;//permits us to do this: a = {} + "str concat"


single_line_block
  :  NEWLINE* '=>' NEWLINE* single_line_element (';' single_line_element)* ';'? //NEWLINE
  ;

single_line_element: comppound_str_concat|compound_stmt|simple_stmt| (nop=';');

///////////// simple_stmt /////////////

simple_stmt :
	exprListShortcut
  | assignment
  | annotations
  | assert_stmt
  | delete_stmt
  | return_stmt
  | throw_stmt
  | flow_stmt
  | import_stmt
  | typedef_stmt
  | await_stmt
  | lonleyExpression
  //| u=using_stmt    {$ret = $u.ret;} //dsls
  ;

lonleyExpression: expr_stmt_tuple;

exprListShortcut :
 	a1=refName atype1=mustBeArrayType ( aassStyle=assignStyle arhsExpr = expr_stmt_tuple)? //to deal with cases: myvar Integer[][] = null - which are otherwise picked up by next line...
	| e1=refName e2=refName (rest=expr_stmt_)+;//to deal with cases such as: thing call arg. But watch out for: thing String[]

mustBeArrayType: (primitiveType | namedType | tupleType) trefOrArrayRef+ | funcType trefOrArrayRef*;

transientAndShared: //can be defined either way around
	trans=TRANSIENT_N
	| shared=SHARED_N
	| lazy=LAZY_N
	| trans=TRANSIENT_N shared=SHARED_N
	| trans=TRANSIENT_N lazy=LAZY_N
	| lazy=LAZY_N shared=SHARED_N
	| shared=SHARED_N trans=TRANSIENT_N
	| lazy=LAZY_N trans=TRANSIENT_N
	| shared=SHARED_N lazy=LAZY_N
	| lazy=LAZY_N shared=SHARED_N trans=TRANSIENT_N
	| lazy=LAZY_N trans=TRANSIENT_N shared=SHARED_N
	| shared=SHARED_N lazy=LAZY_N trans=TRANSIENT_N
	| shared=SHARED_N trans=TRANSIENT_N lazy=LAZY_N
	| trans=TRANSIENT_N lazy=LAZY_N shared=SHARED_N
	| trans=TRANSIENT_N shared=SHARED_N lazy=LAZY_N
;

assignment:
	 (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier? valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?    (refname = refName typeNoNTTuple) ( assStyle=assignStyle ( rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple ) | onchangeEveryShorthand )?
	| (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier?  valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?  refname = refName (refCnt+=':')* ( assStyle=assignStyle (  rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple  )   | onchangeEveryShorthand)
	| LPARA {setNotInArrayDef();} assignmentTupleDereflhsOrNothing (',' assignmentTupleDereflhsOrNothing)+ RPARA {popInArrayDef();} ( assStyle=assignStyle (  rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple  )   | onchangeEveryShorthand)
	| ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier? valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?  assignee=expr_stmt ( assStyle=assignStyle  (rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple )  | onchangeEveryShorthand)
	| lonleyannotation = annotation
	;

assignmentForcedRHS:
	 (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier? valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?    (refname = refName type) ( assStyle=assignStyle ( rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple ) | onchangeEveryShorthand )
	| (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier?  valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?  refname = refName (refCnt+=':')* ( assStyle=assignStyle (  rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple  )   | onchangeEveryShorthand)
	| LPARA {setNotInArrayDef();} assignmentTupleDereflhsOrNothing (',' assignmentTupleDereflhsOrNothing)+ RPARA {popInArrayDef();} ( assStyle=assignStyle (  rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple  )   | onchangeEveryShorthand)
	| ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier? valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?  assignee=expr_stmt ( assStyle=assignStyle  (rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple )  | onchangeEveryShorthand)
	| lonleyannotation = annotation
	;

assignmentTupleDereflhsOrNothing:
	assignmentTupleDereflhs?
;

assignmentTupleDereflhs:
	(annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?    (refname = refName type)
	| (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?  refname = refName (refCnt+=':')*
	| ppp? transAndShared=transientAndShared? valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?  assignee=expr_stmt
;


//assignmentNamdAndTypeOnly: (annotations NEWLINE? )? ppp? trans='transient'? gpuVarQualifier? valvar=(VAL|VAR)? prefix=('-'|'+'|'~')?    (refname = refName type);

onchangeEveryShorthand :
	('<-' | isEvery='<=') (LPARA {setNotInArrayDef();} onChangeEtcArgs RPARA {popInArrayDef();})? expr_stmt_tuple
	;

assert_stmt
	: ASSERT_N e=expr_stmt_ s=stringNode?
	;

delete_stmt
	: DEL_N expr_stmt (',' expr_stmt)* ','?
	;

flow_stmt
	: BREAK_N expr_stmt_tuple?
	| CONTINUE_N expr_stmt_tuple?
	;

throw_stmt : THROW_N expr_stmt;
return_stmt : RETURN_N expr_stmt_tuple? ;

import_stmt
	: import_stmt_impot
	| import_stmt_from
	;

import_stmt_impot
	: (IMPORT_N | using=USING_N) prim=dotted_as_name (',' sec+=dotted_as_name )* ','?
	| (IMPORT_N | using=USING_N) dotted_name DOT star='*'
	;

import_stmt_from
  	: FROM_N dotted_name (IMPORT_N | using=USING_N) (import_as_name (',' import_as_name )* ','? | star='*')
	;

import_as_name
    : NAME (AS_N NAME)?
	;

dotted_as_name
	: dotted_name (AS_N NAME)?
	;

typedef_stmt
  : pppNoInject? TYPEDEF_N NAME typedefArgs? '='  type
  ;

typedefArgs: '<' NAME  (',' NAME )* ','? '>';


await_stmt
 :  AWAIT_N LPARA {setNotInArrayDef();} onChangeEtcArgs  (  ';'  ( expr_stmt | block )?  )? RPARA {popInArrayDef();} //TODO: expand the syntax permitte here
;

///////////// compound stmt /////////////

compound_stmt
	:
	( (annotations NEWLINE*)?
		(
			 funcdef
		  	| constructorDef
			| classdef
			| objectProvider
			| annotationDef
			| enumdef
		)
	)
	| comp = compound_stmt_atomic_base
;


compound_stmt_atomic_base
  : fors
  | match_stmt
  | if_stmt
  | async_block
  | while_stmt
  | loop_stmt
  | try_stmt
  | block_async
  | with_stmt
  | trans_block
  | init_block
  | sync_block
  | onchange
  | every
  ;

blockOrBlock : block | single_line_block;

funcdef
    : ppp?  ( (override=OVERRIDE_N)? ('def' | gpuitem = GPUDEF_N | gpuitem =GPUKERNEL_N kerneldim = intNode) | (override=OVERRIDE_N) )  ( (extFuncOn ('|' extFuncOn)* )? funcDefName DOT?)?
    		genericQualiList?
    		LPARA {setNotInArrayDef();} funcParams? RPARA {popInArrayDef();} retTypeIncVoid? blockOrBlock?
	;

funcDefName
	: NAME //now for operator overloading...
	| '='
	| '+' | '-'
	| '*'| '/'
	| '**'| '++' | '--'
	| MOD_N | OR_N | AND_N | NOT_N
	| '<''<' | '>' '>' | '>' '>' '>'
	| COMP_N | BAND_N | BOR_N | BXOR_N
	| '-=' | '*=' | '/=' | MOD_N '=' | '**=' | '+=' | OR_N '=' | AND_N '=' | '<<='
	| '>>=' | '>>>=' | BAND_N '=' | BOR_N '=' | BXOR_N '='
	;


extFuncOn
	: extFunOn=type
	//: extFunOn=namedType_ExActor|extFunOnPrim=primitiveType
	;

funcParams
  : funcParam (',' funcParam)* (',')?
  ;

funcParam:
	annotations? sharedOrLazy? (gpuVarQualifier gpuInOutFuncParamModifier?)? (isFinal=VAL|VAR)?
	(
		NAME ( type isvararg='...'?)? ('=' expr_stmt )
		|
		NAME? ( type isvararg='...'?) ('=' expr_stmt )?
	)
	;


sharedOrLazy:
	lazy = LAZY_N shared=SHARED_N?
	| shared=SHARED_N lazy=LAZY_N?
;

gpuVarQualifier: GLOBAL_N | LOCAL_N | CONSTANT_N;

gpuInOutFuncParamModifier : IN_N | OUT_N;

constructorDef
  :  ppp? DEF_N? THIS_N LPARA {setNotInArrayDef();} funcParams? RPARA {popInArrayDef();} blockOrBlock
  ;



objectProviderArgs
	: objectProviderArg (',' objectProviderArg )* (',')?
	;

objectProviderArg:
	annotations? pppNoInject? transAndShared=transientAndShared?
		(isFinal=VAL|VAR)? NAME  ((type | (refCnt+=':')+) isvararg='...'? )? ( '='  expr_stmt )?
	;



objectProvider: pppNoInject? (trans=TRANSIENT_N | shared=SHARED_N)? PROVIDER_N providerName=NAME
genericQualiList?
(LPARA {setNotInArrayDef();} objectProviderArgs? RPARA {popInArrayDef();})?
	objectProviderBlock
	;

objectProviderBlock:
	LBRACE NEWLINE* linex+=objectProviderLine* RBRACE
	;

objectProviderLine:
	(pppNoInject? (single=SINGLE_N | shared=SHARED_N)? PROVIDE_N genericQualiList? lazy=LAZY_N? fieldName=stringNode? provName=NAME? provide=type ('=>' provideExpr=expr_stmt  | objectProviderNestedDeps )?
	| opdl=objectProviderLineDep) (';'|NEWLINE+)
	;

objectProviderNestedDeps : LBRACE ((';'|NEWLINE+)? nestedDep+=objectProviderLineDep ((';'|NEWLINE+) nestedDep+=objectProviderLineDep)* )? (';'|NEWLINE+)? RBRACE;

objectProviderLineDep: (single=SINGLE_N | shared=SHARED_N)? lazy=LAZY_N? fieldName=stringNode? nameFrom=type ('=>' exprTo=expr_stmt | '<=' typeOnlyRHS=type objectProviderNestedDeps?)?;


classdef
	: p=ppp? aoc=(ABSTRACT_N | OPEN_N | CLOSED_N)? (trans=TRANSIENT_N | shared=SHARED_N)? (CLASS_N |istrait=TRAIT_N | isactor=ACTOR_N) className=NAME
		genericQualiList?
      (LPARA {setNotInArrayDef();} classdefArgs? RPARA {popInArrayDef();})?
		(
			istypedActor=OF_N ( typedActorOn=namedType_ExActor typeActeeExprList=expr_stmtList?  )
		)?
		(
			NEWLINE* (EXTENDS_N | '<') superCls=dotted_name ('<' superGenType+=type (',' superGenType+=type )* ','? '>')?
		 	 extExpressions=expr_stmtList?
		)?
		(
			NEWLINE* (WITH_N | '~') implInstance ((',' implInstance)*) ','?
		)?
		(block | NEWLINE+ | EOF)
	;


implInstance:
	impli=dotted_name ('<' implType+=type (',' implType+=type )* ','? '>')?
	;

localclassdef
	: (trans=TRANSIENT_N | shared=SHARED_N)? (CLASS_N | isactor=ACTOR_N)
		genericQualiList?
		(LPARA {setNotInArrayDef();}  classdefArgs? RPARA {popInArrayDef();})?
		(
			istypedActor=OF_N ( typedActorOn=namedType_ExActor typeActeeExprList=expr_stmtList?  )
		)?
		(
			(EXTENDS_N | '<') superCls=dotted_name ('<' type (',' type )* ','? '>')?
		 	 extExpressions=expr_stmtList?
		)?
		(
			(WITH_N | '~') implInstance ((',' implInstance)* | ','?)
		)?
		block
	;

anonclassdef
	: (NEW_N isactor=ACTOR_N?  )
		(
			superCls=dotted_name ('<' type (',' type )* ','? '>')?
		)

		(
			(WITH_N | '~') implInstance ((',' implInstance)* | ','?)
		)?
		block?
	;


expr_stmtList : (LPARA {setNotInArrayDef();} (expr_stmt ( ',' expr_stmt )*)? RPARA {popInArrayDef();});


classdefArgs
	: classdefArg (',' classdefArg )* (',')?
	;

classdefArg:
	annotations? pppNoInject? (override=OVERRIDE_N)? transAndShared=transientAndShared? (isFinal=VAL|VAR)? (prefix=('-'|'+'|'~'))? NAME  ((type | (refCnt+=':')+) isvararg='...'? )? ( '='  expr_stmt )?
	;


annotationDef
  : pppNoInject? ANNOTATION_N NAME
      ( LPARA {setNotInArrayDef();} annotationArg  (',' annotationArg )* ','? RPARA {popInArrayDef();} )?
      block?
  ;

annotationArg:
	annotations? NAME type? ('=' expr_stmt)?
	;


enumdef
  : pppNoInject? ENUM_N NAME (LPARA {setNotInArrayDef();} classdefArgs? RPARA {popInArrayDef();})? enumblock
  ;

enumItem
  : annotations? NAME pureFuncInvokeArgs? block?//dont need to have args?
  ;

enumblock
	: LBRACE NEWLINE* enumItem ( NEWLINE* ',' NEWLINE* enumItem )* (';'|NEWLINE*) line* NEWLINE* RBRACE
	;

///////////// compound stmt:compound_stmt_atomic_base /////////////

fors
  : for_stmt
  | for_stmt_old
  ;

for_stmt
	: forblockvariant LPARA {setNotInArrayDef();} ( (localVarName=NAME localVarType=type?) | ( LPARA {setNotInArrayDef();} forVarTupleOrNothing (',' forVarTupleOrNothing)+ RPARA {popInArrayDef();}))  'in' expr=expr_stmt_tuple
	    (';'  (idxName=NAME idxType=type? (('\\=' | '=') idxExpr=expr_stmt)?) )? RPARA {popInArrayDef();} mainblock=block
	    (NEWLINE* ELSE_N elseblock=block )?
	;

forVarTupleOrNothing: forVarTuple?;

forVarTuple: localVarName=NAME localVarType=type?;

for_stmt_old
  : forblockvariant LPARA {setNotInArrayDef();} ( (NAME type? assignStyle assigFrom=expr_stmt_tuple) | assignExpr=expr_stmt_tuple )?
					  	';' check=expr_stmt?
					  	';' postExpr=expr_stmt?
				  	RPARA {popInArrayDef();}
  	  	mainblock=block
    ( NEWLINE* ELSE_N elseblock=block  )?
  ;

forblockvariant: FOR_N | PARFOR_N | PARFORSYNC_N;


match_stmt
  :
  MATCH_N NEWLINE* LPARA {setNotInArrayDef();} NEWLINE* simple_stmt NEWLINE* RPARA {popInArrayDef();} NEWLINE*
   LBRACE NEWLINE*
	   match_case_stmt*
	   ( NEWLINE* ELSE_N  elseb=blockOrBlock)?
    NEWLINE* RBRACE
  ;

match_case_stmt: match_case_stmt_case | match_case_stmt_nocase;

match_case_stmt_case:
    (NEWLINE* CASE_N LPARA {setNotInArrayDef();}
    	  (	((  match_case_stmt_typedCase //CaseExpressionAssign
		    |  match_case_stmt_assign  //TypedCaseExpression
		    |  match_case_stmt_assignTuple
		    |  case_expr_chain_Tuple
		    |  case_expr_chain_or //, passthrough
		    | match_case_assign_typedObjectAssign
	      	) matchAlso=match_also_attachment?)
	      | justAlso=match_also_attachment//needs an also...
	      )  RPARA {popInArrayDef();}
	    blockOrBlock
	)
   ;

match_case_stmt_nocase:
    (NEWLINE*
    	  (	(( match_case_stmt_typedCase //TypedCaseExpression
		    |  match_case_stmt_assign  //CaseExpressionAssign
		    |  match_case_stmt_assignTuple
		    |  case_expr_chain_Tuple
		    |  case_expr_chain_or //, passthrough
		    | match_case_assign_typedObjectAssign
	      	) matchAlso=match_also_attachment?)
	      | justAlso=match_also_attachment//needs an also...
	      )
	    blockOrBlock
	)
   ;

match_also_attachment: ALSO_N expr_stmt;

match_case_stmt_typedCase: (type ( OR_N type  )* ( ';' (case_expr_chain_Tuple | case_expr_chain_or) )? ); //TypedCaseExpression

match_case_stmt_assign: ( ( var=VAR | isfinal=VAL)? NAME (type ('or' type)*)? ( ';'  expr_stmt)? );//CaseExpressionAssign

match_case_assign_typedObjectAssign: (var=VAR | isfinal=VAL)? NAME bitwise_or;

match_case_stmt_assignTuple:  ( LPARA {setNotInArrayDef();} matchTupleAsignOrNone (',' matchTupleAsignOrNone)+ RPARA {popInArrayDef();}) ( ';'  expr_stmt)? ;//CaseExpressionAssign

matchTupleAsignOrNone: matchTupleAsign?;

matchTupleAsign: NAME type;

case_expr_chain_Tuple:
	LPARA {setNotInArrayDef();} case_expr_chain_orOrNone (',' case_expr_chain_orOrNone )+  RPARA {popInArrayDef();}
;

case_expr_chain_orOrNone: case_expr_chain_or?;

case_expr_chain_or
  :
   ce = case_expr_chain_and ( OR_N case_expr_chain_and )*
  ;

case_expr_chain_and
  :
   ce = case_expr ( AND_N case_expr )*
  ;

case_expr
  : bitwise_or ( case_operator)?
  | case_operator_pre bitwise_or
  ;


case_operator : '==' | '<' | '<>' | '&==' | '&<>' | '>' | '>==' | '<==' ;

case_operator_pre
  :  case_operator | IN_N | NOT_N IN_N
  ;



if_stmt
	:
	  IF_N  LPARA {setNotInArrayDef();}  ifexpr=expr_stmt  RPARA {popInArrayDef();}  ifblk=block
		elifUnit*
		(  ( NEWLINE* ELSE_N elseblk=block) )?
	;

elifUnit :  NEWLINE* (ELIF_N | ELSE_N IF_N) LPARA {setNotInArrayDef();}  expr_stmt  RPARA {popInArrayDef();}  block ;

async_block
 : ASYNC_N LBRACE
  (
    line
    | PRE_N preblk+=block
    | POST_N postblk+=block
  )*
 RBRACE
 ;

while_stmt
	: WHILE_N LPARA {setNotInArrayDef();}  mainExpr=expr_stmt
						(';'  (idxName=NAME idxType=type? (('\\=' | '=') idxExpr=expr_stmt)?) )?// | nameAlone=NAME
	RPARA {popInArrayDef();} mainBlock=block
	  ( NEWLINE* ELSE_N elseblock=block )?
	;

loop_stmt
  : LOOP_N
  (LPARA {setNotInArrayDef();}  (idxName=NAME idxType=type? (('\\=' | '=') idxExpr=expr_stmt)?)  RPARA {popInArrayDef();} )? mainBlock=block
  ;

try_stmt
	:	TRY_N  (LPARA {setNotInArrayDef();}  simple_stmt  (';'? simple_stmt  )* ';'?  RPARA {popInArrayDef();})? mainblock=block
	  catchBlock*
	  ( NEWLINE* FINALLY_N  finblock=block )?
	;

catchBlock: NEWLINE* CATCH_N  LPARA {setNotInArrayDef();}   NAME  (type   (OR_N  type   )* )? RPARA {popInArrayDef();}  block ;

block_async
  : block_ ( async='!'( LPARA {setNotInArrayDef();} executor=expr_stmt RPARA {popInArrayDef();} )? )?
  ;


with_stmt
	: WITH_N
	LPARA {setNotInArrayDef();} expr_stmt RPARA {popInArrayDef();}
	block
	;

trans_block
  : TRANS_N  b=block
  ;

init_block
  : INIT_N block
  ;

sync_block
  : SYNC_N b=block
  ;

onchange
 :  ONCHANGE_N (LPARA {setNotInArrayDef();} onChangeEtcArgs (';' opts+=NAME (',' opts+=NAME )* (',')? )? RPARA {popInArrayDef();})? (block )//| single_line_block
 ;


every
 :  EVERY_N (LPARA {setNotInArrayDef();} onChangeEtcArgs (';' opts+=NAME (',' opts+=NAME )* (',')? )? RPARA {popInArrayDef();})? (block )//| single_line_block
;

///////////// annotations /////////////

annotations
  : annotation (NEWLINE* ','? NEWLINE* annotation)*
  ;

annotation
  :'@' (LBRACK loc+=(THIS_N | NAME) (',' loc+=(THIS_N | NAME) )* (',')? RBRACK)?
     dotted_name
    ( LPARA {setNotInArrayDef();} (namedAnnotationArgList |  expr_stmt  )?  RPARA {popInArrayDef();} )?
  ;

namedAnnotationArgList
  : n2expr (',' n2expr )*  ','?
  ;

n2expr : NAME '=' expr_stmt;


///////////// common /////////////

genericQualiList: '<' nameAndUpperBound  (',' nameAndUpperBound )* ','? '>';

nameAndUpperBound: NAME namedType? nullable='?'?;

dottedNameList
	: dotted_name (',' dotted_name  )* ','?
	;

inoutGenericModifier : IN_N | OUT_N;

onChangeEtcArgs
 : onChangeEtcArg (',' onChangeEtcArg)*
 ;

onChangeEtcArg
	: valvar=(VAL|VAR)? NAME (type | (refCnt+=':')+)? '=' expr_stmt
	| expr_stmt
	;

dotted_name
	: NAME ( DOT  NAME )*
	;

assignStyle : '\\=' | '=' | '+=' | '-=' | '*=' | '/=' | MOD_N '=' | '**=' | OR_N '=' | AND_N '=' | '<<=' | '>>=' | '>>>=' | BAND_N '=' | BOR_N '='| BXOR_N '=';

block
  : NEWLINE* block_
  ;

block_
  : LBRACE  line* RBRACE
  ;

pureFuncInvokeArgs
	: LPARA {setNotInArrayDef();} (pureFuncInvokeArg (',' pureFuncInvokeArg)* ','? )?  RPARA {popInArrayDef();}
	;

pureFuncInvokeArg
	: (NAME  '=' )? ( expr_stmt  |  primitiveType |  funcType | tupleType)
	;


funcRefArgs
  : LPARA {setNotInArrayDef();} (  funcRefArg (  ',' funcRefArg )* ','? )? RPARA {popInArrayDef();}
  ;

funcRefArg
	: (NAME '=')? ( '?' lazy=LAZY_N? type | lazy=LAZY_N? primitiveType | lazy=LAZY_N? funcType nullable='?'? | lazy=LAZY_N? LPARA {setNotInArrayDef();} tupleType RPARA {popInArrayDef();} nullable='?'? | lazy=LAZY_N? namedType nullable='?' | expr_stmt | (refcnt+=':')+ )
	;

genTypeList
  :
	'<' genTypeListElemnt ( ',' genTypeListElemnt )* ','? '>'
  ;

genTypeListElemnt
	: '?'   | ( inoutGenericModifier?  type )
	;


///////////// types /////////////
trefOrArrayRef:
		hasAr=LBRACK (arLevels=intNode) RBRACK
	| (hasArAlt+=LBRACK RBRACK)+
	| refOrNullable
	;

refOrNullable: ':' dotted_name?
	| nullable='?'
	| nullableErr='??'
	;


type:
	 bareTypeParamTuple ('|' bareTypeParamTuple)* trefOrArrayRef*
	 ;


bareTypeParamTuple:
	pointerQualifier? primitiveType
	| namedType
	| funcType
	| LPARA {setNotInArrayDef();} tupleType RPARA {popInArrayDef();}
	;


typeNoNTTuple:
	 bareTypeParamTupleNoNT ('|' bareTypeParamTupleNoNT)* trefOrArrayRef*
	 ;

bareTypeParamTupleNoNT:
	pointerQualifier? primitiveType
	| namedType
	| funcType
	| LPARA {setNotInArrayDef();} tupleTypeNoNT RPARA {popInArrayDef();}
	;

tupleTypeNoNT : bareButTupleNoNT (',' bareButTupleNoNT )+ ;

bareButTupleNoNT: primitiveType | funcType;


ppp: inject=INJECT?  pp=(PRIVATE | PROTECTED | PUBLIC | PACKAGE)
	| inject=INJECT pp=(PRIVATE | PROTECTED | PUBLIC | PACKAGE)?;

pppNoInject: PRIVATE | PROTECTED | PUBLIC | PACKAGE;

pointerQualifier : (cnt+='*'|cnt2+='**')+;

//typeNoPrim : namedType | funcType | tupleType;

namedType
  : isactor=ACTOR_N namedType_ExActor?
  | namedType_ExActor
  ;


namedType_ExActor
  :  primaryName=dotted_name priamryGens=genTypeList? (DOT nameAndgens)* ( OF_N of=namedType)? //'of' namedType ?
  ;

nameAndgens : NAME genTypeList?;

tupleType : bareButTuple (',' bareButTuple )+ ;

bareButTuple: (primitiveType | namedType | funcType ) trefOrArrayRef*;

funcType :
	funcType_
	| LPARA {setNotInArrayDef();} funcType_ RPARA {popInArrayDef();}
	;

funcType_
	: genericQualiList?
	    LPARA {setNotInArrayDef();} ( (type (',' type)*)? ','?  | constr='*' ) RPARA {popInArrayDef();} retTypeIncVoid
	;

retTypeIncVoid
  :  type
  |  VOID_N
  ;

primitiveType: (BOOLEAN_N | BOOL_N) | SIZE_T_N | INT_N | LONG_N | FLOAT_N | DOUBLE_N | BYTE_N | SHORT_N | CHAR_N | LAMBDA_N;

///////////// expresssions /////////////

expr_stmt_tuple : expr_stmt ( (',' expr_stmt)+ ','? )?;

expr_stmt
	: for_list_comprehension
	;

for_list_comprehension
	: mainExpr=expr_list ( flc_forStmt_+  (IF_N condexpr=expr_stmt)?)?
	;

flc_forStmt_:
	forblockvariant (localVarName=NAME localVarType=type?  | ( LPARA {setNotInArrayDef();} forVarTupleOrNothing (',' forVarTupleOrNothing)+ RPARA {popInArrayDef();}))   IN_N expr=expr_list
	;



expr_list
	: /*block_async |*/  lambdadefOneLine | lambdadef | anonLambdadef | expr_stmt_+ ;//shortcut in the lambdadef as it ends with a newline so cannot be an expr stmt


lambdadefOneLine : annotations? DEF_N genericQualiList?  LPARA {setNotInArrayDef();}  funcParams? RPARA {popInArrayDef();} retTypeIncVoid? single_line_block ;

lambdadef
    : annotations? DEF_N genericQualiList?  LPARA {setNotInArrayDef();}  funcParams? RPARA {popInArrayDef();} retTypeIncVoid? (block | (single_line_block NEWLINE+))  ;


anonLambdadef : ((NAME (',' NAME)*) | LPARA {setNotInArrayDef();} ( typeAnonParam (',' typeAnonParam)*) RPARA {popInArrayDef();}) retType=type? single_line_block;

typeAnonParam: NAME type?;

expr_stmt_: if_expr//for_list_comprehension
 	;

if_expr: op1=expr_stmt_or (IF_N test=expr_stmt_or ELSE_N op2=expr_stmt_or )?;

expr_stmt_or: head=expr_stmt_and ( OR_N  ors+=expr_stmt_and)*;

expr_stmt_and: head=bitwise_or ( AND_N  ands+=bitwise_or)*;


bitwise_or: head=bitwise_xor ( BOR_N  ands+=bitwise_xor)*;
bitwise_xor: head=bitwise_and ( BXOR_N  ands+=bitwise_and)*;
bitwise_and: head=expr_stmt_BelowEQ ( BAND_N  ands+=expr_stmt_BelowEQ)*;


expr_stmt_BelowEQ : head=instanceof_expr ( eqAndExpression_)*;
eqAndExpression_: equalityOperator instanceof_expr;

instanceof_expr : castExpr (( IS_N | invert=ISNOT_N | ( IS_N invert=NOT_N)) type (OR_N type)* )?;

castExpr: lTGTExpr (AS_N type)*;

lTGTExpr : shiftExpr ( relOpAndExpression_)*;
relOpAndExpression_: relationalOperator  shiftExpr;


shiftExpr: additiveExpr ( shiftExprOp_)*;
shiftExprOp_: (lshift='<' '<' | rshift='>' '>' | rshiftu='>' '>' '>')  additiveExpr;


additiveExpr: divisiveExpr ( additiveOp_)*;
additiveOp_ : ({notArrayDef()}? op='-' divisiveExpr ) | op='+'  divisiveExpr;

divisiveExpr: powExpr ( divisiveExprOP_)*;
divisiveExprOP_:op=('*'|'/'| MOD_N)  powExpr;

powExpr :  lhs=notExpr ( '**'  rhs+=notExpr)*;

notExpr: isnot=NOT_N? containsExpr;

containsExpr : lhs=prefixExpr ( (invert=NOT_N? IN_N)  rhs=prefixExpr)?;

prefixExpr : prefixOp=('++' | '--' | '-' | '+' | COMP_N)?	 postfixExpr;

postfixExpr : sizeOfExpr postfixOp=('++' | '--')?;

sizeOfExpr: (sizeof=SIZEOF_N  ('<' variant=dotted_name '>')? )? asyncSpawnExpr;

asyncSpawnExpr: notNullAssertion (isAsync='!' (LPARA {setNotInArrayDef();} expr_stmt RPARA {popInArrayDef();})? )?;


notNullAssertion :  elvisOperator ( nna='??')?;
elvisOperator :  lhsExpr=vectorize ( '?:'  elsExpr=if_expr)?  ;


vectorize: primary=vectorize vectorize_element+
	| passthrough=dotOperatorExpr
	;

vectorize_element:
	nullsafe='?'? ('^' (doubledot='^')? ) (constru=constructorInvoke | arrayRefElements+ | afterVecExpr=refName genTypeList? (pureFuncInvokeArgs | '&' funcRefArgs?)? )?
	;

dotOperatorExpr: ((pntUnrefCnt+='*'|pntUnrefCnt2+='**' )+ | address='~')? copyExpr ( NEWLINE* dotOpArg NEWLINE* copyExpr)*;

copyExpr : expr_stmt_BelowDot (isCopy='@' (hasCopier=LPARA {setNotInArrayDef();} ( (copyExprItem  (',' copyExprItem)* ','?)? (';' modifier+=NAME (',' modifier+=NAME)* ','?  )? ) RPARA {popInArrayDef();} )? )?;

copyExprItem: ename=NAME '=' expr_stmt
	|  incName=NAME
	| '<' exclName+=NAME (',' exclName+=NAME)* ','? '>'
	| (copyName=NAME | superCopy=SUPER_N )'@' ( hasCopier=LPARA {setNotInArrayDef();} ( (copyExprItem  (',' copyExprItem)* ','?)?  (';' modifier+=NAME (',' modifier+=NAME)* ','? )? ) RPARA {popInArrayDef();} )?
	;


expr_stmt_BelowDot //seperate rule for match operations - basically atoms
	: (isthis=THIS_N pureFuncInvokeArgs | SUPER_N pureFuncInvokeArgs) #superOrThisConstructorInvoke
	| NAME genTypeList? pureFuncInvokeArgs #FuncInvokeExprName
	| expr_stmt_BelowDot genTypeList? pureFuncInvokeArgs #FuncInvokeExpr

	| NAME genTypeList #RefQualifiedGeneric //{ $ret = new RefQualifiedGenericNamedType(getLine(input), getColumn(input), $namedT.text, $gg.genTypes);   }//ret namedType
    | LPARA {setNotInArrayDef();} ACTOR_N dotted_name genTypeList? RPARA {popInArrayDef();} #RefQualifiedGenericActor //{ $ret = gg==null? new RefQualifiedGenericNamedType(getLine(input), getColumn(input), $namedTa.ret, true) : new RefQualifiedGenericNamedType(getLine(input), getColumn(input), $namedTa.ret, $gg.genTypes, true);   }//ret namedType

	| expr_stmt_BelowDot genTypeList? '&' funcRefArgs? #FuncRefExpr
	| expr_stmt_BelowDot (refCnt+=':')* arrayRefElements+ (extraEmptyBracks+=LBRACK RBRACK)* #ArrayRefExpr
	| main=expr_stmt_BelowDot (refCnt+=':')+ post=expr_stmt_BelowDot? #RefExpr
	| notNullAssertion2 #AtomPassThrough
	;


arrayRefElements:  (nullSafe='?'? LBRACK arrayRefElement (',' arrayRefElement)* (trailcomma+=',')* RBRACK )  ;

notNullAssertion2 :  atom (NEWLINE* nna='??')?;

atom
	: classNode
	| thisNode
	| outNode
	| refName
	| superNode
	| ofNode
	| annotation
	| booleanNode
	| changedNode
	| arrayDef
	| mapDef
	| constructorInvoke
	| compound_stmt_atomic_base //set ret.setShouldBePresevedOnStack(true); after extraction
	| localclassdef
	| anonclassdef
	| lambdadef
	| intNode
	| longNode
	| shortNode
	| floatNode
	| doubleNode
	| nullNode
	| stringNode
	| regexStringNode
	| langExtNode
	| nestedNode
	;//move to add Labels above

nestedNode: {setNotInArrayDef();} LPARA expr_stmt_tuple RPARA {popInArrayDef();};

classNode: type '.' CLASS_N;
superNode : SUPER_N (LBRACK superQuali=dotted_name RBRACK)? ( dotOpArg expr_stmt_BelowDot )+;
changedNode: CHANGED_N;
outNode: OUT_N;
thisNode: THIS_N (LBRACK (thisQuali=dotted_name| thisQualiPrim=primitiveType) RBRACK)?;
nullNode: NULL_N;
intNode : INT;
longNode : LONGINT;
shortNode : SHORTINT;
floatNode : FLOAT;
doubleNode : DOUBLE;
booleanNode : TRUE_N | FALSE_N;
stringNode : STRING_ITMcit | isQuote=STRING_ITMquot;
langExtNode: name=NAME body=LANG_EXT;
regexStringNode : REGEX_STRING_ITM;
ofNode : OF_N;

arrayRefElement
	: (lhs=expr_stmt DDD rhs=expr_stmt)
	| (post=expr_stmt DDD )
	| ( DDD pre=expr_stmt)
	| ( simple=expr_stmt )
	;

refName
	: NAME
	;


dotOpArg
	: '.'|'\\.'|'..'|'?.'
	;

arrayDef
	: LBRACK RBRACK
	| (isArray=ALBRACK | LBRACK) NEWLINE* ((expr_stmt  ( ( NEWLINE* ',' NEWLINE* expr_stmt )+  ','? | NEWLINE*',')  ) | ',') NEWLINE* RBRACK //list def or single element array
	| arrayDefComplex
	;

arrayDefComplex: ALBRACK expr_stmt_+ arrayDefComplexNPLus1Row* (NEWLINE* ';' NEWLINE*)? RBRACK
	|  {setInArrayDef();} LBRACK expr_stmt_+ arrayDefComplexNPLus1Row* (NEWLINE* ';' NEWLINE*)? RBRACK {popInArrayDef();}
	;


arrayDefComplexNPLus1Row
	: (';' NEWLINE* | NEWLINE+) expr_stmt_+
	;

mapDef
	: LBRACE NEWLINE*  mapDefElement  (NEWLINE* ',' NEWLINE* mapDefElement )* (NEWLINE* ',')?  NEWLINE* RBRACE
	;

mapDefElement: (isDefault=DEFAULT_N | key=expr_stmt) NEWLINE*  '->' NEWLINE*  value=expr_stmt;


///////////// expresssions:constructors/////////////

constructorInvoke
	: namedActorConstructor
	| ( NEW_N  ( namedConstructor | arrayConstructor |  primNamedOrFuncType refOrNullable+  ) ) //
	| arrayConstructorPrimNoNew
	| newreftypeOnOwn
	;

namedConstructor
	: type  ( ( isConsRef='&' funcRefArgs) | isConsRef='&' | pureFuncInvokeArgs)
	;

namedActorConstructor
  : isNewDefiend=NEW_N? ACTOR_N namedType_ExActor ( (isConsRef='&' funcRefArgs) | isConsRef='&' | pureFuncInvokeArgs)
  ;

arrayConstructor
  :
   primNamedOrFuncType ('|' primNamedOrFuncType)*
   		 (LBRACK  arconExprsSubsection RBRACK)+ (nullEnd+=LBRACK RBRACK)*
   		 (LPARA {setNotInArrayDef();} expr_stmt_tuple RPARA {popInArrayDef();} )?
   		 //constructor args...

  ;

primNamedOrFuncType : (pointerQualifier? primitiveType | namedType |  funcType | tupleType) refOrNullable*;

arrayConstructorPrimNoNew:
   primitiveType (LBRACK  arconExprsSubsection RBRACK)+ (nullEnd+=LBRACK RBRACK)*
   	(LPARA {setNotInArrayDef();} expr_stmt_tuple RPARA {popInArrayDef();} )?
	;

arconExprsSubsection:
	expr_stmt (',' expr_stmt  )* (commaEnd+=',')*
	;


newreftypeOnOwn
  : typex=typeEclRef trefOrArrayRef+ pureFuncInvokeArgs
  ;

typeEclRef
  : primitiveType | namedType  | funcType | tupleType
  ;


///////////// expresssions:operators /////////////

equalityOperator
  : '==' | '&==' | '<>' | '&<>'
  ;

relationalOperator
  : '<' | '>' | '>==' | '<=='
  ;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//TOKEN Names//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//

VAL: 'val';
VAR: 'var';

PRIVATE: 'private';
PUBLIC: 'public';
INJECT: 'inject';
PROTECTED: 'protected';
PACKAGE:'package';

/////////////////////////////////////////////////////////lexer/////////////////////////////////////////////////////////

DOT: '.';

DOTDOT: '..';

DDD: '...';


LONGINT
    :   INT ('l'|'L')
    ;

SHORTINT
    :   INT ('s'|'S')
    ;

fragment HexDigits
    : HexDigit ((HexDigit | '_')* HexDigit)?
    ;

fragment HexDigit
    : [0-9a-fA-F]
    ;


fragment
HexIntegerLiteral
	:	'0' [xX] HexDigits
	;

fragment
DIGITS : ( '0' .. '9' )+ ;

INT :  HexIntegerLiteral
    |   (
    	'0' ( 'b' | 'B' ) ( '0' .. '9'  )+
	    |   '0' DIGITS*
	    |   '1'..'9' DIGITS* //TODO: starting with as many 0 as you like is ok
	    )
    ;


fragment DBL_FRAG
  :  '.' DIGITS (Exponent)?
    |   DIGITS ( '.' (DIGITS (Exponent)?)? | Exponent)
    ;

fragment
Exponent
	:	('e' | 'E') ( '+' | '-' )? DIGITS
	;


FLOAT :  (DBL_FRAG | INT) ('f' |'F');

DOUBLE:  DBL_FRAG ('d'|'D')? | INT ('d'|'D');

fragment EscapeSequence
    : '\\' [btnfr{"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit
    ;

STRING_ITMcit
  : '"' (   ~( '\\' | '"'  ) | EscapeSequence )*'"'
  ;
STRING_ITMquot
  : '\'' (   ~( '\\' | '\''  ) | EscapeSequence )*'\''
  ;

fragment EscapeSequenceLE
    : '\\' [{|\\]
	;

LANG_EXT
  :  '||' (   ~( '\\' | '|'  ) | EscapeSequenceLE )* '||'
  ;


REGEX_STRING_ITM
  : 'r"' (   ~( '\\' | '"' )  )*'"'
  | 'r\'' (  ~( '\\' | '\'' )  )*'\''
  ;

MULTILINE_COMMENT
    :   '/*'
        ( (MULTILINE_COMMENT | .) )*?
        '*/' -> channel(HIDDEN)
    ;

LINE_COMMENT
    : '//' ~('\n'|'\r')*  -> channel(HIDDEN)
    ;

IGNORE_NEWLINE  :  '\r'? '\n' {skipNewLine}? -> channel(HIDDEN) ;
NEWLINE  :  '\r'? '\n';

LPARA: '(' { prevskip.add(skipNewLine); skipNewLine=true; } ;
RPARA: ')' { skipNewLine=prevskip.isEmpty()?false:prevskip.pop(); };
LBRACK: '['{ prevskip.add(skipNewLine); skipNewLine=false; } ;
ALBRACK: 'a['{ prevskip.add(skipNewLine); skipNewLine=false; } ;
RBRACK: ']'{ skipNewLine=prevskip.isEmpty()?false:prevskip.pop(); };

LBRACE:'{'{ prevskip.add(skipNewLine); skipNewLine=false; } ;
RBRACE:'}'{ skipNewLine=prevskip.isEmpty()?false:prevskip.pop(); };


WS  :  (' ' | '\t' | '\f' )+ -> channel(HIDDEN)	;

WS2 :
	'\\' (' ' | '\t' | '\f' | LINE_COMMENT|MULTILINE_COMMENT )* ('\r'? '\n')+ -> channel(HIDDEN)
	;//ignore newline if prefixed with \ just like in python

//

// A
ABSTRACT_N: 'abstract';
ACTOR_N: 'actor';
ALSO_N: 'also';
ANNOTATION_N: 'annotation';
ASSERT_N: 'assert';
ASYNC_N: 'async';
AWAIT_N: 'await';
AND_N: 'and';
AS_N: 'as';
// B
BOOL_N: 'bool';
BOOLEAN_N: 'boolean';
BREAK_N: 'break';
BYTE_N: 'byte';
BAND_N: 'band';
BOR_N: 'bor';
BXOR_N: 'bxor';
// C
CASE_N: 'case';
CATCH_N: 'catch';
CHANGED_N: 'changed';
CHAR_N: 'char';
CLASS_N: 'class';
CLOSED_N: 'closed';
CONSTANT_N: 'constant';
CONTINUE_N: 'continue';
COMP_N: 'comp';
// D
DEF_N: 'def';
DEFAULT_N: 'default';
DEL_N: 'del';
DOUBLE_N: 'double';
// E
ELIF_N: 'elif';
ELSE_N: 'else';
ENUM_N: 'enum';
EVERY_N: 'every';
EXTENDS_N: 'extends';
// F
FALSE_N: 'false';
FINALLY_N: 'finally';
FLOAT_N: 'float';
FOR_N: 'for';
FROM_N: 'from';
// G
GLOBAL_N: 'global';
GPUDEF_N: 'gpudef';
GPUKERNEL_N: 'gpukernel';
// I
IF_N: 'if';
IMPORT_N: 'import';
IN_N: 'in';
INIT_N: 'init';
INT_N: 'int';
IS_N: 'is';
ISNOT_N: 'isnot';
// L
LAMBDA_N: 'lambda';
LOCAL_N: 'local';
LONG_N: 'long';
LOOP_N: 'loop';
LAZY_N: 'lazy';
// M
MATCH_N: 'match';
MOD_N: 'mod';
// N
NEW_N: 'new';
NODEFAULT_N: 'nodefault';
NULL_N: 'null';
NOT_N: 'not';
// O
OF_N: 'of';
ONCHANGE_N: 'onchange';
OPEN_N: 'open';
OUT_N: 'out';
OVERRIDE_N: 'override';
OR_N: 'or';
// P
PARFOR_N: 'parfor';
PARFORSYNC_N: 'parforsync';
POST_N: 'post';
PRE_N: 'pre';
PROVIDE_N: 'provide';
PROVIDER_N: 'provider';
// R
RETURN_N: 'return';
// S
SHARED_N: 'shared';
SHORT_N: 'short';
SINGLE_N: 'single';
SIZE_T_N: 'size_t';
SIZEOF_N: 'sizeof';
SUPER_N: 'super';
SYNC_N: 'sync';
// T
THIS_N: 'this';
THROW_N: 'throw';
TO_N: 'to';
TRAIT_N: 'trait';
TRANS_N: 'trans';
TRANSIENT_N: 'transient';
TRUE_N: 'true';
TRY_N: 'try';
TYPEDEF_N: 'typedef';
// U
UNCHECKED_N: 'unchecked';
USING_N: 'using';
// V
VOID_N: 'void';
// W
WHILE_N: 'while';
WITH_N: 'with';

//

KEYWORD: VAL
       | VAR
       | PRIVATE
       | PUBLIC
       | INJECT
       | PROTECTED
       | PACKAGE
       // A
       | ABSTRACT_N
       | ACTOR_N
       | ALSO_N
       | ANNOTATION_N
       | ASSERT_N
       | ASYNC_N
       | AWAIT_N
       | AND_N
       | AS_N
       // B
       | BOOL_N
       | BOOLEAN_N
       | BREAK_N
       | BYTE_N
       | BAND_N
       | BOR_N
       | BXOR_N
       // C
       | CASE_N
       | CATCH_N
       | CHANGED_N
       | CHAR_N
       | CLASS_N
       | CLOSED_N
       | CONSTANT_N
       | CONTINUE_N
       | COMP_N
       // D
       | DEF_N
       | DEFAULT_N
       | DEL_N
       | DOUBLE_N
       // E
       | ELIF_N
       | ELSE_N
       | ENUM_N
       | EVERY_N
       | EXTENDS_N
       // F
       | FALSE_N
       | FINALLY_N
       | FLOAT_N
       | FOR_N
       | FROM_N
       // G
       | GLOBAL_N
       | GPUDEF_N
       | GPUKERNEL_N
       // I
       | IF_N
       | IMPORT_N
       | IN_N
       | INJECT
       | INT_N
       | IS_N
       | ISNOT_N
       // L
       | LAMBDA_N
       | LOCAL_N
       | LONG_N
       | LOOP_N
       | LAZY_N
       // M
       | MATCH_N
       | MOD_N
       // N
       | NEW_N
       | NODEFAULT_N
       | NULL_N
       | NOT_N
       // O
       | OF_N
       | ONCHANGE_N
       | OPEN_N
       | OUT_N
       | OVERRIDE_N
       | OR_N
       // P
       | PACKAGE
       | PARFOR_N
       | PARFORSYNC_N
       | POST_N
       | PRE_N
       | PROVIDE_N
       | PROVIDER_N
       // R
       | RETURN_N
       // S
       | SHARED_N
       | SHORT_N
       | SINGLE_N
       | SIZE_T_N
       | SIZEOF_N
       | SUPER_N
       | SYNC_N
       // T
       | THIS_N
       | THROW_N
       | TO_N
       | TRAIT_N
       | TRANS_N
       | TRANSIENT_N
       | TRUE_N
       | TRY_N
       | TYPEDEF_N
       // U
       | UNCHECKED_N
       | USING_N
       // V
       | VOID_N
       // W
       | WHILE_N
       | WITH_N
       ;

NAME: //{permitDollarPrefixRefName}? => '$'+ NAME_ITMS |
	 NAME_ITMS//a nice list of keywords...!!! <- do this before release!
	 | '\\' KEYWORD
	;

fragment
NAME_ITMS: {permitDollarPrefixRefName}? ('0' .. '9')* ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '$' | '\u0080'..'\ufffe') ( 'a' .. 'z' | '$' | 'A' .. 'Z' | '_' | '0' .. '9' | '\u0080'..'\ufffe' )*
  | ('0' .. '9')* ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '\u0080'..'\ufffe') ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '\u0080'..'\ufffe' )*
  ;

// IntelliJ needs *every* character to be used
// This globs everything that hasn't been matched
OTHER: . -> channel(HIDDEN);
