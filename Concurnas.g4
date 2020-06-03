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
	| nop=SEMI_COLON SEMI_COLON //nop
 ;

stmts: csOrss (     (SEMI_COLON|NEWLINE+)  csOrss)*   (SEMI_COLON|NEWLINE+)? ;

csOrss: comppound_str_concat|compound_stmt|simple_stmt;//actor MyClass(12) expression reachable version takes priority over compound statement

comppound_str_concat: compound_stmt additiveOp_*;//permits us to do this: a = {} + "str concat"


single_line_block
  :  NEWLINE* FAT_ARROW NEWLINE* single_line_element (SEMI_COLON single_line_element)* SEMI_COLON? //NEWLINE
  ;

single_line_element: comppound_str_concat|compound_stmt|simple_stmt| (nop=SEMI_COLON);

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
	 (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier? valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?    (refname = refName typeNoNTTuple) ( assStyle=assignStyle ( rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple ) | onchangeEveryShorthand )?
	| (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier?  valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?  refname = refName (refCnt+=COLON)* ( assStyle=assignStyle (  rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple  )   | onchangeEveryShorthand)
	| LPARA {setNotInArrayDef();} assignmentTupleDereflhsOrNothing (COMMA assignmentTupleDereflhsOrNothing)+ RPARA {popInArrayDef();} ( assStyle=assignStyle (  rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple  )   | onchangeEveryShorthand)
	| ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier? valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?  assignee=expr_stmt ( assStyle=assignStyle  (rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple )  | onchangeEveryShorthand)
	| lonleyannotation = annotation
	;

assignmentForcedRHS:
	 (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier? valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?    (refname = refName type) ( assStyle=assignStyle ( rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple ) | onchangeEveryShorthand )
	| (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier?  valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?  refname = refName (refCnt+=COLON)* ( assStyle=assignStyle (  rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple  )   | onchangeEveryShorthand)
	| LPARA {setNotInArrayDef();} assignmentTupleDereflhsOrNothing (COMMA assignmentTupleDereflhsOrNothing)+ RPARA {popInArrayDef();} ( assStyle=assignStyle (  rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple  )   | onchangeEveryShorthand)
	| ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? gpuVarQualifier? valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?  assignee=expr_stmt ( assStyle=assignStyle  (rhsAnnotShurtcut = annotation | rhsAssignment = assignmentForcedRHS | rhsExpr = expr_stmt_tuple )  | onchangeEveryShorthand)
	| lonleyannotation = annotation
	;

assignmentTupleDereflhsOrNothing:
	assignmentTupleDereflhs?
;

assignmentTupleDereflhs:
	(annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?    (refname = refName type)
	| (annotations NEWLINE? )? ppp? (override=OVERRIDE_N)? transAndShared=transientAndShared? valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?  refname = refName (refCnt+=COLON)*
	| ppp? transAndShared=transientAndShared? valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?  assignee=expr_stmt
;


//assignmentNamdAndTypeOnly: (annotations NEWLINE? )? ppp? trans='transient'? gpuVarQualifier? valvar=(VAL|VAR)? prefix=(MINUS|PLUS|TILDE)?    (refname = refName type);

onchangeEveryShorthand :
	(BACK_ARROW | isEvery=LESS_THAN_EQUALS) (LPARA {setNotInArrayDef();} onChangeEtcArgs RPARA {popInArrayDef();})? expr_stmt_tuple
	;

assert_stmt
	: ASSERT_N e=expr_stmt_ s=stringNode?
	;

delete_stmt
	: DEL_N expr_stmt (COMMA expr_stmt)* COMMA?
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
	: (IMPORT_N | using=USING_N) prim=dotted_as_name (COMMA sec+=dotted_as_name )* COMMA?
	| (IMPORT_N | using=USING_N) dotted_name DOT star=MULTIPLY
	;

import_stmt_from
  	: FROM_N dotted_name (IMPORT_N | using=USING_N) (import_as_name (COMMA import_as_name )* COMMA? | star=MULTIPLY)
	;

import_as_name
    : NAME (AS_N NAME)?
	;

dotted_as_name
	: dotted_name (AS_N NAME)?
	;

typedef_stmt
  : pppNoInject? TYPEDEF_N NAME typedefArgs? EQUALS  type
  ;

typedefArgs: LESS_THAN NAME  (COMMA NAME )* COMMA? MORE_THAN;


await_stmt
 :  AWAIT_N LPARA {setNotInArrayDef();} onChangeEtcArgs  (  SEMI_COLON  ( expr_stmt | block )?  )? RPARA {popInArrayDef();} //TODO: expand the syntax permitte here
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
    : ppp?  ( (override=OVERRIDE_N)? (DEF_N | gpuitem = GPUDEF_N | gpuitem =GPUKERNEL_N kerneldim = intNode) | (override=OVERRIDE_N) )  ( (extFuncOn (PIPE extFuncOn)* )? funcDefName DOT?)?
    		genericQualiList?
    		LPARA {setNotInArrayDef();} funcParams? RPARA {popInArrayDef();} retTypeIncVoid? blockOrBlock?
	;

funcDefName
	: NAME //now for operator overloading...
	| EQUALS
	| PLUS
	| MINUS
	| MULTIPLY
	| DIVIDE
	| DUEL_MULTIPLY
	| DUEL_PLUS
	| DUEL_MINUS
	| MOD_N | OR_N | AND_N | NOT_N
	| DUEL_LESS_THAN | DUEL_MORE_THAN | TRIPLE_MORE_THAN
	| COMP_N | BAND_N | BOR_N | BXOR_N
	| MINUS_EQUALS
	| MULTIPLY_EQUALS
	| DIVIDE_EQUALS
	| MOD_N EQUALS
	| DUEL_MULTIPLY EQUALS
	| PLUS_EQUALS | OR_N_EQUALS | AND_N_EQUALS | DUEL_LESS_THAN_EQUALS
	| DUEL_MORE_THAN_EQUALS | TRIPLE_MORE_THAN_EQUALS | BAND_N_EQUALS | BOR_N_EQUALS | BXOR_N_EQUALS
	;


extFuncOn
	: extFunOn=type
	//: extFunOn=namedType_ExActor|extFunOnPrim=primitiveType
	;

funcParams
  : funcParam (COMMA funcParam)* (COMMA)?
  ;

funcParam:
	annotations? sharedOrLazy? (gpuVarQualifier gpuInOutFuncParamModifier?)? (isFinal=VAL|VAR)?
	(
		NAME ( type isvararg=TRIPLE_DOT?)? (EQUALS expr_stmt )
		|
		NAME? ( type isvararg=TRIPLE_DOT?) (EQUALS expr_stmt )?
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
	: objectProviderArg (COMMA objectProviderArg )* (COMMA)?
	;

objectProviderArg:
	annotations? pppNoInject? transAndShared=transientAndShared?
		(isFinal=VAL|VAR)? NAME  ((type | (refCnt+=COLON)+) isvararg=TRIPLE_DOT? )? ( EQUALS  expr_stmt )?
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
	(pppNoInject? (single=SINGLE_N | shared=SHARED_N)? PROVIDE_N genericQualiList? lazy=LAZY_N? fieldName=stringNode? provName=NAME? provide=type (FAT_ARROW provideExpr=expr_stmt  | objectProviderNestedDeps )?
	| opdl=objectProviderLineDep) (SEMI_COLON|NEWLINE+)
	;

objectProviderNestedDeps : LBRACE ((SEMI_COLON|NEWLINE+)? nestedDep+=objectProviderLineDep ((SEMI_COLON|NEWLINE+) nestedDep+=objectProviderLineDep)* )? (SEMI_COLON|NEWLINE+)? RBRACE;

objectProviderLineDep: (single=SINGLE_N | shared=SHARED_N)? lazy=LAZY_N? fieldName=stringNode? nameFrom=type (FAT_ARROW exprTo=expr_stmt | LESS_THAN_EQUALS typeOnlyRHS=type objectProviderNestedDeps?)?;


classdef
	: p=ppp? aoc=(ABSTRACT_N | OPEN_N | CLOSED_N)? (trans=TRANSIENT_N | shared=SHARED_N)? (CLASS_N |istrait=TRAIT_N | isactor=ACTOR_N) className=NAME
		genericQualiList?
      (LPARA {setNotInArrayDef();} classdefArgs? RPARA {popInArrayDef();})?
		(
			istypedActor=OF_N ( typedActorOn=namedType_ExActor typeActeeExprList=expr_stmtList?  )
		)?
		(
			NEWLINE* (EXTENDS_N | LESS_THAN) superCls=dotted_name (LESS_THAN superGenType+=type (COMMA superGenType+=type )* COMMA? MORE_THAN)?
		 	 extExpressions=expr_stmtList?
		)?
		(
			NEWLINE* (WITH_N | TILDE) implInstance ((COMMA implInstance)*) COMMA?
		)?
		(block | NEWLINE+ | EOF)
	;


implInstance:
	impli=dotted_name (LESS_THAN implType+=type (COMMA implType+=type )* COMMA? MORE_THAN)?
	;

localclassdef
	: (trans=TRANSIENT_N | shared=SHARED_N)? (CLASS_N | isactor=ACTOR_N)
		genericQualiList?
		(LPARA {setNotInArrayDef();}  classdefArgs? RPARA {popInArrayDef();})?
		(
			istypedActor=OF_N ( typedActorOn=namedType_ExActor typeActeeExprList=expr_stmtList?  )
		)?
		(
			(EXTENDS_N | LESS_THAN) superCls=dotted_name (LESS_THAN type (COMMA type )* COMMA? MORE_THAN)?
		 	 extExpressions=expr_stmtList?
		)?
		(
			(WITH_N | TILDE) implInstance ((COMMA implInstance)* | COMMA?)
		)?
		block
	;

anonclassdef
	: (NEW_N isactor=ACTOR_N?  )
		(
			superCls=dotted_name (LESS_THAN type (COMMA type )* COMMA? MORE_THAN)?
		)

		(
			(WITH_N | TILDE) implInstance ((COMMA implInstance)* | COMMA?)
		)?
		block?
	;


expr_stmtList : (LPARA {setNotInArrayDef();} (expr_stmt ( COMMA expr_stmt )*)? RPARA {popInArrayDef();});


classdefArgs
	: classdefArg (COMMA classdefArg )* (COMMA)?
	;

classdefArg:
	annotations? pppNoInject? (override=OVERRIDE_N)? transAndShared=transientAndShared? (isFinal=VAL|VAR)? (prefix=(MINUS|PLUS|TILDE))? NAME  ((type | (refCnt+=COLON)+) isvararg=TRIPLE_DOT? )? ( EQUALS  expr_stmt )?
	;


annotationDef
  : pppNoInject? ANNOTATION_N NAME
      ( LPARA {setNotInArrayDef();} annotationArg  (COMMA annotationArg )* COMMA? RPARA {popInArrayDef();} )?
      block?
  ;

annotationArg:
	annotations? NAME type? (EQUALS expr_stmt)?
	;


enumdef
  : pppNoInject? ENUM_N NAME (LPARA {setNotInArrayDef();} classdefArgs? RPARA {popInArrayDef();})? enumblock
  ;

enumItem
  : annotations? NAME pureFuncInvokeArgs? block?//dont need to have args?
  ;

enumblock
	: LBRACE NEWLINE* enumItem ( NEWLINE* COMMA NEWLINE* enumItem )* (SEMI_COLON|NEWLINE*) line* NEWLINE* RBRACE
	;

///////////// compound stmt:compound_stmt_atomic_base /////////////

fors
  : for_stmt
  | for_stmt_old
  ;

for_stmt
	: forblockvariant LPARA {setNotInArrayDef();} ( (localVarName=NAME localVarType=type?) | ( LPARA {setNotInArrayDef();} forVarTupleOrNothing (COMMA forVarTupleOrNothing)+ RPARA {popInArrayDef();})) IN_N expr=expr_stmt_tuple
	    (SEMI_COLON  (idxName=NAME idxType=type? ((ESCAPE_EQUALS | EQUALS) idxExpr=expr_stmt)?) )? RPARA {popInArrayDef();} mainblock=block
	    (NEWLINE* ELSE_N elseblock=block )?
	;

forVarTupleOrNothing: forVarTuple?;

forVarTuple: localVarName=NAME localVarType=type?;

for_stmt_old
  : forblockvariant LPARA {setNotInArrayDef();} ( (NAME type? assignStyle assigFrom=expr_stmt_tuple) | assignExpr=expr_stmt_tuple )?
					  	SEMI_COLON check=expr_stmt?
					  	SEMI_COLON postExpr=expr_stmt?
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

match_case_stmt_typedCase: (type ( OR_N type  )* ( SEMI_COLON (case_expr_chain_Tuple | case_expr_chain_or) )? ); //TypedCaseExpression

match_case_stmt_assign: ( ( var=VAR | isfinal=VAL)? NAME (type (OR_N type)*)? ( SEMI_COLON  expr_stmt)? );//CaseExpressionAssign

match_case_assign_typedObjectAssign: (var=VAR | isfinal=VAL)? NAME bitwise_or;

match_case_stmt_assignTuple:  ( LPARA {setNotInArrayDef();} matchTupleAsignOrNone (COMMA matchTupleAsignOrNone)+ RPARA {popInArrayDef();}) ( SEMI_COLON  expr_stmt)? ;//CaseExpressionAssign

matchTupleAsignOrNone: matchTupleAsign?;

matchTupleAsign: NAME type;

case_expr_chain_Tuple:
	LPARA {setNotInArrayDef();} case_expr_chain_orOrNone (COMMA case_expr_chain_orOrNone )+  RPARA {popInArrayDef();}
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


case_operator : EQUALS
              | LESS_THAN
              | LESS_THAN_MORE_THAN
              | AND_DUEL_EQUALS
              | AND_LESS_THAN_MORE_THAN
              | MORE_THAN
              | MORE_THAN_DUEL_EQUALS
              | LESS_THAN_DUEL_EQUALS
              ;

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
						(SEMI_COLON  (idxName=NAME idxType=type? ((ESCAPE_EQUALS | EQUALS) idxExpr=expr_stmt)?) )?// | nameAlone=NAME
	RPARA {popInArrayDef();} mainBlock=block
	  ( NEWLINE* ELSE_N elseblock=block )?
	;

loop_stmt
  : LOOP_N
  (LPARA {setNotInArrayDef();}  (idxName=NAME idxType=type? ((ESCAPE_EQUALS | EQUALS) idxExpr=expr_stmt)?)  RPARA {popInArrayDef();} )? mainBlock=block
  ;

try_stmt
	:	TRY_N  (LPARA {setNotInArrayDef();}  simple_stmt  (SEMI_COLON? simple_stmt  )* SEMI_COLON?  RPARA {popInArrayDef();})? mainblock=block
	  catchBlock*
	  ( NEWLINE* FINALLY_N  finblock=block )?
	;

catchBlock: NEWLINE* CATCH_N  LPARA {setNotInArrayDef();}   NAME  (type   (OR_N  type   )* )? RPARA {popInArrayDef();}  block ;

block_async
  : block_ ( async=EXCLAMATION( LPARA {setNotInArrayDef();} executor=expr_stmt RPARA {popInArrayDef();} )? )?
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
 :  ONCHANGE_N (LPARA {setNotInArrayDef();} onChangeEtcArgs (SEMI_COLON opts+=NAME (COMMA opts+=NAME )* (COMMA)? )? RPARA {popInArrayDef();})? (block )//| single_line_block
 ;


every
 :  EVERY_N (LPARA {setNotInArrayDef();} onChangeEtcArgs (SEMI_COLON opts+=NAME (COMMA opts+=NAME )* (COMMA)? )? RPARA {popInArrayDef();})? (block )//| single_line_block
;

///////////// annotations /////////////

annotations
  : annotation (NEWLINE* COMMA? NEWLINE* annotation)*
  ;

annotation
  :AMP (LBRACK loc+=(THIS_N | NAME) (COMMA loc+=(THIS_N | NAME) )* (COMMA)? RBRACK)?
     dotted_name
    ( LPARA {setNotInArrayDef();} (namedAnnotationArgList |  expr_stmt  )?  RPARA {popInArrayDef();} )?
  ;

namedAnnotationArgList
  : n2expr (COMMA n2expr )*  COMMA?
  ;

n2expr : NAME EQUALS expr_stmt;


///////////// common /////////////

genericQualiList: LESS_THAN nameAndUpperBound  (COMMA nameAndUpperBound )* COMMA? MORE_THAN;

nameAndUpperBound: NAME namedType? nullable=QUESTION?;

dottedNameList
	: dotted_name (COMMA dotted_name  )* COMMA?
	;

inoutGenericModifier : IN_N | OUT_N;

onChangeEtcArgs
 : onChangeEtcArg (COMMA onChangeEtcArg)*
 ;

onChangeEtcArg
	: valvar=(VAL|VAR)? NAME (type | (refCnt+=COLON)+)? EQUALS expr_stmt
	| expr_stmt
	;

dotted_name
	: NAME ( DOT  NAME )*
	;

assignStyle : ESCAPE EQUALS
            | EQUALS
            | PLUS_EQUALS
            | MINUS_EQUALS
            | MULTIPLY_EQUALS
            | DIVIDE_EQUALS
            | MOD_N_EQUALS
            | DUEL_MULTIPLY_EQUALS
            | OR_N_EQUALS
            | AND_N_EQUALS
            | DUEL_LESS_THAN_EQUALS
            | DUEL_MORE_THAN_EQUALS
            | TRIPLE_MORE_THAN_EQUALS
            | BAND_N EQUALS
            | BOR_N EQUALS
            | BXOR_N EQUALS;

block
  : NEWLINE* block_
  ;

block_
  : LBRACE  line* RBRACE
  ;

pureFuncInvokeArgs
	: LPARA {setNotInArrayDef();} (pureFuncInvokeArg (COMMA pureFuncInvokeArg)* COMMA? )?  RPARA {popInArrayDef();}
	;

pureFuncInvokeArg
	: (NAME  EQUALS )? ( expr_stmt  |  primitiveType |  funcType | tupleType)
	;


funcRefArgs
  : LPARA {setNotInArrayDef();} (  funcRefArg (  COMMA funcRefArg )* COMMA? )? RPARA {popInArrayDef();}
  ;

funcRefArg
	: (NAME EQUALS)? ( QUESTION lazy=LAZY_N? type | lazy=LAZY_N? primitiveType | lazy=LAZY_N? funcType nullable=QUESTION? | lazy=LAZY_N? LPARA {setNotInArrayDef();} tupleType RPARA {popInArrayDef();} nullable=QUESTION? | lazy=LAZY_N? namedType nullable=QUESTION | expr_stmt | (refcnt+=COLON)+ )
	;

genTypeList
  :
	LESS_THAN genTypeListElemnt ( COMMA genTypeListElemnt )* COMMA? MORE_THAN
  ;

genTypeListElemnt
	: QUESTION   | ( inoutGenericModifier?  type )
	;


///////////// types /////////////
trefOrArrayRef:
		hasAr=LBRACK (arLevels=intNode) RBRACK
	| (hasArAlt+=LBRACK RBRACK)+
	| refOrNullable
	;

refOrNullable: COLON dotted_name?
	| nullable=QUESTION
	| nullableErr=DUEL_QUESTION
	;


type:
	 bareTypeParamTuple (PIPE bareTypeParamTuple)* trefOrArrayRef*
	 ;


bareTypeParamTuple:
	pointerQualifier? primitiveType
	| namedType
	| funcType
	| LPARA {setNotInArrayDef();} tupleType RPARA {popInArrayDef();}
	;


typeNoNTTuple:
	 bareTypeParamTupleNoNT (PIPE bareTypeParamTupleNoNT)* trefOrArrayRef*
	 ;

bareTypeParamTupleNoNT:
	pointerQualifier? primitiveType
	| namedType
	| funcType
	| LPARA {setNotInArrayDef();} tupleTypeNoNT RPARA {popInArrayDef();}
	;

tupleTypeNoNT : bareButTupleNoNT (COMMA bareButTupleNoNT )+ ;

bareButTupleNoNT: primitiveType | funcType;


ppp: inject=INJECT?  pp=(PRIVATE | PROTECTED | PUBLIC | PACKAGE)
	| inject=INJECT pp=(PRIVATE | PROTECTED | PUBLIC | PACKAGE)?;

pppNoInject: PRIVATE | PROTECTED | PUBLIC | PACKAGE;

pointerQualifier : (cnt+=MULTIPLY|cnt2+=DUEL_MULTIPLY)+;

//typeNoPrim : namedType | funcType | tupleType;

namedType
  : isactor=ACTOR_N namedType_ExActor?
  | namedType_ExActor
  ;


namedType_ExActor
  :  primaryName=dotted_name priamryGens=genTypeList? (DOT nameAndgens)* ( OF_N of=namedType)? //'of' namedType ?
  ;

nameAndgens : NAME genTypeList?;

tupleType : bareButTuple (COMMA bareButTuple )+ ;

bareButTuple: (primitiveType | namedType | funcType ) trefOrArrayRef*;

funcType :
	funcType_
	| LPARA {setNotInArrayDef();} funcType_ RPARA {popInArrayDef();}
	;

funcType_
	: genericQualiList?
	    LPARA {setNotInArrayDef();} ( (type (COMMA type)*)? COMMA?  | constr=MULTIPLY ) RPARA {popInArrayDef();} retTypeIncVoid
	;

retTypeIncVoid
  :  type
  |  VOID_N
  ;

primitiveType: (BOOLEAN_N | BOOL_N) | SIZE_T_N | INT_N | LONG_N | FLOAT_N | DOUBLE_N | BYTE_N | SHORT_N | CHAR_N | LAMBDA_N;

///////////// expresssions /////////////

expr_stmt_tuple : expr_stmt ( (COMMA expr_stmt)+ COMMA? )?;

expr_stmt
	: for_list_comprehension
	;

for_list_comprehension
	: mainExpr=expr_list ( flc_forStmt_+  (IF_N condexpr=expr_stmt)?)?
	;

flc_forStmt_:
	forblockvariant (localVarName=NAME localVarType=type?  | ( LPARA {setNotInArrayDef();} forVarTupleOrNothing (COMMA forVarTupleOrNothing)+ RPARA {popInArrayDef();}))   IN_N expr=expr_list
	;



expr_list
	: /*block_async |*/  lambdadefOneLine | lambdadef | anonLambdadef | expr_stmt_+ ;//shortcut in the lambdadef as it ends with a newline so cannot be an expr stmt


lambdadefOneLine : annotations? DEF_N genericQualiList?  LPARA {setNotInArrayDef();}  funcParams? RPARA {popInArrayDef();} retTypeIncVoid? single_line_block ;

lambdadef
    : annotations? DEF_N genericQualiList?  LPARA {setNotInArrayDef();}  funcParams? RPARA {popInArrayDef();} retTypeIncVoid? (block | (single_line_block NEWLINE+))  ;


anonLambdadef : ((NAME (COMMA NAME)*) | LPARA {setNotInArrayDef();} ( typeAnonParam (COMMA typeAnonParam)*) RPARA {popInArrayDef();}) retType=type? single_line_block;

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
shiftExprOp_: (lshift=LESS_THAN LESS_THAN | rshift=MORE_THAN MORE_THAN | rshiftu=MORE_THAN MORE_THAN MORE_THAN)  additiveExpr;


additiveExpr: divisiveExpr ( additiveOp_)*;
additiveOp_ : ({notArrayDef()}? op=MINUS divisiveExpr ) | op=PLUS  divisiveExpr;

divisiveExpr: powExpr ( divisiveExprOP_)*;
divisiveExprOP_:op=(MULTIPLY|DIVIDE| MOD_N)  powExpr;

powExpr :  lhs=notExpr ( DUEL_MULTIPLY  rhs+=notExpr)*;

notExpr: isnot=NOT_N? containsExpr;

containsExpr : lhs=prefixExpr ( (invert=NOT_N? IN_N)  rhs=prefixExpr)?;

prefixExpr : prefixOp=(DUEL_PLUS | DUEL_MINUS | MINUS | PLUS | COMP_N)?	 postfixExpr;

postfixExpr : sizeOfExpr postfixOp=(DUEL_PLUS | DUEL_MINUS)?;

sizeOfExpr: (sizeof=SIZEOF_N  (LESS_THAN variant=dotted_name MORE_THAN)? )? asyncSpawnExpr;

asyncSpawnExpr: notNullAssertion (isAsync=EXCLAMATION (LPARA {setNotInArrayDef();} expr_stmt RPARA {popInArrayDef();})? )?;


notNullAssertion :  elvisOperator ( nna=DUEL_QUESTION)?;
elvisOperator :  lhsExpr=vectorize ( QUESTION_COLON  elsExpr=if_expr)?  ;


vectorize: primary=vectorize vectorize_element+
	| passthrough=dotOperatorExpr
	;

vectorize_element:
	nullsafe=QUESTION? (CARET (doubledot=CARET)? ) (constru=constructorInvoke | arrayRefElements+ | afterVecExpr=refName genTypeList? (pureFuncInvokeArgs | AND funcRefArgs?)? )?
	;

dotOperatorExpr: ((pntUnrefCnt+=MULTIPLY|pntUnrefCnt2+=DUEL_MULTIPLY )+ | address=TILDE)? copyExpr ( NEWLINE* dotOpArg NEWLINE* copyExpr)*;

copyExpr : expr_stmt_BelowDot (isCopy=AMP (hasCopier=LPARA {setNotInArrayDef();} ( (copyExprItem  (COMMA copyExprItem)* COMMA?)? (SEMI_COLON modifier+=NAME (COMMA modifier+=NAME)* COMMA?  )? ) RPARA {popInArrayDef();} )? )?;

copyExprItem: ename=NAME EQUALS expr_stmt
	|  incName=NAME
	| LESS_THAN exclName+=NAME (COMMA exclName+=NAME)* COMMA? MORE_THAN
	| (copyName=NAME | superCopy=SUPER_N )AMP ( hasCopier=LPARA {setNotInArrayDef();} ( (copyExprItem  (COMMA copyExprItem)* COMMA?)?  (SEMI_COLON modifier+=NAME (COMMA modifier+=NAME)* COMMA? )? ) RPARA {popInArrayDef();} )?
	;


expr_stmt_BelowDot //seperate rule for match operations - basically atoms
	: (isthis=THIS_N pureFuncInvokeArgs | SUPER_N pureFuncInvokeArgs) #superOrThisConstructorInvoke
	| NAME genTypeList? pureFuncInvokeArgs #FuncInvokeExprName
	| expr_stmt_BelowDot genTypeList? pureFuncInvokeArgs #FuncInvokeExpr

	| NAME genTypeList #RefQualifiedGeneric //{ $ret = new RefQualifiedGenericNamedType(getLine(input), getColumn(input), $namedT.text, $gg.genTypes);   }//ret namedType
    | LPARA {setNotInArrayDef();} ACTOR_N dotted_name genTypeList? RPARA {popInArrayDef();} #RefQualifiedGenericActor //{ $ret = gg==null? new RefQualifiedGenericNamedType(getLine(input), getColumn(input), $namedTa.ret, true) : new RefQualifiedGenericNamedType(getLine(input), getColumn(input), $namedTa.ret, $gg.genTypes, true);   }//ret namedType

	| expr_stmt_BelowDot genTypeList? AND funcRefArgs? #FuncRefExpr
	| expr_stmt_BelowDot (refCnt+=COLON)* arrayRefElements+ (extraEmptyBracks+=LBRACK RBRACK)* #ArrayRefExpr
	| main=expr_stmt_BelowDot (refCnt+=COLON)+ post=expr_stmt_BelowDot? #RefExpr
	| notNullAssertion2 #AtomPassThrough
	;


arrayRefElements:  (nullSafe=QUESTION? LBRACK arrayRefElement (COMMA arrayRefElement)* (trailcomma+=COMMA)* RBRACK )  ;

notNullAssertion2 :  atom (NEWLINE* nna=DUEL_QUESTION)?;

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

classNode: type DOT CLASS_N;
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
	: (lhs=expr_stmt TRIPLE_DOT rhs=expr_stmt)
	| (post=expr_stmt TRIPLE_DOT )
	| ( TRIPLE_DOT pre=expr_stmt)
	| ( simple=expr_stmt )
	;

refName
	: NAME
	;


dotOpArg
	: DOT PIPE ESCAPE_DOT PIPE DUEL_DOT PIPE QUESTION_DOT
	;

arrayDef
	: LBRACK RBRACK
	| (isArray=ALBRACK | LBRACK) NEWLINE* ((expr_stmt  ( ( NEWLINE* COMMA NEWLINE* expr_stmt )+  COMMA? | NEWLINE*COMMA)  ) | COMMA) NEWLINE* RBRACK //list def or single element array
	| arrayDefComplex
	;

arrayDefComplex: ALBRACK expr_stmt_+ arrayDefComplexNPLus1Row* (NEWLINE* SEMI_COLON NEWLINE*)? RBRACK
	|  {setInArrayDef();} LBRACK expr_stmt_+ arrayDefComplexNPLus1Row* (NEWLINE* SEMI_COLON NEWLINE*)? RBRACK {popInArrayDef();}
	;


arrayDefComplexNPLus1Row
	: (SEMI_COLON NEWLINE* | NEWLINE+) expr_stmt_+
	;

mapDef
	: LBRACE NEWLINE*  mapDefElement  (NEWLINE* COMMA NEWLINE* mapDefElement )* (NEWLINE* COMMA)?  NEWLINE* RBRACE
	;

mapDefElement: (isDefault=DEFAULT_N | key=expr_stmt) NEWLINE*  FORWARD_ARROW NEWLINE*  value=expr_stmt;


///////////// expresssions:constructors/////////////

constructorInvoke
	: namedActorConstructor
	| ( NEW_N  ( namedConstructor | arrayConstructor |  primNamedOrFuncType refOrNullable+  ) ) //
	| arrayConstructorPrimNoNew
	| newreftypeOnOwn
	;

namedConstructor
	: type  ( ( isConsRef=AND funcRefArgs) | isConsRef=AND | pureFuncInvokeArgs)
	;

namedActorConstructor
  : isNewDefiend=NEW_N? ACTOR_N namedType_ExActor ( (isConsRef=AND funcRefArgs) | isConsRef=AND | pureFuncInvokeArgs)
  ;

arrayConstructor
  :
   primNamedOrFuncType (PIPE primNamedOrFuncType)*
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
	expr_stmt (COMMA expr_stmt  )* (commaEnd+=COMMA)*
	;


newreftypeOnOwn
  : typex=typeEclRef trefOrArrayRef+ pureFuncInvokeArgs
  ;

typeEclRef
  : primitiveType | namedType  | funcType | tupleType
  ;


///////////// expresssions:operators /////////////

equalityOperator
  : DUEL_EQUALS | AND_DUEL_EQUALS | LESS_THAN_MORE_THAN | AND_LESS_THAN_MORE_THAN
  ;

relationalOperator
  : LESS_THAN | MORE_THAN | MORE_THAN_DUEL_EQUALS | LESS_THAN_DUEL_EQUALS
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

LONGINT
    :   INT ('lPIPEL')
    ;

SHORTINT
    :   INT ('sPIPES')
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
  :  DOT DIGITS (Exponent)?
    |   DIGITS ( DOT (DIGITS (Exponent)?)? | Exponent)
    ;

fragment
Exponent
	:	(EXPONENT_S) ( PLUS | MINUS )? DIGITS
	;


FLOAT :  (DBL_FRAG | INT) FLOAT_S;

DOUBLE:  DBL_FRAG DOUBLE_S? | INT DOUBLE_S;

fragment EscapeSequence
    : ESCAPE [btnfr{"'\\]
    | ESCAPE ([0-3]? [0-7])? [0-7]
    | ESCAPE 'u'+ HexDigit HexDigit HexDigit HexDigit
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
  :  '||' (   ~( '\\' | '|' /* Tokens aren't supported in sets, apparently */  ) | EscapeSequenceLE )* '||'
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
    : '//' ~('\n' | '\r')*  -> channel(HIDDEN)
    ;

IGNORE_NEWLINE  :  '\r'? '\n' {skipNewLine}? -> channel(HIDDEN) ;
NEWLINE  :  '\r'? '\n';

LPARA: LEFT_PARENTHESES { prevskip.add(skipNewLine); skipNewLine=true; } ;
RPARA: RIGHT_PARENTHESES { skipNewLine=prevskip.isEmpty()?false:prevskip.pop(); };
LBRACK: LEFT_BRACKET { prevskip.add(skipNewLine); skipNewLine=false; } ;
ALBRACK: LEFT_BRACKET_ALT { prevskip.add(skipNewLine); skipNewLine=false; } ;
RBRACK: RIGHT_BRACKET { skipNewLine=prevskip.isEmpty()?false:prevskip.pop(); };

LBRACE: LEFT_BRACE { prevskip.add(skipNewLine); skipNewLine=false; } ;
RBRACE: RIGHT_BRACE { skipNewLine=prevskip.isEmpty()?false:prevskip.pop(); };

WS  :  (' ' | '\t' | '\f' )+ -> channel(HIDDEN)	;

WS2 :
	ESCAPE (' ' | '\t' | '\f' | LINE_COMMENT|MULTILINE_COMMENT )* ('\r'? '\n')+ -> channel(HIDDEN)
	;//ignore newline if prefixed with \ just like in python

// Keywords

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

// Symbol Combinations

FORWARD_ARROW: MORE_THAN MINUS;
BACK_ARROW: LESS_THAN MINUS;
FAT_ARROW: EQUALS MORE_THAN;

LESS_THAN_DUEL_EQUALS: LESS_THAN DUEL_EQUALS;
MORE_THAN_DUEL_EQUALS: MORE_THAN DUEL_EQUALS;

ESCAPE_EQUALS: ESCAPE EQUALS;
ESCAPE_DOT: ESCAPE DOT;

DUEL_QUESTION: QUESTION QUESTION;

DUEL_PLUS: PLUS PLUS;
DUEL_MINUS: MINUS MINUS;
DUEL_MULTIPLY: MULTIPLY MULTIPLY;

DUEL_EQUALS: EQUALS EQUALS;

AND_DUEL_EQUALS: AND DUEL_EQUALS;
AND_LESS_THAN_MORE_THAN: AND LESS_THAN_MORE_THAN;

DUEL_LESS_THAN: LESS_THAN LESS_THAN;
DUEL_MORE_THAN: MORE_THAN MORE_THAN;

TRIPLE_MORE_THAN: MORE_THAN MORE_THAN MORE_THAN;

TRIPLE_DOT: DOT DOT DOT;
DUEL_DOT: DOT DOT;

PLUS_EQUALS: PLUS EQUALS;
MINUS_EQUALS: MINUS EQUALS;
MULTIPLY_EQUALS: MULTIPLY EQUALS;
DIVIDE_EQUALS: DIVIDE EQUALS;

LESS_THAN_EQUALS: LESS_THAN EQUALS;
LESS_THAN_MORE_THAN: LESS_THAN MORE_THAN;

MOD_N_EQUALS: MOD_N EQUALS;
DUEL_MULTIPLY_EQUALS: DUEL_MULTIPLY EQUALS;
OR_N_EQUALS: OR_N EQUALS;
AND_N_EQUALS: AND_N EQUALS;
BAND_N_EQUALS: BAND_N EQUALS;
BOR_N_EQUALS: BOR_N EQUALS;
BXOR_N_EQUALS: BXOR_N EQUALS;

DUEL_LESS_THAN_EQUALS: DUEL_LESS_THAN EQUALS;
DUEL_MORE_THAN_EQUALS: DUEL_MORE_THAN EQUALS;
TRIPLE_MORE_THAN_EQUALS: TRIPLE_MORE_THAN EQUALS;

QUESTION_COLON: QUESTION COLON;
QUESTION_DOT: QUESTION DOT;

// Symbols

DOT: '.';
COMMA: ',';
SEMI_COLON: ';';
COLON: ':';
EXCLAMATION: '!';
QUESTION: '?';
ESCAPE: '\\';
CARET: '^';

LEFT_PARENTHESES: '(';
RIGHT_PARENTHESES: ')';

LEFT_BRACKET_ALT: 'a' LEFT_BRACKET;
LEFT_BRACKET: '[';
RIGHT_BRACKET: ']';

LEFT_BRACE: '{';
RIGHT_BRACE: '}';

AND: '&';
AMP: '@';

TILDE: '~';
PIPE: '|';

MINUS: '-';
PLUS: '+';
MULTIPLY: '*';
DIVIDE: '/';

EQUALS: '=';
LESS_THAN: '<';
MORE_THAN: '>';

EXPONENT_S: 'e' | 'E';
DOUBLE_S: 'd' | 'D';
FLOAT_S: 'f' | 'F';

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

//

NAME: //{permitDollarPrefixRefName}? => '$'+ NAME_ITMS |
	 NAME_ITMS//a nice list of keywords...!!! <- do this before release!
	 | ESCAPE KEYWORD
	;

fragment
NAME_ITMS: {permitDollarPrefixRefName}? ('0' .. '9')* ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '$' | '\u0080'..'\ufffe') ( 'a' .. 'z' | '$' | 'A' .. 'Z' | '_' | '0' .. '9' | '\u0080'..'\ufffe' )*
  | ('0' .. '9')* ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '\u0080'..'\ufffe') ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '\u0080'..'\ufffe' )*
  ;

// IntelliJ needs *every* character to be used
// This globs everything that hasn't been matched
OTHER: . -> channel(HIDDEN);
