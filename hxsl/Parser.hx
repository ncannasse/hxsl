/*
 * HxSL - Haxe Shader Language
 *
 * Copyright (c) 2012, The haXe Project Contributors
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */
package hxsl;
import hxsl.Data;
import haxe.macro.Expr;

class Parser {

	var vertex : Function;
	var fragment : Function;
	var helpers : Map<String,Function>;
	var globals : Array<ParsedVar>;
	var cur : ParsedCode;
	var allowReturn : Bool;

	public function new() {
		helpers = new Map();
		globals = [];
	}

	function error(msg:String, p:Position) : Dynamic {
		throw new Error(msg, p);
		return null;
	}

	public function parse( e : Expr ) : ParsedHxsl {
		switch( e.expr ) {
		case EBlock(l):
			for( x in l )
				parseDecl(x);
		default:
			error("Shader code should be a block", e.pos);
		}
		if( vertex == null ) error("Missing vertex function", e.pos);
		if( fragment == null ) error("Missing fragment function", e.pos);
		allowReturn = false;
		var vs = buildShader(vertex);
		var fs = buildShader(fragment);
		var help = new Map();
		allowReturn = true;
		for( h in helpers.keys() )
			help.set(h, buildShader(helpers.get(h)));
		return { vertex : vs, fragment : fs, globals : globals, pos : e.pos, helpers : help };
	}

	public dynamic function includeFile( file : String ) : Null<Expr> {
		return null;
	}

	function getType( t : ComplexType, pos ) {
		switch(t) {
		case TPath(p):
			if( p.params.length == 1 ) {
				switch( p.params[0] ) {
				case TPExpr(e):
					switch( e.expr ) {
					case EConst(c):
						switch( c ) {
						case CInt(i):
							p.params = [];
							var i = Std.parseInt(i);
							if( i > 0 )
								return TArray(getType(t,pos), i);
						default:
						}
					default:
					}
				case TPType(t) if( p.pack.length == 0 && p.name == "Array" && p.sub == null ):
					return TArray(getType(t, pos), 0); // 0 length is for runtime constant
				default:
				}
			}
			if( p.pack.length > 0 || p.sub != null || p.params.length > 0 )
				error("Unsupported type", pos);
			return switch( p.name ) {
			case "Bool": TBool;
			case "Float": TFloat;
			case "Float2": TFloat2;
			case "Float3": TFloat3;
			case "Float4": TFloat4;
			case "Matrix", "M44": TMatrix(4, 4, { t : null } );
			case "M33": TMatrix(3, 3, { t : null } );
			case "M34": TMatrix(3, 4, { t : null } );
			case "M43": TMatrix(4, 3, { t : null } );
			case "Texture": TTexture(false);
			case "CubeTexture": TTexture(true);
			case "Color", "Int": TInt;
			default:
				error("Unknown type '" + p.name + "'", pos);
			}
		case TAnonymous(fields):
			var fl = [];
			for( f in fields ) {
				var t = switch( f.kind ) {
					case FVar(t, e):
						if( e != null ) error("Default value not supported", e.pos);
						getType(t, f.pos);
					default:
						error("Unsupported field declaration", f.pos);
				}
				fl.push( { name : f.name, t : t } );
			}
			return TObject(fl);
		default:
			error("Unsupported type", pos);
		}
		return null;
	}

	function allocVarDecl( v, t, p ) : ParsedVar {
		if( t != null )
			switch( t ) {
			case TPath(path):
				if( path.params.length == 1 && path.name != "Array" ) {
					switch (path.params[0]) {
					case TPType(tt):
						var v = allocVar(v, tt, getKindFromName(path.name, p), p);
						// Texture types can only specify uniform kind
						switch ( v.t ) {
						case TTexture(_):
							if( v.k != VConst )
								error("Invalid kind for texture: " + v.n, p);
							v.k = VTexture;
						default:
						}
						return v;
					default:
					}
				}
			default:
			}
		// leave kind indeterminate until it is used.
		return allocVar(v, t, null, p);
	}

	function allocVar( v, t, k, p ) : ParsedVar {
		return { n : v, k : k, t : t == null ? null : getType(t, p), p : p };
	}

	function getKindFromName( name:String, p ) {
		switch ( name ) {
		case "Input": return VInput;
		case "Var": return VVar;
		case "Const": return VConst;
		case "Param":return VParam;
		default:
			error("Unrecognized kind: " + name, p);
			return null;
		}
	}

	function parseDecl( e : Expr ) {
		switch( e.expr ) {
		case EVars(vl):
			var p = e.pos;
			for( v in vl ) {
				if( v.type == null ) error("Missing type for variable '" + v.name + "'", p);
				var v = allocVarDecl(v.name, v.type, p);
				if( v.n == "input" && v.k == null )
					v.k = VInput;
				globals.push(v);
			}
			return;
		case EFunction(name,f):
			switch( name ) {
			case "vertex": vertex = f;
			case "fragment": fragment = f;
			default:
				if( helpers.exists(name) )
					error("Duplicate function '" + name + "'", e.pos);
				helpers.set(name, f);
			}
			return;
		case ECall(f, pl):
			switch( f.expr ) {
			case EConst(c):
				switch( c ) {
				case CIdent(s):
					if( s == "include" && pl.length == 1 ) {
						switch( pl[0].expr ) {
						case EConst(c):
							switch( c ) {
							case CString(str):
								var f = includeFile(str);
								if( f == null )
									error("Failed to include file", pl[0].pos);
								switch( f.expr ) {
								case EBlock(el):
									for( e in el )
										parseDecl(e);
								default:
									parseDecl(f);
								}
								return;
							default:
							}
						default:
						}
					}
				default:
				}
			default:
			}
		default:
		};
		error("Unsupported declaration", e.pos);
	}

	function buildShader( f : Function ) {
		cur = {
			pos : f.expr.pos,
			args : [],
			exprs : [],
		};
		var pos = f.expr.pos;
		for( p in f.args ) {
			if( p.type == null ) error("Missing parameter type '" + p.name + "'", pos);
			if( p.value != null ) error("Unsupported default value", p.value.pos);
			cur.args.push(allocVar(p.name, p.type, null, pos));
		}
		parseExpr(f.expr);
		return cur;
	}


	function addAssign( e1 : ParsedValue, e2 : ParsedValue, p : Position ) {
		cur.exprs.push( { v : e1, e : e2, p : p } );
	}

	function parseExpr( e : Expr ) {
		switch( e.expr ) {
		case EBlock(el):
			var eold = cur.exprs;
			var old = allowReturn;
			var last = el[el.length - 1];
			cur.exprs = [];
			for( e in el ) {
				allowReturn = old && (e == last);
				parseExpr(e);
			}
			allowReturn = old;
			eold.push({ v : null, e : { v : PBlock(cur.exprs), p : e.pos }, p : e.pos });
			cur.exprs = eold;
		case EIf(cond, eif, eelse):
			var pcond = parseValue(cond);

			var eold = cur.exprs;
			cur.exprs = [];
			parseExpr(eif);
			var pif = { v : PBlock(cur.exprs), p : eif.pos };

			var pelse = null;
			if( eelse != null ) {
				cur.exprs = [];
				parseExpr(eelse);
				pelse = { v : PBlock(cur.exprs), p : eelse.pos };
			}
			cur.exprs = eold;
			cur.exprs.push( {v:null, e: { v:PIf(pcond, pif, pelse), p : e.pos }, p : e.pos} );
		case EBinop(op, e1, e2):
			switch( op ) {
			case OpAssign:
				addAssign(parseValue(e1), parseValue(e2), e.pos);
			case OpAssignOp(op):
				addAssign(parseValue(e1), parseValue( { expr : EBinop(op, e1, e2), pos : e.pos } ), e.pos);
			default:
				error("Operation should have side-effects", e.pos);
			}
		case EVars(vl):
			for( v in vl ) {
				if( v.expr == null && v.type == null )
					error("Missing type for variable '" + v.name + "'", e.pos);
				var l = { v : PLocal(allocVar(v.name, v.type, VTmp, e.pos)), p : e.pos };
				cur.exprs.push( { v : l, e : v.expr == null ? null : parseValue(v.expr), p : e.pos } );
			}
		case ECall(v, params):
			switch( v.expr ) {
			case EConst(c):
				switch(c) {
				case CIdent(s):
					if( s == "kill" && params.length == 1 ) {
						var v = parseValue(params[0]);
						cur.exprs.push( { v : null, e : { v : PUnop(CKill,v), p : e.pos }, p : e.pos } );
						return;
					}
					if( s == "setDepth" && params.length == 1 ) {
						var v = parseValue(params[0]);
						cur.exprs.push( { v : null, e : { v : PUnop(CSetDepth,v), p : e.pos }, p : e.pos } );
						return;
					}
				default:
				}
			default:
			}
			error("Unsupported call", e.pos);
		case EFor(it, expr):
			var iter = null, vname = null;
			switch( it.expr ) {
			case EIn(v,it):
				switch( v.expr ) {
				case EConst(c):
					switch( c ) {
					case CIdent(i) #if !haxe3 , CType(i) #end: vname = i;
					default:
					}
				default:
				}
				iter = parseValue(it);
			default:
			}
			if( vname == null )
				error("For should be in the form for( x in it )", it.pos);

			var old = cur.exprs;
			cur.exprs = [];
			parseExpr(expr);
			var pexpr = { v : PBlock(cur.exprs), p : expr.pos };
			cur.exprs = old;
			cur.exprs.push( {v : null, e:{v:PFor(vname, iter, pexpr), p:e.pos}, p:e.pos } );
		case EReturn(r):
			if( r == null ) error("Return must return a value", e.pos);
			if( !allowReturn ) error("Return only allowed as final expression in helper methods", e.pos);
			var v = parseValue(r);
			cur.exprs.push( { v : null, e : { v : PReturn(v), p : e.pos }, p : e.pos } );
		default:
			error("Unsupported expression", e.pos);
		}
	}

	function parseValue( e : Expr ) : ParsedValue {
		switch( e.expr ) {
		case EField(ef, s):
			return { v : PField(parseValue(ef),s), p : e.pos };
		case EConst(c):
			switch( c ) {
			case CIdent(i) #if !haxe3 , CType(i) #end:
				switch( i ) {
				case "null":
					return { v : PConst(CNull), p : e.pos };
				case "true":
					return { v : PConst(CBool(true)), p : e.pos };
				case "false":
					return { v : PConst(CBool(false)), p : e.pos };
				default:
					return { v : PVar(i), p : e.pos };
				}
			case CInt(v):
				return { v : PConst(CInt(Std.parseInt(v))), p : e.pos };
			case CFloat(f):
				return { v : PConst(CFloat(Std.parseFloat(f))), p : e.pos };
			default:
			}
		case EBinop(op, e1, e2):
			var op = switch( op ) {
			case OpMult: CMul;
			case OpAdd: CAdd;
			case OpDiv: CDiv;
			case OpSub: CSub;
			case OpLt: CLt;
			case OpLte: CLte;
			case OpGt: CGt;
			case OpEq: CEq;
			case OpNotEq: CNeq;
			case OpGte: CGte;
			case OpMod: CMod;
			case OpBoolOr: COr;
			case OpBoolAnd: CAnd;
			case OpInterval: CInterval;
			default: error("Unsupported operation", e.pos);
			};
			return { v : POp(op, parseValue(e1), parseValue(e2)), p : e.pos };
		case EUnop(OpNeg, _, { expr : EConst(CInt(v)) } ):
			return { v : PConst(CInt(-Std.parseInt(v))), p : e.pos };
		case EUnop(op, _, e1):
			var op = switch( op ) {
			case OpNeg: CNeg;
			case OpNot: CNot;
			default: error("Unsupported operation", e.pos);
			}
			return { v : PUnop(op,parseValue(e1)), p : e.pos };
		case ECall(c, params):
			switch( c.expr ) {
			case EField(v, f):
				return makeCall(f, [v].concat(params), e.pos);
			case EConst(c):
				switch( c ) {
				case CIdent(i) #if !haxe3 , CType(i) #end:
					return makeCall(i, params, e.pos);
				default:
				}
			default:
			}
		case EArrayDecl(values):
			var vl = [];
			for( v in values )
				vl.push(parseValue(v));
			return { v : PVector(vl), p : e.pos };
		case EParenthesis(k):
			var v = parseValue(k);
			v.p = e.pos;
			return v;
		case EIf(ec, eif, eelse), ETernary(ec,eif,eelse):
			var vcond = parseValue(ec);
			var vif = parseValue(eif);
			if( eelse == null ) error("'if' needs an 'else'", e.pos);
			var velse = parseValue(eelse);
			return { v : PCond(vcond, vif, velse), p : e.pos };
		case EArray(e1, e2):
			var e1 = parseValue(e1);
			var e2 = parseValue(e2);
			return { v : PRow(e1, e2), p : e.pos };
		default:
		}
		error("Unsupported value expression", e.pos);
		return null;
	}

	function parseInt( e : Expr ) : Null<Int> {
		return switch( e.expr ) {
		case EConst(c): switch( c ) { case CInt(i): Std.parseInt(i); default: null; }
		case EUnop(op, _, e):
			if( op == OpNeg ) {
				var i = parseInt(e);
				if( i == null ) null else -i;
			} else
				null;
		default: null;
		}
	}

	inline function makeUnop( op, e, p ) {
		return { v : PUnop(op, e), p : p };
	}

	inline function makeOp( op, e1, e2, p ) {
		return { v : POp(op, e1, e2), p : p };
	}

	function makeCall( n : String, params : Array<Expr>, p : Position ) {

		if( helpers.exists(n) ) {
			var vl = [];
			for( p in params )
				vl.push(parseValue(p));
			return { v : PCall(n, vl), p : p };
		}

		// texture handling
		if( n == "get" && params.length >= 2 ) {
			var v = parseValue(params.shift());
			var v = switch( v.v ) {
			case PVar(v): v;
			default: error("get should only be used on a single texture variable", v.p);
			};
			var t = parseValue(params.shift());
			var flags = [];
			var idents = ["mm_no","mm_nearest","mm_linear","wrap","clamp","nearest","linear","single","rgba","dxt1","dxt5","ignore_sampler"];
			var values = [TMipMapDisable,TMipMapNearest,TMipMapLinear,TWrap,TClamp,TFilterNearest,TFilterLinear,TSingle,TTypeRgba,TTypeDxt1,TTypeDxt5,TIgnoreSampler];
			var targets = ["mipmap", "wrap", "filter", "lod", "ignore_sampler", "type"];
			var targetValues = [PMipMap, PWrap, PFilter, PLodBias, PIgnoreSampler, PType];
			for( p in params ) {
				switch( p.expr ) {
				case EBinop(OpAssign, { expr : EConst(CIdent(sflag)), pos : fpos }, e2):
					var ip = Lambda.indexOf(targets, sflag);
					if( ip >= 0 ) {
						flags.push({ f : PTParam(targetValues[ip],parseValue(e2)), p : p.pos });
						continue;
					}
					error("Invalid parameter, should be "+targets.join("|"), fpos);
				case EConst(CIdent(sflag)):
					var ip = Lambda.indexOf(idents, sflag);
					if( ip >= 0 ) {
						flags.push({ f : PTFlag(values[ip]), p : p.pos });
						continue;
					}
				case ECall({ expr : EConst(CIdent("lod")) }, [{ expr : EConst(c) }]):
					switch( c ) {
					case CInt(v), CFloat(v):
						flags.push({ f : PTFlag(TLodBias(Std.parseFloat(v))), p : p.pos });
						continue;
					default:
					}
				default:
				}
				error("Invalid parameter, should be "+idents.join("|"), p.pos);
			}
			return { v : PTex(v, t, flags), p : p };
		}
		// build operation
		var v = [];
		for( p in params )
			v.push(parseValue(p));
		var me = this;
		function checkParams(k) {
			if( params.length < k ) me.error(n + " require " + k + " parameters", p);
		}
		switch(n) {
		case "get": checkParams(2); // will cause an error

		case "inv","rcp": checkParams(1); return makeUnop(CRcp, v[0], p);
		case "sqt", "sqrt": checkParams(1); return makeUnop(CSqrt, v[0], p);
		case "rsq", "rsqrt": checkParams(1); return makeUnop(CRsq, v[0], p);
		case "log": checkParams(1); return makeUnop(CLog, v[0], p);
		case "exp": checkParams(1); return makeUnop(CExp, v[0], p);
		case "len", "length": checkParams(1); return makeUnop(CLen, v[0], p);
		case "sin": checkParams(1); return makeUnop(CSin, v[0], p);
		case "cos": checkParams(1); return makeUnop(CCos, v[0], p);
		case "abs": checkParams(1); return makeUnop(CAbs, v[0], p);
		case "neg": checkParams(1); return makeUnop(CNeg, v[0], p);
		case "sat", "saturate": checkParams(1); return makeUnop(CSat, v[0], p);
		case "frc", "frac": checkParams(1); return makeUnop(CFrac, v[0], p);
		case "int": checkParams(1);  return makeUnop(CInt,v[0], p);
		case "nrm", "norm", "normalize": checkParams(1); return makeUnop(CNorm, v[0], p);
		case "trans", "transpose": checkParams(1); return makeUnop(CTrans, v[0], p);

		case "add": checkParams(2); return makeOp(CAdd, v[0], v[1], p);
		case "sub": checkParams(2); return makeOp(CSub, v[0], v[1], p);
		case "mul": checkParams(2); return makeOp(CMul, v[0], v[1], p);
		case "div": checkParams(2); return makeOp(CDiv, v[0], v[1], p);
		case "pow": checkParams(2); return makeOp(CPow, v[0], v[1], p);
		case "min": checkParams(2); return makeOp(CMin, v[0], v[1], p);
		case "max": checkParams(2); return makeOp(CMax, v[0], v[1], p);
		case "mod": checkParams(2); return makeOp(CMod, v[0], v[1], p);
		case "dp","dp3","dp4","dot": checkParams(2); return makeOp(CDot, v[0], v[1], p);
		case "crs", "cross": checkParams(2); return makeOp(CCross, v[0], v[1], p);

		case "lt", "slt": checkParams(2); return makeOp(CLt, v[0], v[1], p);
		case "gte", "sge": checkParams(2); return makeOp(CGte, v[0], v[1], p);
		case "gt", "sgt": checkParams(2); return makeOp(CGt, v[0], v[1], p);
		case "lte", "sle": checkParams(2); return makeOp(CLte, v[0], v[1], p);
		case "eq", "seq": checkParams(2); return makeOp(CEq, v[0], v[1], p);
		case "neq", "sne": checkParams(2); return makeOp(CNeq, v[0], v[1], p);
		case "or": checkParams(2); return makeOp(COr, v[0], v[1], p);
		case "and": checkParams(2); return makeOp(CAnd, v[0], v[1], p);
		case "not": checkParams(1); return makeUnop(CNot, v[0], p);
		default:
		}
		return error("Unknown operation '" + n + "'", p);
	}

}
