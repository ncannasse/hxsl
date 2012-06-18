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

class Compiler {

	var cur : Code;
	var allVars : Array<Variable>;
	var scopeVars : Hash<Variable>;
	var globalVars : Hash<Variable>;
	var ops : Array<Array<{ p1 : VarType, p2 : VarType, r : VarType }>>;
	var tempCount : Int;
	var helpers : Hash<Data.ParsedCode>;
	var ret : { v : CodeValue };
	var inConstantExpr : Bool;
	var allowTextureRead : Bool;
	var globalsRead : Hash<Variable>;
	var iterators : IntHash<Variable>;
	var paramsInferred : Array<Variable>;

	public var config : { inlTranspose : Bool, inlInt : Bool, allowAllWMasks : Bool, forceReads : Bool };

	public function new() {
		tempCount = 0;
		scopeVars = new Hash();
		paramsInferred = [];
		config = { inlTranspose : true, inlInt : true, allowAllWMasks : false, forceReads : true };
		ops = new Array();
		for( o in initOps() )
			ops[Type.enumIndex(o.op)] = o.types;
	}

	function initOps() {
		var mat4 = TMatrix(4, 4, { t : false } );
		var mat4_t = TMatrix(4, 4, { t : true } );
		var mat3 = TMatrix(3, 3, { t : false } );
		var mat3_t = TMatrix(3, 3, { t : true } );

		var floats = [
			{ p1 : TFloat, p2 : TFloat, r : TFloat },
			{ p1 : TFloat2, p2 : TFloat2, r : TFloat2 },
			{ p1 : TFloat3, p2 : TFloat3, r : TFloat3 },
			{ p1 : TFloat4, p2 : TFloat4, r : TFloat4 },
		];
		var ops = [];
		for( o in Lambda.map(Type.getEnumConstructs(CodeOp), function(c) return Type.createEnum(CodeOp, c)) )
			ops.push({ op : o, types : switch( o ) {
				case CAdd, CSub, CDiv, CPow, CMod: floats;
				case CMin, CMax, CLt, CGte: floats;
				case CEq, CNeq: floats.concat( [{p1 : TBool, p2 : TBool, r : TBool}] );
				case CAnd, COr: [ { p1 : TBool, p2 : TBool, r : TBool } ];
				case CDot: [ { p1 : TFloat4, p2 : TFloat4, r : TFloat }, { p1 : TFloat3, p2 : TFloat3, r : TFloat } ];
				case CCross: [ { p1 : TFloat3, p2 : TFloat3, r : TFloat3 }];
				case CMul: floats.concat([
					{ p1 : TFloat4, p2 : mat4_t, r : TFloat4 },
					{ p1 : TFloat3, p2 : mat3_t, r : TFloat3 },
					{ p1 : TFloat3, p2 : mat4_t, r : TFloat3 }, // only use the 3x4 part of the matrix
					{ p1 : mat4, p2 : mat4_t, r : mat4 },
					{ p1 : mat3, p2 : mat3_t, r : mat3 },
					{ p1 : mat4_t, p2 : mat4, r : mat4_t },
					{ p1 : mat3_t, p2 : mat3, r : mat3_t },
				]);
			}});
		return ops;
	}

	function error(msg:String, p) : Dynamic {
		throw new Error(msg, p);
		return null;
	}

	public dynamic function warn( msg:String, p:haxe.macro.Expr.Position) {
	}

	function typeStr( t : VarType )  {
		switch( t ) {
		case TMatrix(r, c, t):
			return "M" + r + "" + c + (t.t ? "T" : "");
		case TTexture(cube):
			return cube ? "CubeTexture" : "Texture";
		default:
			return Std.string(t).substr(1);
		}

	}

	public function compile( h : ParsedHxsl ) : Data {
		allVars = [];
		iterators = new IntHash();
		globalVars = new Hash();
		
		globalVars.set("out", allocVar("out", VOut, TFloat4, h.pos));

		helpers = h.helpers;
		for ( v in h.vars ) {
			var k = v.k;
			switch ( v.t ) {
			case TTexture(_): k = VTexture;
			default:
			}
			globalVars.set( v.n, allocVar(v.n, k, v.t, v.p) );
		}

		var vertex = compileShader(h.vertex,true);
		var fragment = compileShader(h.fragment,false);
		checkGlobalVars(vertex, fragment);
		
		var vars = [];
		for( v in allVars ) {
			if( v.kind == null ) throw "assert";
			switch( v.kind ) {
			case VTmp:
			default:
				vars.push(v);
			}
		}

		return { vars : vars, vertex : vertex, fragment : fragment };
	}

	function compileShader( c : ParsedCode, vertex : Bool ) : Code {
		cur = {
			vertex : vertex,
			pos : c.pos,
			consts : [],
			args : [],
			exprs : [],
			tex : [],
			tempSize : 0,
		};
		globalsRead = new Hash();
		scopeVars = cloneVars(globalVars);

		for( v in c.args ) {
			switch( v.t ) {
			case TTexture(_):
				if( cur.vertex ) error("You can't use a texture inside a vertex shader", v.p);
				var tex = allocVar(v.n, VTexture, v.t, v.p);
				cur.tex.push(tex);
			default:
				cur.args.push(allocVar(v.n, VParam, v.t, v.p));
			}
		}

		for( e in c.exprs )
			compileAssign(e.v, e.e, e.p);

		checkLocalVars();

		return cur;
	}

	function cloneVars(vars:Hash<Variable>) {
		var old = new Hash();
		for( v in vars.keys() )
			old.set(v, vars.get(v));
		return old;
	}

	function closeBlock( old : Hash<Variable> ) {
		for( v in scopeVars )
			if( v.kind == VTmp && old.get(v.name) != v && !v.read )
				warn("Unused local variable '" + v.name + "'", v.pos);
		scopeVars = old;
	}

	function compileAssign( v : Null<ParsedValue>, e : ParsedValue, p : Position ) {
		if( v == null ) {
			switch( e.v ) {
			case PBlock(el):
				var old = cloneVars(scopeVars);
				for( e in el )
					compileAssign(e.v, e.e, e.p);
				closeBlock(old);
				return;
			case PFor(it, first, last, expr):
				var firstc = compileValueOpt(first);
				var lastc = compileValueOpt(last);

				inConstantExpr = true;
				checkRead(firstc);
				checkRead(lastc);
				inConstantExpr = false;

				var savedVars = cloneVars(scopeVars);
				var savedExprs = cur.exprs;
				cur.exprs = [];

				var itc = allocVar(it.n, it.k, it.t, it.p);
				itc.write = Tools.fullBits(itc.type);
				iterators.set(itc.id, itc);
				compileAssign(expr.v, expr.e, expr.p);
				iterators.remove(itc.id);

				savedExprs.push( { v : null, e : {d:CFor(itc, firstc, lastc, cur.exprs), t:TBool, p:p} } );

				scopeVars = savedVars;
				cur.exprs = savedExprs;
				return;
			case PIf(cond, eif, eelse):
				cond = optimizeValue(cond);

				var ccond = compileConditional(cond);
				unify(ccond.t, TBool, ccond.p);

				// determine if branch result can be determined now
				switch ( cond.v ) {
				case PConst(c):
					var result = parseLiteral(c);
					if ( result != null && result > 0 ) {
						for ( e in eif ) {
							compileAssign(e.v, e.e, e.p);
						}
					} else if ( eelse != null ) {
						for ( e in eelse ) {
							compileAssign(e.v, e.e, e.p);
						}
					}
				default:
					var old = cur.exprs;
					cur.exprs = [];
					for ( e in eif ) {
						compileAssign(e.v, e.e, e.p);
					}
					var ifexpr = cur.exprs;
					var elseexpr = null;
					if ( eelse != null ) {
						cur.exprs = [];
						for ( e in eelse ) {
							compileAssign(e.v, e.e, e.p);
						}
						elseexpr = cur.exprs;
					}
					cur.exprs = old;
					cur.exprs.push( {v : null, e : {d:CIf(ccond, ifexpr, elseexpr), t:TBool, p:p}} );
				}
				return;
			case PReturn(v):
				if( ret == null ) error("Unexpected return", e.p);
				if( ret.v != null ) error("Duplicate return", e.p);
				ret.v = compileValueOpt(v);
				checkRead(ret.v);
				return;
			default:
			}
			var e = compileValueOpt(e);
			switch( e.d ) {
			case CUnop(op, _):
				if( op == CKill ) {
					checkRead(e);
					cur.exprs.push( { v : null, e : e } );
					return;
				}
			default:
			}
			error("assert",p);
		}
		if( e == null ) {
			switch( v.v ) {
			case PLocal(v):
				allocVar(v.n, VTmp, v.t, v.p);
				return;
			default:
			}
			error("assert",p);
		}
		var e = compileValueOpt(e);
		switch( v.v ) {
		case PLocal(v):
			if( v.t == null ) v.t = e.t;
		default:
		}
		var v = compileValueOpt(v,true);
		unify(e.t, v.t, e.p);
		addAssign(v, e, p);
	}

	function addAssign( v : CodeValue, e : CodeValue, p ) {
		checkRead(e);
		switch( v.d ) {
		case CVar(vr, swiz):
			var bits = Tools.swizBits(swiz, vr.type);
			// if no type, infer varying register
			inferKind(vr, VVar, p);

			switch( vr.kind ) {
			case VVar:
				if( !cur.vertex ) error("You can't write a variable in fragment shader", v.p);
				vr.write |= bits;
			case VParam:
				error("Constant values cannot be written", v.p);
			case VCompileConstant:
				error("Compile constants cannot be written", v.p);
			case VInput:
				error("Input values cannot be written", v.p);
			case VOut:
				if ( !cur.vertex && swiz != null ) error("Can not use write mask for fragment output", v.p);
				vr.write |= bits;
			case VTmp:
				vr.write |= bits;
			case VTexture:
				error("You can't write to a texture", v.p);
			}
			if( swiz != null ) {
				var min = -1;
				for( s in swiz ) {
					var k = Type.enumIndex(s);
					if( k <= min || (!config.allowAllWMasks && swiz.length > 1 && k != min + 1) ) error("Unsupported write mask", v.p);
					min = k;
				}
			}
		default:
			error("Invalid assign", p);
		}
		cur.exprs.push( { v : v, e : e } );
	}

	function allocVar( name, k, t, p ) {
		if( scopeVars.exists(name) && k != VTmp ) error("Duplicate variable '" + name + "'", p);
		if ( k != null ) checkTypeForKind(t, k, p);

		var v : Variable = {
			name : name,
			type : t,
			kind : k,
			id : allVars.length,
			refId : -1,
			index : 0,
			pos : p,
			read : false,
			write : if( k == null ) 0 else switch( k ) { case VInput, VParam: Tools.fullBits(t); default: 0; },
			assign : null,
		};
		allVars.push(v);
		scopeVars.set(name, v);
		return v;
	}

	// If the kind of a variable has not yet been determined, set it to k.
	function inferKind( v:Variable, k:VarKind, p) {
		if ( v.kind == null ) {
			if ( v.type == TBool ) {
				// Booleans can only be compile constants
				k = VCompileConstant;
			}

			checkTypeForKind( v.type, k, p );
			if ( v.read ) throw "assert";
			if ( v.write != 0 ) throw "assert";
			if ( v.assign != null ) throw "assert";

			v.kind = k;
			v.write = switch(k) { case VInput, VParam: Tools.fullBits(v.type); default: 0; }
			if( k == VParam )
				paramsInferred.push(v);
		}
	}

	function checkTypeForKind( type, kind, pos ) {
		switch ( kind ) {
		case VCompileConstant:
			switch ( type ) {
			case TBool, TInt, TFloat, TFloat2, TFloat3, TFloat4:
			default: error("Unsupported type for compile constant", pos);
			}
		default:
			if ( type == TBool ) error("Bool type only supported for compile constants", pos);
		}
	}

	function allocTemp( t, p ) {
		return allocVar("$t" + tempCount++, VTmp, t, p);
	}

	function allocConst( cvals : Array<String>, p ) : CodeValue {
		if ( cvals.length == 1 ) {
			if ( cvals[0] == "true" ) {
				if ( !inConstantExpr ) error("Can't use boolean values outside of constant expressions", p);
				return {d : CLiteral([1.0]), t : TBool, p:p};
			} else if ( cvals[0] == "false" ) {
				if ( !inConstantExpr ) error("Can't use boolean values outside of constant expressions", p);
				return {d : CLiteral([0.0]), t : TBool, p:p};
			} else if ( cvals[0] == "null" ) {
				if ( !inConstantExpr ) error("Can't use null outside of constant expressions", p);
			}
		}

		var pvals = [];
		for ( c in cvals ) {
			pvals.push( parseLiteral(c) );
		}
		return { d : CLiteral(pvals), t : Tools.makeFloat(cvals.length), p:p };
	}

	function constSwiz(k,count) {
		var s = [];
		var e = [X, Y, Z, W][k];
		for( i in 0...count ) s.push(e);
		return s;
	}

	function checkGlobalVars(vertex, fragment) {
		for ( v in globalVars ) {
			var p = v.pos;

			if ( v.kind == null ) {
				warn("Unused global variable '" + v.name + "'", p);
				continue;
			}

			switch ( v.kind ) {
			case VParam:
				if ( !v.read ) warn("Uniform variable '" + v.name + "' is not used.", p);
			case VCompileConstant:
				if ( !v.read ) warn("Compile constant '" + v.name + "' is not used.", p);
			case VInput:
				if( !v.read ) {
					warn("Input '" + v.name + "' is not used.", p);
					// force the input read
					if( config.forceReads ) {
						cur = vertex;
						addAssign( { d : CVar(allocTemp(TFloat4, p)), t : TFloat4, p : p }, { d : CVar(v), t : TFloat4, p : p }, p);
					}
				}
			case VVar, VTexture, VOut:
				// checked in checkLocalVars
			default:
				throw "assert";
			}
		}
	}

	function checkLocalVars() {
		var shader = (cur.vertex ? "vertex" : "fragment")+" shader";
		for( v in scopeVars ) {
			var p = v.pos;
			if ( v.kind == null )
				continue;

			switch( v.kind ) {
			case VOut:
				if( v.write == 0 ) error("Output is not written by " + shader, p);
				if( v.write != Tools.fullBits(v.type) ) error("Some output components are not written by " + shader, p);
				v.write = 0; // reset status between two shaders
			case VVar:
				if( cur.vertex ) {
					if( v.write == 0 ) {
						// delay error
					} else if( v.write != Tools.fullBits(v.type) )
						error("Some components of variable '" + v.name + "' are not written by vertex shader", p);
					else if( v.write != 15 ) {
						// force the output write in RuntimeCompiler
					}
				} else {
					if( !v.read && v.write == 0 )
						warn("Variable '" + v.name + "' is not used", p);
					else if( !v.read )
						warn("Variable '" + v.name + "' is not read by " + shader, p);
					else if( v.write == 0 )
						error("Variable '" + v.name + "' is not written by vertex shader", p);
				}
			case VTmp:
				if( !v.read ) warn("Unused local variable '" + v.name+"'", p);
			case VParam:
				if( !globalVars.exists(v.name) && !v.read ) warn("Parameter '" + v.name + "' not used by " + shader, p);
			case VTexture:
				if( !cur.vertex && !v.read ) {
					warn("Unused texture " + v.name, p);
					if( config.forceReads ) {
						// force the texture read
						var t = { d : CVar(allocTemp(TFloat4, p)), t : TFloat4, p : p };
						var cst = switch( v.type ) {
						case TTexture(cube): cube ? ["0","0","0"] : ["0","0"];
						default: throw "assert";
						}
						addAssign(t, { d : CTex(v,allocConst(cst,p),[]), t : TFloat4, p : p }, p);
					}
				}
			case VCompileConstant, VInput:
				// checked in checkGlobalVars
			}
		}
	}

	function rowVar( v : Variable, row : Int ) {
		var v2 = Reflect.copy(v);
		v2.name += "[" + row + "]";
		v2.index += row;
		v2.id = allVars.length;
		v2.refId = v.id;
		v2.type = Tools.makeFloat(switch( v.type ) {
		case TMatrix(r, c, t): if( t.t ) r else c;
		default: -1;
		});
		allVars.push(v2);
		return v2;
	}

	function checkReadVar( v : Variable, swiz, p ) {
		// If this is within a conditional, interpret as a compile constant, otherwise a uniform.
		inferKind(v, inConstantExpr ? VCompileConstant : VParam, p);

		if ( inConstantExpr ) {
			if ( v.kind == VParam && globalVars.exists(v.name) && paramsInferred.remove(v) ) {
				// Re-infer Uniform --> CompileConstant
				if ( !v.read ) throw "assert";
				checkTypeForKind(v.type, VCompileConstant, p);
				v.kind = VCompileConstant;
				if ( !globalsRead.exists(v.name) ) throw "assert";
			}

			switch ( v.kind ) {
			case VCompileConstant:
				v.read = true;
			default:
				if ( !iterators.exists(v.id) ) {
					error("You can only use constants and literals in a conditional statement.", p);
				}
			}
		} else {
			switch( v.kind ) {
			case VCompileConstant:
				v.read = true;
				if ( !globalsRead.exists(v.name) ) {
					globalsRead.set(v.name, v);
					cur.args.push(v);
				}
			case VOut: error("Output cannot be read", p);
			case VVar: if( cur.vertex ) error("You cannot read variable in vertex shader", p); v.read = true;
			case VParam:
				v.read = true;
				if ( globalVars.exists(v.name) && !globalsRead.exists(v.name) ) {
					globalsRead.set(v.name, v);
					cur.args.push(v);
				}
			case VTmp:
				if( v.write == 0 ) error("Variable '"+v.name+"' has not been initialized", p);
				var bits = Tools.swizBits(swiz, v.type);
				if( v.write & bits != bits ) error("Some fields of '"+v.name+"' have not been initialized", p);
				v.read = true;
			case VInput:
				v.read = true;
			case VTexture:
				if( !allowTextureRead )
					error("You can't read from a texture", p);
			}
		}
	}

	function checkRead( e : CodeValue ) {
		switch( e.d ) {
		case CLiteral(_):
		case CIf(_), CFor(_):
			throw "assert";
		case CVar(v, swiz):
			checkReadVar(v,swiz,e.p);
		case COp(_, e1, e2):
			checkRead(e1);
			checkRead(e2);
		case CUnop(_, e):
			checkRead(e);
		case CTex(t, v, _), CTexE(t, v, _):
			if( cur.vertex ) error("You can't read from texture in vertex shader", e.p);
			t.read = true;
			checkRead(v);
		case CSwiz(v, _):
			checkRead(v);
		case CBlock(_, v):
			checkRead(v);
		case CVector(values):
			for ( v in values ) checkRead(v);
		case CAccess(v1, e2):
			checkReadVar(v1,null,e.p);
			checkRead(e2);
		}
	}

	function compileValueOpt(e,?target) {
		return compileValue(optimizeValue(e),target);
	}

	function optimizeValue( e : ParsedValue ) : ParsedValue {
		switch( e.v ) {
		case PBlock(_), PReturn(_): throw "assert";
		case POp(op, e1, e2):
			var o1 = optimizeValue(e1);
			var o2 = optimizeValue(e2);
			switch( o1.v ) {
			case PConst(v1):
				switch( o2.v ) {
				case PConst(v2):
					var c = makeConstOp(op, v1, v2, e.p);
					if( c != null )
						return c;
				default:
				}
			default:
			}
			if( o1 != e1 || o2 != e2 )
				return { v : POp(op, o1, o2), p : e.p };
		case PUnop(op, e1):
			var o1 = optimizeValue(e1);
			switch( o1.v ) {
			case PConst(v):
				var c = makeConstUnop(op, v, o1.p);
				if( c != null )
					return c;
			default:
			}
			if( e1 != o1 )
				return { v : PUnop(op, o1), p : e.p };
		case PVector(el):
			var ol = new Array();
			for( e in el )
				ol.push(optimizeValue(e));
			return { v : PVector(ol), p : e.p };
		case PTex(v, acc, flags):
			var newFlags = [];
			for ( f in flags )
				newFlags.push( {v:f.v, e:optimizeValue(f.e), p:f.p} );
			return { v : PTex(v, optimizeValue(acc), newFlags), p : e.p };
		case PSwiz(e1, swiz):
			var o1 = optimizeValue(e1);
			if( o1 != e1 )
				return { v : PSwiz(o1, swiz), p : e.p };
		case PRow(e1, idx):
			return { v : PRow(optimizeValue(e1), idx), p : e.p };
		case PCall(n, el):
			var ol = new Array();
			for( e in el )
				ol.push(optimizeValue(e));
			return { v : PCall(n, ol), p : e.p };
		case PIff(ec, e1, e2):
			return { v : PIff(optimizeValue(ec), optimizeValue(e1), optimizeValue(e2)), p : e.p };
		case PVar(_), PLocal(_), PConst(_), PAccess(_), PIf(_), PFor(_):
		}
		return e;
	}

	function compileConditional( e : ParsedValue ) : CodeValue {
		var old = inConstantExpr;
		inConstantExpr = true;
		var result = null;
		switch ( e.v ) {
		case PVar(_):
			result = compileValue(e);
		case PVector(values):
			if( values.length == 0 || values.length > 4 )
				error("Vector size should be 1-4", e.p);
			var literals = [];
			for ( val in values ) {
				switch ( val.v ) {
				case PConst(i): literals.push(parseLiteral(i));
				default: error("Vector declarations in conditionals can only use literal values", e.p);
				}
			}
			result = { d: CLiteral(literals), t : Tools.makeFloat(literals.length), p : e.p };
		case PConst(i):
			return allocConst([i], e.p);
		case POp(op, e1, e2):
			result = makeOp(op, e1, e2, e.p, true);
		case PUnop(op, e1):
			result = makeUnop(op, e1, e.p, true);
		case PIff(cond, eif, eelse):
			result = makeIff(cond, eif, eelse, e.p);
		default:
			error("Unsupported operation in conditional", e.p);
		}
		if ( !old ) {
			checkRead(result);
		}
		inConstantExpr = old;
		return result;
	}

	function compileValue( e : ParsedValue, ?isTarget ) : CodeValue {
		switch( e.v ) {
		case PBlock(_), PReturn(_), PIf(_), PFor(_):
			throw "assert";
		case PVar(vname):
			var v = scopeVars.get(vname);
			if( v == null ) error("Unknown variable '" + vname + "'", e.p);
			var swiz = null;
			var t = v.type;
			return { d : CVar(v, swiz), t : t, p : e.p };
		case PAccess(vname, eindex):
			var v = scopeVars.get(vname);
			if( v == null ) error("Unknown variable '" + vname + "'", e.p);
			var type, index;
			switch( v.type ) {
			case TArray(t, _):
				type = t;
				index = compileValue(eindex);
				unify(index.t, TFloat, index.p);
			case TFloat, TFloat2, TFloat3, TFloat4:
				type = TFloat;
				index = compileConditional(eindex);
				if ( !tryUnify(index.t, TFloat) ) {
					unify(index.t, TInt, index.p);
				}
			default: error("You can't index a variable of this type", e.p);
			}
			return { d : CAccess(v, index), t : type, p : e.p };
		case PConst(i):
			return allocConst([i], e.p);
		case PLocal(v):
			var v = allocVar(v.n, VTmp, v.t, v.p);
			return { d : CVar(v), t : v.type, p : e.p };
		case PSwiz(v, s):
			// compile e[row].col
			if( s.length == 1 ) switch( v.v ) {
			case PRow(v, index):
				var v = compileValue(v,isTarget);
				switch( v.t ) {
				case TMatrix(r, c, t):
					if( t.t == null ) t.t = false;
					if( index < 0 || index >= r ) error("You can't access row " + index + " on " + typeStr(v.t), e.p);
					for( s in s ) if( Type.enumIndex(s) >= c ) error("You can't access colum " + Std.string(s) + " on " + typeStr(v.t), e.p);
					// inverse row/col
					if( t.t ) {
						var s2 = [[X,Y,Z,W][index]];
						index = Type.enumIndex(s[0]);
						s = s2;
						var tmp = r;
						r = c;
						c = r;
					}
					switch( v.d ) {
					case CVar(vr, _):
						checkRead(v);
						var v2 = rowVar(vr, index);
						return { d : CVar(v2, s), t : TFloat, p : e.p };
					default:
						// we could use a temp but there's a lot of calculus lost anyway, so let's the user think about it
						error("You can't access matrix row on a complex expression", e.p);
					}
				default:
					// let's fall through, we will get an error anyway
				}
			default:
			}
			var v = compileValue(v,isTarget);
			// check swizzling according to value type
			var count = switch( v.t ) {
			case TMatrix(_), TTexture(_), TArray(_): 0;
			default: Tools.floatSize(v.t);
			}
			// allow all components access on input and varying values only
			switch( v.d ) {
			case CVar(v, s):
				if( s == null && (v.kind == VInput || v.kind == VVar) ) count = 4;
			default:
			}
			// check that swizzling is correct
			for( s in s )
				if( Type.enumIndex(s) >= count )
					error("Invalid swizzling on " + typeStr(v.t), e.p);
			// build swizzling
			switch( v.d ) {
			case CVar(v, swiz):
				var ns;
				if( swiz == null )
					ns = s
				else {
					// combine swizzlings
					ns = [];
					for( s in s )
						ns.push(swiz[Type.enumIndex(s)]);
				}
				return { d : CVar(v, ns), t : Tools.makeFloat(s.length), p : e.p };
			default:
				return { d : CSwiz(v, s), t : Tools.makeFloat(s.length), p : e.p };
			}
		case POp(op, e1, e2):
			return makeOp(op, e1, e2, e.p);
		case PUnop(op, e1):
			return makeUnop(op, e1, e.p);
		case PTex(vname, acc, flags):
			var v = scopeVars.get(vname);
			if( v == null ) error("Unknown texture '" + vname + "'", e.p);
			var acc = compileValue(acc);

			var single = false;
			var tflags = [];
			var cflags = [];
			for ( f in flags ) {
				if ( f.v == null ) {
					switch ( f.e.v ) {
					case PConst(ident):
						switch (ident) {
						case "mm_no":
							cflags.push( { t:PMipMap, e: { d:CLiteral([0.0]), t:TFloat, p:f.p }} );
							tflags.push(TMipMapDisable);
						case "mm_near":
							cflags.push( { t:PMipMap, e: { d:CLiteral([1.0]), t:TFloat, p:f.p }} );
							tflags.push(TMipMapNearest);
						case "mm_linear":
							cflags.push( { t:PMipMap, e: { d:CLiteral([2.0]), t:TFloat, p:f.p }} );
							tflags.push(TMipMapLinear);
						case "nearest":
							cflags.push( { t:PFilter, e: { d:CLiteral([0.0]), t:TFloat, p:f.p }} );
							tflags.push(TFilterNearest);
						case "linear":
							cflags.push( { t:PFilter, e: { d:CLiteral([1.0]), t:TFloat, p:f.p }} );
							tflags.push(TFilterLinear);
						case "wrap":
							cflags.push( { t:PWrap, e: { d:CLiteral([1.0]), t:TFloat, p:f.p }} );
							tflags.push(TWrap);
						case "clamp":
							cflags.push( { t:PWrap, e: { d:CLiteral([0.0]), t:TFloat, p:f.p }} );
							tflags.push(TClamp);
						case "single":
							single = true;
							cflags.push( { t:PSingle, e: { d:CLiteral([1.0]), t:TFloat, p:f.p }} );
							tflags.push(TSingle);
						default:
							throw "assert"; // should've been caught in parser
						}
					default: throw "assert";
					}
				} else {
					var e = compileConditional(f.e);
					switch ( f.v.v ) {
					case PConst(ident):
						switch (ident) {
						case "mipmap": if (!tryUnify(e.t, TFloat)) unify(e.t, TInt, e.p); cflags.push({t:PMipMap, e:e});
						case "filter": if (!tryUnify(e.t, TFloat)) unify(e.t, TInt, e.p); cflags.push({t:PFilter, e:e});
						case "wrap": unify(e.t, TBool, e.p); cflags.push({t:PWrap, e:e});
						case "clamp": unify(e.t, TBool, e.p); cflags.push({t:PClamp, e:e});
						case "lod":
							unify(e.t, TFloat, e.p);
							cflags.push( { t:PLodBias, e:e } );
							switch( e.d ) {
							case CLiteral(v):
								tflags.push(TLodBias(v[0]));
							default:
							}
						default: throw "assert"; // should've been caught in parser
						}
					default: throw "assert";
					}
				}
			}

			switch( v.type ) {
			case TTexture(cube):
				if ( globalVars.exists(v.name) && !globalsRead.exists(v.name) ) {
					globalsRead.set(v.name, v);
					cur.tex.push(v);
				}
				unify(acc.t, cube?TFloat3:(single ? TFloat : TFloat2), acc.p);
			default: error("'"+vname + "' is not a texture", e.p);
			}
			if( cflags.length == tflags.length )
				return { d : CTex(v, acc, tflags), t : TFloat4, p : e.p };
			return { d : CTexE(v, acc, cflags), t : TFloat4, p : e.p };
		case PIff(cond,e1,e2):
			return makeIff(cond, e1, e2, e.p);
		case PVector(values):
			return compileVector(values, e.p);
		case PRow(v, index):
			var v = compileValue(v);
			switch( v.t ) {
			case TMatrix(r, c, t):
				if( index < 0 || index >= c ) error("You can't read row " + index + " on " + typeStr(v.t), e.p);
				if( t.t == null ) t.t = false;
				switch( v.d ) {
				case CVar(vr, swiz):
					if( t.t ) error("You can't read a row from a transposed matrix", e.p); // TODO : use temp
					checkRead(v);
					var vr = rowVar(vr, index);
					return { d : CVar(vr), t : vr.type, p : e.p };
				default:
					error("You can't read a row from a complex expression", e.p); // TODO : use temp
				}
			default:
				unify(v.t, TMatrix(4, 4, { t : null } ), v.p);
			}
			throw "assert"; // unreachable
		case PCall(n,vl):
			var h = helpers.get(n);
			if( h == null ) error("Unknown function '" + n + "'", e.p);
			var vals = [];
			allowTextureRead = true;
			for( v in vl )
				vals.push(compileValue(v));
			allowTextureRead = false;
			if( h.args.length != vl.length ) error("Function " + n + " requires " + h.args.length + " arguments", e.p);
			var old = cloneVars(scopeVars);
			// only allow access to globals/output from within our helper functions
			scopeVars = cloneVars(globalVars);

			// init args
			for( i in 0...h.args.length ) {
				var value = vals[i];
				var a = h.args[i];
				unify(value.t, a.t, value.p);
				switch( a.t ) {
				case TTexture(_):
					switch( value.d ) {
					case CVar(v, _):
						// copy variable
						scopeVars.set(a.n, v);
					default:
						error("Invalid texture access", value.p);
					}
				default:
					var v = allocVar(a.n, VTmp, a.t, a.p);
					addAssign( { d : CVar(v), t : v.type, p : v.pos }, value, value.p );
				}
			}
			// compile block
			var rold = ret;
			ret = { v : null };
			for( e in h.exprs )
				compileAssign(e.v, e.e, e.p);
			var v = ret.v;
			if( v == null )
				error("Missing return", h.pos);
			ret = rold;
			closeBlock(old);
			return { d : v.d, t : v.t, p : e.p };
		};
	}

	function compileVector(values:Array<ParsedValue>, p) {
		if( values.length == 0 || values.length > 4 )
			error("Vector size should be 1-4", p);

		var cvals = [];
		var numFloats = 0;
		var floatTypes = [ TFloat, TFloat2, TFloat3, TFloat4 ];
		for ( v in values ) {
			var e = compileValue(v);
			var unified = false;
			for ( t in floatTypes ) {
				if ( tryUnify(e.t, t) ) {
					unified = true;
					break;
				}
			}
			if ( !unified ) {
				unify(e.t, TFloat, e.p);
			}
			cvals.push(e);
			numFloats += Tools.floatSize(e.t);
		}

		if ( numFloats > 4 ) {
			error("Vector can only include up to 4 elements", p);
		}

		return { d : CVector(cvals), t : floatTypes[numFloats-1], p : p };
	}

	function tryUnify( t1 : VarType, t2 : VarType ) {
		if( Type.enumEq(t1, t2) ) return true;
		switch( t1 ) {
		case TMatrix(r,c,t1):
			switch( t2 ) {
			case TMatrix(r2, c2, t2):
				if( r != r2 || c != c2 ) return false;
				if( t1.t == null ) {
					if( t2.t == null ) t2.t = false;
					t1.t = t2.t;
					return true;
				}
				return( t1.t == t2.t );
			default:
			}
		case TTexture(c1):
			switch( t2 ) {
			case TTexture(c2): return c1 == c2;
			default:
			}
		case TFloat3:
			return t2 == TFloat3;
		case TFloat4, TInt:
			return (t2 == TFloat4 || t2 == TInt);
		default:
		}
		return false;
	}

	function unify(t1, t2, p) {
		if( !tryUnify(t1, t2) ) {
			// if we only have the transpose flag different, let's print a nice error message
			switch(t1) {
			case TMatrix(r, c, t):
				switch( t2 ) {
				case TMatrix(r2, c2, t):
					if( r == r2 && c == c2 && t.t != null ) {
						if( t.t )
							error("Matrix is transposed by another operation", p);
						else
							error("Matrix is not transposed by a previous operation", p);
					}
				default:
				}
			default:
			}
			// default error message
			error(typeStr(t1) + " should be " +typeStr(t2), p);
		}
	}

	function makeConstOp( op : CodeOp, v1 : String, v2 : String, p ) {
		var me = this;
		function makeConst(f:Float->Float->Float) {
			var r = f(parseLiteral(v1), parseLiteral(v2));
			if( r + 1 == r ) r = 0; // NaN / Infinite
			return { v : PConst(Std.string(r)), p : p };
		}
		return switch( op ) {
		case CAdd: makeConst(function(x, y) return x + y);
		case CSub: makeConst(function(x, y) return x - y);
		case CMul: makeConst(function(x, y) return x * y);
		case CMin: makeConst(function(x, y) return x < y ? x : y);
		case CMax: makeConst(function(x, y) return x < y ? y : x);
		case CLt: makeConst(function(x, y) return x < y ? 1 : 0);
		case CGte: makeConst(function(x, y) return x >= y ? 1 : 0);
		case CEq: makeConst(function(x, y) return x == y ? 1 : 0);
		case CNeq: makeConst(function(x, y) return x != y ? 1 : 0);
		case CDot: makeConst(function(x, y) return x * y);
		case CDiv: makeConst(function(x, y) return x / y);
		case CPow: makeConst(function(x, y) return Math.pow(x,y));
		case CMod: makeConst(function(x, y) return x % y);
		case CAnd: makeConst(function(x, y) { return (x > 0 && y > 0) ? 1 : 0; });
		case COr:  makeConst(function(x, y) { return (x > 0 || y > 0) ? 1 : 0; });
		case CCross: null;
		};
	}

	static function parseLiteral(s:String) : Float {
		if ( s == "null" ) { return 0.0; }
		if ( s == "true" ) { return 1.0; }
		else if ( s == "false" ) { return 0.0; }
		else return Std.parseFloat(s);
	}

	function makeIff( cond : ParsedValue, e1 : ParsedValue, e2 : ParsedValue, pos ) {
		var cond = inConstantExpr ? compileConditional(cond) : compileValue(cond);
		var cond2 = switch( cond.d ) {
			case COp(op, e1, e2):
				if( op == CGte || op == CLt )
					{ d : COp(op == CGte ? CLt : CGte,e1,e2), t : cond.t, p : cond.p }
				else if ( op == CAnd || op == COr )
					{ d : CUnop(CNot, cond), t : cond.t, p : cond.p }
				else null;
			case CVar(v, swiz): if ( v.type == TBool ) { d : CUnop(CNot, cond), t : cond.t, p : cond.p } else null;
			default: null;
		}
		if( cond2 == null ) unify(cond.t, TBool, cond.p);
		var e1 = inConstantExpr ? compileConditional(e1) : compileValue(e1);
		var e2 = inConstantExpr ? compileConditional(e2) : compileValue(e2);
		unify(e2.t, e1.t, e2.p);
		if( !Tools.isFloat(e1.t) && e1.t != TBool ) error("'if' values should be vectors or bools", pos);
		var mkCond = function(c) return c;
		if( cond.t != TBool && cond.t != e1.t ) {
			if( cond.t != TFloat ) unify(cond.t, e1.t, cond.p);
			cond = { d : CSwiz(cond, constSwiz(0, Tools.floatSize(e1.t))), t : e1.t, p : cond.p };
			cond2 = { d : CSwiz(cond2, constSwiz(0, Tools.floatSize(e1.t))), t : e1.t, p : cond.p };
		}
		// compile "if( c ) e1 else e2" into "c * e1 + (!c) * e2"
		// we could optimize by storing the value of "c" into a temp var
		var e1 = { d : COp(CMul, cond, e1), t : e1.t, p : pos };
		var e2 = { d : COp(CMul, cond2, e2), t : e2.t, p : pos };
		return { d : COp(CAdd, e1, e2), t : e1.t, p : pos };
	}

	function makeOp( op : CodeOp, e1 : ParsedValue, e2 : ParsedValue, p, ?conditional ) {
		switch( op ) {
		// optimize 1 / sqrt(x) && 1 / x
		case CDiv:
			switch( e1.v ) {
			case PConst(c):
				if( Std.parseFloat(c) == 1 ) {
					switch( e2.v ) {
					case PUnop(op, v):
						if( op == CSqrt )
							return makeUnop(CRsq, v, p);
					default:
					}
					return makeUnop(CRcp, e2, p);
				}
			default:
			}
		// optimize 2^x
		case CPow:
			switch( e1.v ) {
			case PConst(c):
				if( Std.parseFloat(c) == 2 )
					return makeUnop(CExp, e2, p);
			default:
			}
		// prevent use of boolean operators outside conditionals
		case CAnd, COr:
			if ( !conditional ) error("Unsupported operation outside of conditional", p);
		default:
		}

		var e1 = conditional ? compileConditional(e1) : compileValue(e1);
		var e2 = conditional ? compileConditional(e2) : compileValue(e2);

		// look for a valid operation as listed in "ops"
		var types = ops[Type.enumIndex(op)];
		if ( conditional ) {
			switch(op) {
			case CEq, CNeq:
				// override these operators to return booleans
				types = [
					{ p1 : TInt, p2 : TInt, r : TBool },
					{ p1 : TFloat, p2 : TInt, r : TBool },
					{ p1 : TInt, p2 : TFloat, r : TBool },
					{ p1 : TFloat, p2 : TFloat, r : TBool },
					{ p1 : TFloat2, p2 : TFloat2, r : TBool },
					{ p1 : TFloat3, p2 : TFloat3, r : TBool },
					{ p1 : TFloat4, p2 : TFloat4, r : TBool },
				];
			case CGte, CLt:
				// only support scalar comparisons
				types = [
					{ p1 : TInt, p2 : TInt, r : TBool },
					{ p1 : TFloat, p2 : TInt, r : TBool },
					{ p1 : TInt, p2 : TFloat, r : TBool },
					{ p1 : TFloat, p2 : TFloat, r : TBool },
				];
			default:
			}
		}

		var first = null;
		for( t in types ) {
			if( isCompatible(e1.t, t.p1) && isCompatible(e2.t, t.p2) ) {
				if( first == null ) first = t;
				if( tryUnify(e1.t, t.p1) && tryUnify(e2.t, t.p2) )
					return { d : COp(op, e1, e2), t : t.r, p : p };
			}
		}
		// if we have an operation on a single scalar, let's map it on all floats
		if( e2.t == TFloat && Tools.isFloat(e1.t) )
			for( t in types )
				if( isCompatible(e1.t, t.p1) && isCompatible(e1.t, t.p2) ) {
					var swiz = [];
					for( i in 0...Tools.floatSize(e1.t) )
						swiz.push(X);
					return { d : COp(op,e1,{ d : CSwiz(e2, swiz), t : e1.t, p : e2.p }), t : e1.t, p : p };
				}
		// ...or the other way around
		if( e1.t == TFloat && Tools.isFloat(e2.t) )
			for( t in types )
				if( isCompatible(e2.t, t.p1) && isCompatible(e2.t, t.p2) ) {
					var swiz = [];
					for( i in 0...Tools.floatSize(e2.t) )
						swiz.push(X);
					return { d : COp(op,{ d : CSwiz(e1, swiz), t : e2.t, p : e1.p }, e2), t : e2.t, p : p };
				}

		// we have an error, so let's find the most appropriate override
		// in order to print the most meaningful error message
		if( first == null )
			for( t in types )
				if( isCompatible(e1.t, t.p1) ) {
					first = t;
					break;
				}
		if( first == null )
			first = types[0];
		unify(e1.t, first.p1, e1.p);
		unify(e2.t, first.p2, e2.p);
		throw "assert";
		return null;
	}

	function makeConstUnop( op : CodeUnop, v : String, p ) {
		var me = this;
		function makeConst(f:Float->Float) {
			var r = f(Std.parseFloat(v));
			if( r + 1 == r ) r = 0; // NaN / Infinite
			return { v : PConst(Std.string(r)), p : p };
		}
		return switch( op ) {
		case CNorm, CTrans, CKill: null; // invalid
		case CInt: makeConst(function(x) return Std.int(x));
		case CFrac: makeConst(function(x) return x % 1.);
		case CExp: makeConst(Math.exp);
		case CAbs: makeConst(Math.abs);
		case CRsq: makeConst(function(x) return 1 / Math.sqrt(x));
		case CRcp: makeConst(function(x) return 1 / x);
		case CLog: makeConst(Math.log);
		case CSqrt: makeConst(Math.sqrt);
		case CSin: makeConst(Math.sin);
		case CCos: makeConst(Math.cos);
		case CSat: makeConst(Math.cos);
		case CNeg: makeConst(function(x) return -x);
		case CLen: makeConst(function(x) return x);
		case CNot: makeConst(function(x) return x == 0 ? 1.0 : 0.0);
		};
	}

	function makeUnop( op : CodeUnop, e : ParsedValue, p, ?conditional ) {
		switch ( op ) {
		case CNot:
			if ( !conditional ) error("Unsupported operation outside of conditional", p);
		default:
		}

		var e = conditional ? compileConditional(e) : compileValue(e);
		var rt = e.t;
		switch( op ) {
		case CNorm: rt = TFloat3;
		case CLen: rt = TFloat;
		case CTrans:
			switch( e.t ) {
			case TMatrix(r, c, t):
				// transpose-free ?
				if( t.t == null ) {
					t.t = true;
					e.p = p;
					return e;
				}
				if( config.inlTranspose ) {
					var v = switch( e.d ) {
					case CVar(v, _): v;
					default: error("You cannot transpose a complex expression", e.p);
					}
					var t0 = null;
					var tr = Tools.makeFloat(r);
					var vrow = [];
					for( s in 0...r )
						vrow.push(rowVar(v, s));
					for( i in 0...c ) {
						var t = allocTemp(tr, p);
						t.read = true; // will be readed by the matrix we build
						if( t0 == null ) t0 = t;
						for( s in 0...r )
							addAssign( { d : CVar(t,[[X, Y, Z, W][s]]), t : TFloat, p : p }, { d : CVar(vrow[s],[[X,Y,Z,W][i]]), t : TFloat, p : p }, p);
					}
					var vmt = Reflect.copy(t0);
					vmt.id = allVars.length;
					vmt.type = TMatrix(c, r, { t : !t.t } );
					vmt.write = Tools.fullBits(vmt.type);
					allVars.push(vmt);
					return { d : CVar(vmt), t : vmt.type, p : p };
				}
				return { d : CUnop(CTrans, e), t : TMatrix(c, r, { t : !t.t } ), p : p };
			default:
			}
		case CInt:
			// inline int(t) as t - frc(t)
			if( config.inlInt ) {
				if( !Tools.isFloat(e.t) )
					unify(e.t, TFloat4, e.p); // force error
				var v = allocTemp(e.t, p);
				var ev = { d : CVar(v), t : e.t, p : p };
				addAssign( ev, e, p);
				var efrc = { d : CUnop(CFrac, ev), t : e.t, p : p };
				return { d : COp(CSub, ev, efrc), t : e.t, p : p };
			}
		default:
		}
		if( !Tools.isFloat(e.t) )
			unify(e.t, TFloat4, e.p); // force error
		return { d : CUnop(op, e), t : rt, p : p };
	}

	function isCompatible( t1 : VarType, t2 : VarType ) {
		if( t1 == t2 ) return true;
		switch( t1 ) {
		case TMatrix(r,c,t1):
			switch( t2 ) {
			case TMatrix(r2,c2,t2):
				return r2 == r && c2 == c && ( t1.t == null || t2.t == null || t1.t == t2.t );
			default:
			}
		case TFloat3:
			return t2 == TFloat3;
		case TFloat4, TInt:
			return (t2 == TFloat4 || t2 == TInt);
		default:
		}
		return false;
	}

}
