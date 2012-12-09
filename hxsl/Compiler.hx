/*
 * HxSL - Haxe Shader Language
 *
 * Copyright (c) 2012, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package hxsl;
import hxsl.Data;

private typedef VarProps = {
	var read : Bool;
	var global : Bool;
	var write : Int;
	var inferred : Bool;
}

class Compiler {

	var cur : Code;
	var vars : Hash<Variable>;
	var namedVars : Hash<Variable>;
	var allVars : Array<Variable>;
	var varProps : Array<VarProps>;
	var ops : Array<Array<{ p1 : VarType, p2 : VarType, r : VarType }>>;
	var helpers : Hash<Data.ParsedCode>;
	var ret : { v : CodeValue };
	var allowTextureRead : Bool;

	public var allowAllWMasks : Bool;

	public function new() {
		allowAllWMasks = false;
		ops = new Array();
		for( o in initOps() )
			ops[Type.enumIndex(o.op)] = o.types;
	}

	function initOps() {
		var mat4 = TMatrix(4, 4, { t : false } );
		var mat4_t = TMatrix(4, 4, { t : true } );
		var mat3 = TMatrix(3, 3, { t : false } );
		var mat3_t = TMatrix(3, 3, { t : true } );
		var mat34_t = TMatrix(3, 4, { t : true } );

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
				case CMin, CMax, CLt, CGte, CEq, CNeq, CLte, CGt: floats;
				case CDot: [ { p1 : TFloat4, p2 : TFloat4, r : TFloat }, { p1 : TFloat3, p2 : TFloat3, r : TFloat } ];
				case CCross: [ { p1 : TFloat3, p2 : TFloat3, r : TFloat3 }];
				case CAnd, COr: [{ p1 : TBool, p2 : TBool, r : TBool }];
				case CMul: floats.concat([
					{ p1 : TFloat4, p2 : mat4_t, r : TFloat4 },
					{ p1 : TFloat4, p2 : mat34_t, r : TFloat3 },
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

	public dynamic function warn( msg:String, p:Position) {
	}
	
	inline function props( v : Variable ) {
		return varProps[v.id];
	}

	public function compile( h : ParsedHxsl ) : Data {
		vars = new Hash();
		namedVars = new Hash();
		varProps = [];
		allVars = [];

		var out = allocVar("out", VOut, TFloat4, h.pos);
		props(out).global = true;
		helpers = h.helpers;

		var globals = [];
		for( v in h.globals ) {
			var v = allocVar(v.n, v.k, v.t, v.p);
			if( namedVars.get(v.name) != null )
				error("Duplicate variable "+v.name,v.pos);
			namedVars.set(v.name, v);
			globals.push(v);
			props(v).global = true;
		}

		var vertex = compileShader(h.vertex,true);
		var fragment = compileShader(h.fragment, false);
		
		return { globals : globals, vertex : vertex, fragment : fragment };
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
		for( a in c.args ) {
			var v;
			switch( a.t ) {
			case TTexture(_):
				if( cur.vertex ) error("You can't use a texture inside a vertex shader", a.p);
				v = allocVar(a.n, VTexture, a.t, a.p);
				cur.tex.push(v);
			default:
				v = allocVar(a.n, null, a.t, a.p);
				// set Const but allow to refine as Param later
				if( v.kind == null ) {
					v.kind = VConst;
					props(v).inferred = true;
				}
				cur.args.push(v);
			}
			if( namedVars.get(v.name) != null )
				error("Duplicate variable "+v.name,v.pos);
			namedVars.set(v.name, v);
		}

		for( e in c.exprs )
			compileAssign(e.v, e.e, e.p);

		checkVars();

		// cleanup
		for( v in vars )
			if( !props(v).global )
				vars.remove(v.name);

		return cur;
	}

	function saveVars() {
		var old = new Hash();
		for( v in vars.keys() )
			old.set(v, vars.get(v));
		return old;
	}

	function closeBlock( old : Hash<Variable> ) {
		for( v in vars )
			if( v.kind == VTmp && old.get(v.name) != v && !props(v).read )
				warn("Unused local variable '" + v.name + "'", v.pos);
		vars = old;
	}

	function compileAssign( v : Null<ParsedValue>, e : ParsedValue, p : Position ) {
		if( v == null ) {
			switch( e.v ) {
			case PBlock(el):
				var old = saveVars();
				for( e in el )
					compileAssign(e.v, e.e, e.p);
				closeBlock(old);
				return;
			case PReturn(v):
				if( ret == null ) error("Unexpected return", e.p);
				if( ret.v != null ) error("Duplicate return", e.p);
				ret.v = compileValue(v);
				return;
			case PIf(cond, eif, eelse):
				var cond = compileValue(cond);
				unify(cond.t, TBool, e.p);
				// save writes
				var oldWrite = [];
				for( p in varProps )
					oldWrite.push(p.write);
				var old = cur.exprs;
				cur.exprs = [];
				compileAssign(null, eif, p);
				var vif = cur.exprs;
				var velse = null;
				if( eelse == null ) {
					// restore writes
					for( i in 0...oldWrite.length )
						varProps[i].write = oldWrite[i];
				} else {
					// save and restore writes
					var ifWrite = [];
					for( i in 0...oldWrite.length ) {
						var p = varProps[i];
						ifWrite.push(p.write);
						varProps[i].write = oldWrite[i];
					}
					cur.exprs = [];
					compileAssign(null, eelse, p);
					velse = cur.exprs;
					// merge writes
					for( i in 0...oldWrite.length )
						varProps[i].write = oldWrite[i] | (ifWrite[i] & varProps[i].write);
				}
				cur.exprs = old;
				cur.exprs.push( { v : null, e : { d : CIf(cond, vif, velse), t : TNull, p : e.p } } );
				return;
			case PFor(it, first, last, loop):
				
				var first = compileValue(first);
				var last = compileValue(last);
				unify(first.t, TFloat, first.p);
				unify(last.t, TFloat, last.p);

				var it = allocVar(it, VTmp, TFloat, e.p);
				props(it).write = 1;

				var oldExprs = cur.exprs;
				var oldWrite = [];
				for( p in varProps )
					oldWrite.push(p.write);
				compileAssign(null, loop, p);
				var eloop = cur.exprs;
				cur.exprs = oldExprs;
				for( i in 0...oldWrite.length )
					varProps[i].write = oldWrite[i];
				
				vars.remove(it.name);
				cur.exprs.push({ v : null, e : { d : CFor(it, first, last, eloop), t : TNull, p : e.p } });
			default:
			}
			var e = compileValue(e);
			switch( e.d ) {
			case CUnop(op, _):
				if( op == CKill ) {
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
		var e = compileValue(e);
		switch( v.v ) {
		case PLocal(v):
			if( v.t == null ) v.t = e.t;
		default:
		}
		var v = compileValue(v,true);
		unify(e.t, v.t, e.p);
		addAssign(v, e, p);
	}

	function addAssign( v : CodeValue, e : CodeValue, p : Position ) {
		switch( v.d ) {
		case CVar(vr, swiz):
			var bits = swizBits(swiz, vr.type);
			var vp = props(vr);
			// first write on a unknown var : assume it's a varying and reset its written bits
			if( vr.kind == null ) {
				vr.kind = VVar;
				vp.write = 0;
			}
			switch( vr.kind ) {
			case VVar:
				if( !cur.vertex ) error("You can't write a variable in fragment shader", v.p);
				if( vp.write & bits != 0  ) error("Multiple writes to the same variable are not allowed", v.p);
				vp.write |= bits;
			case VConst, VParam:
				error("Constant values cannot be written", v.p);
			case VInput:
				error("Input values cannot be written", v.p);
			case VOut:
				if( !cur.vertex && vp.write != 0 ) error("You must use a single write for fragment shader output", v.p);
				vp.write |= bits;
			case VTmp:
				vp.write |= bits;
			case VTexture:
				error("You can't write to a texture", v.p);
			}
			if( swiz != null ) {
				var min = -1;
				for( s in swiz ) {
					var k = Type.enumIndex(s);
					if( k <= min || (!allowAllWMasks && swiz.length > 1 && k != min + 1) ) error("Unsupported write mask", v.p);
					min = k;
				}
			}
		default:
			error("Invalid assign", p);
		}
		cur.exprs.push( { v : v, e : e } );
	}

	function swizBits( s : Array<Comp>, t : VarType ) {
		if( s == null ) return fullBits(t);
		var b = 0;
		for( x in s )
			b |= 1 << Type.enumIndex(x);
		return b;
	}

	function fullBits( t : VarType ) {
		return (1 << Tools.floatSize(t)) - 1;
	}

	function allocVar( name, k, t, p ) {
		if( k == null ) {
			switch( t ) {
			case TBool:
				k = VParam;
			case TTexture(_):
				k = VTexture;
			default:
			}
		}
		var v : Variable = {
			id : allVars.length,
			name : name,
			type : t,
			kind : k,
			index : 0,
			pos : p,
		};
		varProps[v.id] = {
			global : false,
			read : false,
			write : if( k == null ) fullBits(t) else switch( k ) { case VInput, VConst, VParam: fullBits(t); default: 0; },
			inferred : false,
		};
		#if neko
		untyped v.__string = function() return neko.NativeString.ofString(__this__.name + "#" + __this__.id+" : "+Tools.typeStr(__this__.type));
		#end
		vars.set(name, v);
		allVars.push(v);
		return v;
	}

	function constSwiz(k,count) {
		var s = [];
		var e = [X, Y, Z, W][k];
		for( i in 0...count ) s.push(e);
		return s;
	}

	function checkVars() {
		var shader = (cur.vertex ? "vertex" : "fragment")+" shader";
		for( v in vars ) {
			var p = v.pos;
			var vp = props(v);
			// not used var
			if( v.kind == null ) {
				if( cur.vertex ) continue;
				v.kind = VConst;
			}
			switch( v.kind ) {
			case VOut:
				if( vp.write == 0 ) error("Output is not written by " + shader, p);
				if( vp.write != fullBits(v.type) ) error("Some output components are not written by " + shader, p);
				vp.write = 0; // reset status between two shaders
			case VVar:
				if( cur.vertex ) {
					if( vp.write == 0 ) {
						// delay error
					} else if( vp.write != fullBits(v.type) )
						error("Some components of variable '" + v.name + "' are not written by vertex shader", p);
				} else {
					if( !vp.read && vp.write == 0 )
						warn("Variable '" + v.name + "' is not used", p);
					else if( !vp.read )
						warn("Variable '" + v.name + "' is not read by " + shader, p);
					else if( vp.write == 0 )
						error("Variable '" + v.name + "' is not written by vertex shader", p);
				}
			case VInput:
				if( !cur.vertex && !vp.read )
					warn("Input '" + v.name + "' is not used", p);
			case VTmp:
				if( !vp.read ) warn("Unused local variable '" + v.name+"'", p);
			case VConst:
				if( !vp.read ) warn("Constant '" + v.name + "' not used" + (vp.global ? "" :" by " + shader), p);
			case VParam:
				if( !cur.vertex && !vp.read ) warn("Unused parameter '" + v.name + "'", p);
			case VTexture:
				if( !cur.vertex && !vp.read )
					warn("Unused texture " + v.name, p);
			}
		}
	}

	function isUnsupportedWriteMask( s : Array<Comp> ) {
		return s != null && s.length > 1 && (s[0] != X || s[1] != Y || (s.length > 2 && (s[2] != Z || (s.length > 3 && s[3] != W))));
	}

	function checkReadVar( v : Variable, swiz, p : Position ) {
		var vp = props(v);
		// first read on an unknown var, infer its type
		if( v.kind == null ) {
			v.kind = VConst;
			vp.inferred = true;
		}
		switch( v.kind ) {
		case VOut: error("Output cannot be read", p);
		case VVar: if( cur.vertex ) error("You cannot read varying in vertex shader", p); vp.read = true;
		case VConst, VParam:
			if( !cur.vertex ) {
				if( vp.read && v.index == 0 )
					error("You cannot read the same constant in both vertex and fragment shader", p);
				v.index = 1; // mark as used in fragment shader
			}
			vp.read = true;
		case VTmp:
			if( vp.write == 0 ) error("Variable '"+v.name+"' has not been initialized", p);
			var bits = swizBits(swiz, v.type);
			if( vp.write & bits != bits ) error("Some fields of '"+v.name+"' have not been initialized", p);
			vp.read = true;
		case VInput:
			// allow reading in fragment shader : we will create a varying at runtime compile time
			vp.read = true;
		case VTexture:
			if( !allowTextureRead )
				error("You can't read from a texture", p);
		}
	}

	function constValue( v : CodeValue ) : Null<Float> {
		switch( v.d ) {
		case CConst(c):
			switch(c) {
			case CFloat(f):
				return f;
			case CInt(i):
				return i;
			default:
			}
		default:
		}
		return null;
	}
	
	function compileValue( e : ParsedValue, ?isTarget : Bool ) : CodeValue {
		switch( e.v ) {
		case PBlock(_), PReturn(_):
			throw "assert";
		case PVar(vname):
			var v = vars.get(vname);
			if( v == null )
				error("Unknown variable '" + vname + "'", e.p);
			if( !isTarget )
				checkReadVar(v,null,e.p);
			return { d : CVar(v, null), t : v.type, p : e.p };
		case PConst(c):
			var t = switch( c ) {
			case CNull: TNull;
			case CInt(_), CFloat(_): TFloat;
			case CBool(_): TBool;
			case CFloats(ar):
				if( ar.length == 0 || ar.length > 4 )
					error("Floats must contain 1-4 values", e.p);
				Tools.makeFloat(ar.length);
			}
			return { d : CConst(c), t : t, p : e.p };
		case PLocal(v):
			var v = allocVar(v.n, VTmp, v.t, v.p);
			return { d : CVar(v), t : v.type, p : e.p };
		case PSwiz(v, swiz):
			// special case to restring reading to swiz
			var v = switch( v.v ) {
			case PVar(vname):
				var v = vars.get(vname);
				if( v == null )
					error("Unknown variable '" + vname + "'", e.p);
				{ d : CVar(v, null), t : v.type, p : e.p };
			default:
				compileValue(v, isTarget);
			}
			// check swizzling according to value type
			var count = switch( v.t ) {
			case TMatrix(_), TTexture(_), TArray(_): 0;
			default: Tools.floatSize(v.t);
			}
			// allow all components access on input and varying values only
			switch( v.d ) {
			case CVar(v, s):
				if( s == null && count > 0 && (v.kind == VInput || v.kind == VVar) ) count = 4;
			default:
			}
			// check that swizzling is correct
			for( s in swiz )
				if( Type.enumIndex(s) >= count )
					error("Invalid swizzling on " + Tools.typeStr(v.t), e.p);
			// build swizzling
			switch( v.d ) {
			case CVar(v, swiz2):
				var ns;
				if( swiz2 == null ) {
					if( !isTarget )
						checkReadVar(v, swiz, e.p);
					ns = swiz;
				} else {
					// combine swizzlings
					ns = [];
					for( s in swiz )
						ns.push(swiz2[Type.enumIndex(s)]);
				}
				return { d : CVar(v, ns), t : Tools.makeFloat(swiz.length), p : e.p };
			default:
				return { d : CSwiz(v, swiz), t : Tools.makeFloat(swiz.length), p : e.p };
			}
		case POp(op, e1, e2):
			return makeOp(op, e1, e2, e.p);
		case PUnop(op, e1):
			return makeUnop(op, e1, e.p);
		case PTex(vname, acc, flags):
			var v = vars.get(vname);
			if( v == null ) error("Unknown texture '" + vname + "'", e.p);
			props(v).read = true;
			
			var single = false;
			var tflags = [];
			var modes = [];
			
			for( f in flags ) {
				var param;
				switch( f.f ) {
				case PTFlag(fl):
					param = switch(fl) {
					case TMipMapDisable, TMipMapLinear, TMipMapNearest: PMipMap;
					case TWrap, TClamp: PWrap;
					case TFilterLinear, TFilterNearest: PFilter;
					case TSingle: single = true; PSingle;
					case TLodBias(_): PLodBias;
					}
					tflags.push( { f : CTFlag(fl), p : f.p } );
				case PTParam(p, v):
					var v = compileValue(v);
					param = p;
					var t = switch( p ) {
					case PLodBias: TFloat;
					case PMipMap, PSingle, PWrap, PFilter: TBool;
					}
					unify(v.t, t, v.p);
					tflags.push( { f : CTParam(p, v), p : f.p } );
				}
				if( modes[Type.enumIndex(param)] )
					error("Duplicate or conflicting texture flag", f.p);
				modes[Type.enumIndex(param)] = true;
			}
			var acc = compileValue(acc);
			switch( v.type ) {
			case TTexture(cube):
				unify(acc.t, cube?TFloat3:(single ? TFloat : TFloat2), acc.p);
			default: error("'"+vname + "' is not a texture", e.p);
			}
			return { d : CTex(v, acc, tflags), t : TFloat4, p : e.p };
		case PCond(cond,e1,e2):
			var cond = compileValue(cond);
			unify(cond.t, TBool, cond.p);
			var e1 = compileValue(e1);
			var e2 = compileValue(e2);
			unify(e2.t, e1.t, e2.p);
			return { d : CCond(cond, e1, e2), t : e1.t, p : e.p };
		case PVector(values):
			if( values.length == 0 || values.length > 4 )
				error("Vector size should be 1-4", e.p);
			var exprs = [];
			for( v in values ) {
				var e = compileValue(v);
				unify(e.t, TFloat, e.p);
				exprs.push(e);
			}
			return { d : CVector(exprs), t : Tools.makeFloat(exprs.length), p : e.p };
		case PRow(e1, e2):
			var e1 = compileValue(e1);
			var e2 = compileValue(e2);
			unify(e2.t, TFloat, e2.p);
			switch( e1.t ) {
			case TMatrix(rows, cols, t):
				if( t.t == null ) t.t = true;
				if( !t.t ) throw "You can't read a row from a not transposed matrix";
				var c = constValue(e2);
				if( c != null && (c < 0 || c >= rows || Std.int(c) != c) )
					error("Accessing matrix outside bounds", e2.p);
				return { d : CRow(e1, e2), t : Tools.makeFloat(cols), p : e.p };
			case TArray(t, size):
				var c = constValue(e2);
				if( c != null && (c < 0 || c >= size || Std.int(c) != c) )
					error("Accessing Array outside bounds", e2.p);
				return { d : CRow(e1, e2), t : t, p : e.p };
			default:
				return error(Tools.typeStr(e1.t) + " cannot be accessed this way", e1.p);
			}
		case PCall(n,vl):
			var h = helpers.get(n);
			if( h == null ) error("Unknown function '" + n + "'", e.p);
			var vals = [];
			allowTextureRead = true;
			for( v in vl )
				vals.push(compileValue(v));
			allowTextureRead = false;
			if( h.args.length != vl.length ) error("Function " + n + " requires " + h.args.length + " arguments", e.p);
			var old = saveVars();
			// only allow access to globals/output from within out helper functions
			for( v in old )
				if( !props(v).global )
					vars.remove(v.name);
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
						vars.set(a.n, v);
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
		case PIf(_), PFor(_):
			throw "assert";
		};
	}

	function tryUnify( t1 : VarType, t2 : VarType ) {
		if( t1 == t2 ) return true;
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
			error(Tools.typeStr(t1) + " should be " +Tools.typeStr(t2), p);
		}
	}

	function makeOp( op : CodeOp, e1 : ParsedValue, e2 : ParsedValue, p : Position ) {
		var e1 = compileValue(e1);
		var e2 = compileValue(e2);

		// look for a valid operation as listed in "ops"
		var types = ops[Type.enumIndex(op)];
		var first = null;
		for( t in types ) {
			if( isCompatible(e1.t, t.p1) && isCompatible(e2.t, t.p2) ) {
				if( first == null ) first = t;
				if( tryUnify(e1.t, t.p1) && tryUnify(e2.t, t.p2) )
					return { d : COp(op, e1, e2), t : t.r, p : p };
			}
		}
		// if we have an operation on a single scalar, let's map it on all floats
		if( e2.t == TFloat && isFloat(e1.t) )
			for( t in types )
				if( isCompatible(e1.t, t.p1) && isCompatible(e1.t, t.p2) ) {
					var swiz = [];
					var s = switch( e2.d ) {
					case CVar(_, s): if( s == null ) X else s[0];
					default: X;
					}
					for( i in 0...Tools.floatSize(e1.t) )
						swiz.push(s);
					return { d : COp(op,e1,{ d : CSwiz(e2, swiz), t : e1.t, p : e2.p }), t : e1.t, p : p };
				}
		// ...or the other way around
		if( e1.t == TFloat && isFloat(e2.t) )
			for( t in types )
				if( isCompatible(e2.t, t.p1) && isCompatible(e2.t, t.p2) ) {
					var swiz = [];
					var s = switch( e1.d ) {
					case CVar(_, s): if( s == null ) X else s[0];
					default: X;
					}
					for( i in 0...Tools.floatSize(e2.t) )
						swiz.push(s);
					return { d : COp(op,{ d : CSwiz(e1, swiz), t : e2.t, p : e1.p }, e2), t : e2.t, p : p };
				}
				
		// if we have a null check, infer a VParam
		if( e1.t == TNull && (op == CEq || op == CNeq) ) {
			var tmp = e1;
			e1 = e2;
			e2 = tmp;
		}
		if( e2.t == TNull ) {
			switch( e1.d ) {
			case CVar(v, swiz):
				if( swiz == null && (v.kind == VParam || v.kind == null || (v.kind == VConst && props(v).inferred)) ) {
					v.kind = VParam;
					return { d : COp(op, e1, e2), t : TBool, p : p };
				}
			default:
			}
			error("Only constants can be compared to null", e1.p);
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

	function makeUnop( op : CodeUnop, e : ParsedValue, p : Position ) {
		var e = compileValue(e);
		var rt = e.t;
		switch( op ) {
		case CNot:
			unify(e.t, TBool, e.p);
			return { d : CUnop(op, e), t : TBool, p : p };
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
				return { d : CUnop(CTrans, e), t : TMatrix(c, r, { t : !t.t } ), p : p };
			default:
			}
		default:
		}
		if( !isFloat(e.t) )
			unify(e.t, TFloat4, e.p); // force error
		return { d : CUnop(op, e), t : rt, p : p };
	}

	function isFloat( t : VarType ) {
		return switch( t ) {
		case TFloat, TFloat2, TFloat3, TFloat4, TInt: true;
		default: false;
		};
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