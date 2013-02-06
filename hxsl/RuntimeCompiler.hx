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
	var global : Bool;
	var read : Bool;
	var write : Int;
	var newVar : Variable;
	var isVertex : Bool;
	var value : Const;
	var ref : Variable;
}

/**
	The RuntimeCompiler is in charge of :
	- eliminating conditional constructs (If/Cond)
	- evaluating Row/Vector
	- inlining the For loops
	- allocating variables index
	- creating constant pool
	- adding extra padding to ensure platform compat
**/
class RuntimeCompiler {
	
	var varProps : Map<Int,VarProps>;
	var usedVars : Array<Variable>;
	var constVars : Array<Variable>;
	var objectVars : Map<Int,{ v : Variable, fields : Map<String,Variable> }>;
	var varId : Int;
	
	// force replace of variables by their provided value
	var isCond : Bool;
	// do not force constants to be replaced by variables
	var isConst : Bool;
	var cur : Code;
	var defPos : Position;

	public var config : { padWrites : Bool };
	
	public function new() {
		varId = 0;
		config = {
			padWrites : true,
		};
	}
	
	function error( msg : String, pos : Position ) : Dynamic {
		if( pos == null ) pos = defPos;
		throw new Error(msg, pos);
		return null;
	}
	
	function props( v : Variable ) {
		var p = varProps.get(v.id);
		if( p == null ) {
			p = {
				global : false,
				read : false,
				write : 0,
				newVar : null,
				value : CNull,
				isVertex : false,
				ref : null,
			}
			varProps.set(v.id,p);
		}
		return p;
	}
	
	/**
		Compile the final shader : the params is a mapping of all the variables that will be applied at compilation time.
		The can be of the following value :
			* null
			* true/false for Bool
			* an Int base index in the paramsData table for all other types
	**/
	public function compile( data : Data, ?params : { }, ?paramsData : haxe.ds.Vector<Float> ) : Data {
		usedVars = [];
		constVars = [];
		varProps = new Map();
		objectVars = new Map();
		defPos = data.vertex.pos;
			
		var hVars = new Map();
		for( v in data.globals.concat(data.vertex.args).concat(data.fragment.args) )
			switch( v.kind ) {
			case VConst, VParam:
				hVars.set(v.name, v);
			case VInput:
				props(v).global = true;
			default:
			}
		if( params != null )
			for( f in Reflect.fields(params) ) {
				var v = hVars.get(f);
				if( v == null )
					error("Unknown params " + f, null);
				var val : Dynamic = Reflect.field(params, f);
				if( val == null )
					continue;
				function makeVal(val:Dynamic, t:VarType) {
					if( val == null )
						return CNull;
					switch( t ) {
					case TNull, TTexture(_): throw "assert";
					case TBool:
						if( !Std.is(val, Bool) )
							error("Invalid value for parameter " + v.name, null);
						return CBool(val);
					case TFloat:
						if( !Std.is(val, Float) )
							error("Invalid value for parameter " + v.name, null);
						return CFloat(val);
					case TArray(t, 0):
						if( !Std.is(val, Array) )
							error("Invalid value for parameter " + v.name, null);
						var a : Array<Dynamic> = val;
						return CArray([for( v in a ) makeVal(v, t)]);
					case TInt, TFloat2, TFloat3, TFloat4, TMatrix(_), TArray(_):
						if( !Std.is(val, Int) )
							error("Invalid value for parameter " + v.name, null);
						var index : Int = val;
						var size = Tools.floatSize(v.type);
						var a = [];
						for( i in 0...size ) {
							var v = paramsData == null ? 0 : paramsData[i + index];
							#if !flash if( v == null ) v = 0; #end
							a.push(v);
						}
						return a.length == 1 ? CFloat(a[0]) : CFloats(a);
					case TObject(fields):
						if( !Reflect.isObject(val) )
							error("Invalid value for parameter " + v.name, null);
						var obj = new Map();
						for( f in fields ) {
							var fv = Reflect.field(val, f.name);
							if( fv != null )
								obj.set(f.name, makeVal(fv, f.t));
						}
						return CObject(obj);
					}
				}
				props(v).value = makeVal(val, v.type);
			}
		
		var vertex = compileCode(data.vertex);
		var vconst = constVars;
		constVars = [];
		
		var fragment = compileCode(data.fragment);
		var fconst = constVars;
		constVars = [];
		
		usedVars.sort(sortById);
		
		indexVars(vertex, vconst);
		indexVars(fragment, fconst);
		
		var globals = [];
		for( v in usedVars ) {
			if( !props(v).global )
				continue;
			globals.push(v);
		}
		
		return {
			globals : globals,
			vertex : vertex,
			fragment : fragment,
		};
	}

	function isUnsupportedWriteMask( s : Array<Comp> ) {
		return s != null && s.length > 1 && (s[0] != X || s[1] != Y || (s.length > 2 && (s[2] != Z || (s.length > 3 && s[3] != W))));
	}
	
	function indexVars( c : Code, constVars : Array<Variable> ) {
		var indexes = [0, 0, 0, 0, 0, 0];

		function calculateUsedSize( v : Variable, index : { i : Int } ) : { v : Variable, size : Int } {
			v.index = index.i;
			var p = props(v);
			if( p.ref != null ) p.ref.index = v.index;
			
			switch( v.type ) {
			case TObject(fields):
				var o = objectVars.get(v.id);
				if( o == null ) throw "assert"; // not really used it seems ?
				// only take into account the actual used registers
				var tot = 0;
				var fl = [];
				for( f in fields ) {
					var fv = o.fields.get(f.name);
					if( fv != null ) {
						var k = calculateUsedSize(fv, index);
						tot += k.size;
						fl.push({ name : f.name, t : k.v.type });
					}
				}
				// mute the variable in usedVars with the one with the actual fields
				var index = Lambda.indexOf(usedVars, v);
				if( index >= 0 ) {
					v = { id : v.id, index : v.index, kind : v.kind, name : v.name, pos : v.pos, type : TObject(fl) };
					usedVars[index] = v;
				}
				return { v : v, size : tot };
			case TArray(_, size) if( objectVars.exists(v.id) ):
				var o = objectVars.get(v.id);
				var tot = 0;
				for( i in 0...size ) {
					var fv = o.fields.get(""+i);
					var k = calculateUsedSize(fv, index);
					tot += k.size;
				}
				return { v : v, size : tot };
			default:
				var size = Tools.regSize(v.type);
				index.i += size;
				return { v : v, size : size };
			}
		}
		
		for( v in usedVars.copy() ) {
			var p = props(v);
			if( p.isVertex == c.vertex ) {
				var tkind = Type.enumIndex(v.kind);
				var inf = calculateUsedSize(v, { i : indexes[tkind] } );
				var v = inf.v;
				indexes[tkind] += inf.size;
				switch( v.kind ) {
				case VConst, VTexture:
					c.args.push(v);
					props(v).global = false; // remove from global list
				case VParam:
					throw "assert"; // should have been translated to VConst
				case VInput, VOut, VVar, VTmp:
				}
			}
		}
		// move consts at the end
		var cdelta = indexes[Type.enumIndex(VConst)];
		for( v in constVars ) {
			if( v.kind != VConst ) throw "assert";
			v.index += cdelta;
		}
		
		c.tempSize = indexes[Type.enumIndex(VTmp)];
	}
	
	function sortById( v1 : Variable, v2 : Variable ) {
		return v1.id - v2.id;
	}
	
	function compileCode( c : Code ) : Code {
		isCond = false;
		isConst = false;
		cur = {
			vertex : c.vertex,
			args : [],
			consts : [],
			exprs : [],
			pos : c.pos,
			tempSize : 0,
		};
		for( e in c.exprs )
			compileAssign(e.v, e.e);
		checkVars();
		return cur;
	}
	
	function checkVars() {
		for( v in usedVars ) {
			var p = props(v);
			switch( v.kind ) {
			case VVar:
				if( p.write != fullBits(v.type) )
					error("Some components of variable '" + v.name + "' are not written by vertex shader", v.pos);
				else if( p.write != 15 && cur.vertex ) {
					// force the output write
					padWrite(v);
				}
			default:
			}
		}
	}
	
	function isGoodSwiz( s : Array<Comp> ) {
		if( s == null ) return true;
		var cur = 0;
		for( x in s )
			if( Type.enumIndex(x) != cur++ )
				return false;
		return true;
	}
	
	function padWrite( v : Variable ) {
		if( !config.padWrites )
			return;
		// if we already have a partial "mov" copy, we can simply extend the writing on other components
		for( e in cur.exprs ) {
			if( e.v == null ) continue;
			switch( e.v.d ) {
			case CVar(vv, sv):
				if( v == vv && isGoodSwiz(sv) ) {
					switch( e.e.d ) {
					case CVar(v2, sv2):
						// only allow "mov" extension if we are sure that the variable is padded with "1"
						if( v2.kind == VInput || v2.kind == VVar ) {
							// remove swizzle on write
							var vn = Reflect.copy(v);
							props(v).ref = vn;
							vn.type = TFloat4;
							e.v.d = CVar(vn);
							// remove swizzle on read
							if( isGoodSwiz(sv2) ) {
								var p = props(v2);
								var vn2 = p.ref;
								// allow several writes
								if( vn2 == null ) {
									vn2 = Reflect.copy(v2);
									vn2.type = TFloat4;
									p.ref = vn2;
								}
								e.e.d = CVar(vn2);
							} else
							// or pad swizzle on input var
								while( sv2.length < 4 )
									sv2.push(X);
							// adjust types
							e.e.t = e.v.t = TFloat4;
							return;
						}
					default:
					}
				}
			default:
			}
		}
		// store 1-values into remaining components
		var missing = [], ones = [];
		for( i in Tools.floatSize(v.type)...4 ) {
			missing.push(Type.createEnumIndex(Comp, i));
			ones.push(1.);
		}
		var c = allocConst(ones, v.pos);
		addAssign({ d : CVar(v, missing), t : Tools.makeFloat(missing.length), p : v.pos },c, v.pos);
	}
	
	function addAssign( v, e, p ) {
		cur.exprs.push( { v : v, e : e } );
	}
	
	function allocVar(name, k, t, p) {
		var id = -(varId + 1);
		varId++;
		var v : Variable = {
			id : id,
			name : name,
			kind : k,
			type : t,
			index : -1,
			pos : p,
		};
		return v;
	}
	
	function makeConst(index:Int, swiz, p) {
		var v = allocVar("$c" + index, VConst, TFloat4, p);
		constVars.push(v);
		v.index = index;
		return { d : CVar(v, swiz), t : Tools.makeFloat(swiz.length), p : p };
	}
	
	function mergeSwiz(from:Array<Comp>, to:Array<Comp>) {
		var out = [];
		for( s in to )
			out.push( from[Type.enumIndex(s)] );
		return out;
	}
	
	function allocConst( cvals : Array<Float>, p : Position ) : CodeValue {
		var swiz = [X, Y, Z, W];
		var dup = [];
		var dvals = [];

		// de-dupe the input values
		for( i in 0...cvals.length ) {
			var found = false;
			for( j in 0...dvals.length ) {
				if ( cvals[i] == dvals[j] ) {
					dup.push(swiz[j]);
					found = true;
					break;
				}
			}
			if( !found ) {
				dup.push(swiz[dvals.length]);
				dvals.push(cvals[i]);
			}
		}
		
		// find an already existing constant
		for( index in 0...cur.consts.length ) {
			var c = cur.consts[index];
			var s = [];
			for( v in dvals ) {
				for( i in 0...c.length )
					if( c[i] == v ) {
						s.push(swiz[i]);
						break;
					}
			}
			if( s.length == dvals.length )
				return makeConst(index,mergeSwiz(s,dup),p);
		}

		// find an empty slot
		for( i in 0...cur.consts.length ) {
			var c = cur.consts[i];
			if( c.length + dvals.length <= 4 ) {
				var s = [];
				for( v in dvals ) {
					s.push(swiz[c.length]);
					c.push(v);
				}
				return makeConst(i,mergeSwiz(s,dup),p);
			}
		}
		var index = cur.consts.length;
		cur.consts.push(dvals);
		return makeConst(index, mergeSwiz(swiz.splice(0, dvals.length),dup), p);
	}
	
	function isTrue( c : Const ) {
		return switch( c ) {
		case CNull: false;
		case CInt(i): i != 0;
		case CFloat(f): f != 0;
		case CBool(b): b;
		case CFloats(_), CObject(_), CArray(_): true;
		};
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
	
	function compileAssign( v : Null<CodeValue>, e : CodeValue ) {
		switch( e.d ) {
		case CIf(cond, eif, eelse):
			if( isTrue(compileCond(cond)) ) {
				for( e in eif )
					compileAssign(e.v, e.e);
			} else if( eelse != null ) {
				for( e in eelse )
					compileAssign(e.v, e.e);
			}
			return;
		case CFor(vloop, it, exprs):
			switch( it.d ) {
			case COp(CInterval, _first, _max):
				throw "TODO";
			default:
				var vit = compileValue(it);
				switch( vit.d ) {
				case CVar(v,_):
					switch( v.type ) {
					case TArray(t, size):
						var v = newVar(v, v.pos);
						var values = [];
						if( size == 0 ) {
							size = switch( compileCond(it) ) {
							case CArray(vl): values = vl; vl.length;
							case CNull: 0;
							default: throw "assert";
							}
							v.type = TArray(t, size);
						}
						var obj = objectVars.get(v.id);
						if( obj == null ) {
							obj = { v : v, fields : new Map() };
							objectVars.set(v.id, obj);
						}
						var p = props(vloop);
						for( i in 0...size ) {
							var vi = allocVar("$" + v.name + "#" + i, VConst, t, e.p);
							vi.index = i;
							obj.fields.set("" + i, vi);
							p.newVar = vi;
							p.value = props(vi).value = values[i];
							for( e in exprs )
								compileAssign(e.v, e.e);
						}
						return;
					default:
						throw "assert";
					}
				default:
					throw "assert";
				}
			}
			return;
		case CUnop(op, _):
			switch( op ) {
			case CKill:
				addAssign(null,compileValue(e), e.p);
				return;
			default:
			}
		default:
		}
		if( v == null )
			throw "assert "+Type.enumConstructor(e.d);
		var v = compileValue(v, true);
		var e = compileValue(e);
		addAssign(v,e,cur.pos);
	}
	
	function compileCond( v : CodeValue ) {
		var old = isCond, oldC = isConst;
		isCond = true;
		isConst = true;
		var v = compileValue(v);
		isCond = old;
		isConst = oldC;
		return switch( v.d ) {
		case CConst(c): c;
		default: throw "assert";
		}
	}
	
	// don't create a const-var
	function compileConstValue( v : CodeValue ) {
		return switch( v.d ) {
		case CConst(_): v;
		default:
			var old = isConst;
			isConst = true;
			v = compileValue(v);
			isConst = old;
			v;
		};
		
	}
	
	function compileValueForce( v : CodeValue ) {
		var old = isConst;
		isConst = false;
		v = compileValue(v);
		isConst = old;
		return v;
	}
	
	function newVar( v : Variable, p : Position ) {
		if( isCond ) {
			if( v.kind != VParam )
				throw "assert";
			return v;
		}
		if( v.id < 0 )
			return v;
		var p = props(v);
		if( p.newVar == null ) {
			var kind = v.kind;
			if( kind == VParam ) kind = VConst;
			p.newVar = {
				id : v.id,
				kind : kind,
				index : 0,
				name : v.name,
				type : v.type,
				pos : v.pos,
			};
			p.isVertex = cur.vertex;
			usedVars.push(p.newVar);
		}
		return p.newVar;
	}
	
	function compare(c1:Const, c2:Const) : Null<Int> {
		var f1, f2;
		switch( c1 ) {
		case CFloat(f): f1 = f;
		case CInt(i): f1 = i;
		default: return null;
		}
		switch( c2 ) {
		case CFloat(f): f2 = f;
		case CInt(i): f2 = i;
		default: return null;
		}
		return f1 < f2 ? -1 : (f1 > f2 ? 1 : 0);
	}

	function floatValue(c:Const) {
		return switch( c ) {
		case CNull: 0.;
		case CFloat(f): f;
		case CInt(i): i;
		default:
			throw "assert";
		}
	}
	
	function makeOp(op:CodeOp, e1:CodeValue, e2:CodeValue ) {
		e1 = compileConstValue(e1);
		e2 = compileConstValue(e2);
		
		var c1 = switch( e1.d ) {
		case CConst(c): c;
		default: null;
		}
		var c2 = switch( e2.d ) {
		case CConst(c): c;
		default: null;
		}
		if( c1 != null && c2 != null ) {
			function const(f:Float->Float->Float) {
				return CConst(CFloat(f(floatValue(c1), floatValue(c2))));
			}
			switch( op ) {
			case CEq:
				return CConst(CBool(Type.enumEq(c1,c2)));
			case CNeq:
				return CConst(CBool(!Type.enumEq(c1,c2)));
			case CLt:
				var c = compare(c1, c2);
				if( c != null )
					return CConst(CBool(c < 0));
			case CGt:
				var c = compare(c1, c2);
				if( c != null )
					return CConst(CBool(c > 0));
			case CLte:
				var c = compare(c1, c2);
				if( c != null )
					return CConst(CBool(c <= 0));
			case CGte:
				var c = compare(c1, c2);
				if( c != null )
					return CConst(CBool(c >= 0));
			case COr:
				return CConst(CBool(isTrue(c1) || isTrue(c2)));
			case CAnd:
				return CConst(CBool(isTrue(c1) && isTrue(c2)));
			case CAdd: return const(function(a, b) return a + b);
			case CSub: return const(function(a, b) return a - b);
			case CMul, CDot: return const(function(a, b) return a * b);
			case CDiv: return const(function(a, b) return a / b);
			case CPow: return const(function(a, b) return Math.pow(a, b));
			case CMod: return const(function(a, b) return a % b);
			case CMin: return const(function(a, b) return a < b ? a : b);
			case CMax: return const(function(a, b) return a > b ? a : b);
			case CCross:
			case CInterval: throw "assert";
			}
		}
		// force const building
		e1 = compileValueForce(e1);
		e2 = compileValueForce(e2);
		return COp(op, e1, e2);
	}
	
	function makeUnop( op : CodeUnop, e : CodeValue ) {
		e = compileConstValue(e);
		switch( e.d ) {
		case CConst(c):
			function const(f:Float->Float) {
				return CConst(CFloat(f(floatValue(c))));
			}
			switch( op ) {
			case CNorm, CTrans, CKill:
			case CInt: return const(function(x) return Std.int(x));
			case CFrac: return const(function(x) return x % 1.);
			case CExp: return const(Math.exp);
			case CAbs: return const(Math.abs);
			case CRsq: return const(function(x) return 1 / Math.sqrt(x));
			case CRcp: return const(function(x) return 1 / x);
			case CLog: return const(Math.log);
			case CSqrt: return const(Math.sqrt);
			case CSin: return const(Math.sin);
			case CCos: return const(Math.cos);
			case CSat: return const(Math.cos);
			case CNeg: return const(function(x) return -x);
			case CLen: return const(function(x) return x);
			case CNot:
				return CConst(CBool(!isTrue(c)));
			}
		default:
		}
		e = compileValueForce(e);
		return CUnop(op, e);
	}
	
	function compileValue( e : CodeValue, isTarget = false ) : CodeValue {
		var d = switch( e.d ) {
		case CConst(_):
			e.d;
		case CVar(v, swiz):
			var v = newVar(v, e.p);
			var p = props(v);
			if( isCond )
				CConst(p.value);
			else {
				if( isTarget )
					p.write |= swizBits(swiz, v.type);
				else
					p.read = true;
				CVar(v, swiz);
			}
		case COp(op, e1, e2):
			makeOp(op, e1, e2);
		case CTex(v, acc, mode):
			v = newVar(v, e.p); // texture
			props(v).read = true;
			acc = compileValue(acc);
			var flags = [];
			for( m in mode )
				switch( m.f ) {
				case CTFlag(f): flags.push(f);
				case CTParam(t, v):
					var c = compileCond(v);
					switch(t) {
					case PWrap:
						if( isTrue(c) ) flags.push(TWrap);
					case PMipMap:
						if( c != CNull )
							flags.push(isTrue(c) ? TMipMapLinear : TMipMapNearest);
					case PFilter:
						if( c != CNull )
							flags.push(isTrue(c) ? TFilterLinear : TFilterNearest);
					case PLodBias:
						switch( c ) {
						case CNull:
						case CInt(i):
							flags.push(TLodBias(i));
						case CFloat(f):
							flags.push(TLodBias(f));
						default:
							throw "assert";
						}
					case PSingle:
						if( isTrue(c) ) flags.push(TSingle);
					}
				}
			var mode = [];
			for( f in flags )
				mode.push({ f : CTFlag(f), p : e.p });
			CTex(v, acc, mode);
		case CCond(c, eif, eelse):
			if( isTrue(compileCond(c)) )
				return compileValue(eif);
			return compileValue(eelse);
		case CVector(vals):
			return compileVector(vals, e.p);
		case CUnop(op, e):
			makeUnop(op, e);
		case CSwiz(v, swiz):
			v = compileValue(v, isTarget);
			// build swizzling
			switch( v.d ) {
			case CVar(v, s2):
				var ns;
				if( s2 == null )
					ns = swiz;
				else {
					// combine swizzlings
					ns = [];
					for( s in swiz )
						ns.push(s2[Type.enumIndex(s)]);
				}
				return { d : CVar(v, ns), t : Tools.makeFloat(swiz.length), p : e.p };
			default:
				return { d : CSwiz(v, swiz), t : Tools.makeFloat(swiz.length), p : e.p };
			}
		case CRow(v, index):
			v = compileValue(v);
			index = compileValue(index);
			switch( v.d ) {
			case CVar(v, swiz):
				if( swiz == null )
					switch( v.type ) {
					case TArray(t, _):
						v = newVar(v, e.p);
						props(v).read = true;
						return { d : CAccess(v, index), t : t, p : e.p };
					default:
					}
			default:
			}
			throw "assert row " + v;
		case CAccess(v, idx):
			v = newVar(v, e.p);
			props(v).read = true;
			idx = compileValue(idx);
			return { d : CAccess(v, idx), t : e.t, p : e.p };
		case CSubBlock(tmp, v):
			var exprs = cur.exprs;
			cur.exprs = [];
			for( e in tmp )
				compileAssign(e.v, e.e);
			var tmp = cur.exprs;
			cur.exprs = exprs;
			return { d : CSubBlock(tmp, compileValue(v)), t : e.t, p : e.p };
		case CField(v, f):
			v = compileValue(v, isTarget);
			switch( v.d ) {
			case CVar(v, _):
				var obj = objectVars.get(v.id);
				if( obj == null ) {
					obj = { v : v, fields : new Map() };
					objectVars.set(v.id, obj);
				}
				var v2 = obj.fields.get(f);
				if( v2 == null ) {
					v2 = allocVar("$" + v.name + "." + f, v.kind, e.t, e.p);
					obj.fields.set(f, v2);
				}
				return { d : CVar(v2, null), t : v2.type, p : e.p };
			case CConst(CNull):
				return v;
			case CConst(CObject(fl)):
				var val = fl.get(f);
				var ft = TNull;
				switch( v.t ) {
				case TObject(fl):
					for( fi in fl )
						if( fi.name == f ) {
							ft = fi.t;
							break;
						}
				default:
				}
				return v == null ? { d : CConst(CNull), t : TNull, p : e.p } : { d : CConst(val), t : ft, p : e.p };
			default:
				throw "assert";
			}
		default:
			throw "assert "+Type.enumConstructor(e.d);
		}
		// translate the constant to the corresponding variable value
		if( !isConst )
			switch( d ) {
			case CConst(c):
				d = allocConst([floatValue(c)], e.p).d;
			default:
			}
		return { d : d, t : e.t, p : e.p };
	}
	
	function compileVector(values:Array<CodeValue>, p) {
		if( values.length == 0 || values.length > 4 )
			throw "assert";
		var consts = [];
		var exprs = [];
		for( i in 0...values.length ) {
			var e = compileConstValue(values[i]);
			switch( e.d ) {
			case CConst(c):
				consts.push(floatValue(c));
			default:
				exprs[i] = e;
			}
		}
		// all values are constants
		if( consts.length == values.length )
			return allocConst(consts, p);
		// declare a new temporary
		var v = allocVar("$tmp" + Lambda.count(varProps), VTmp, Tools.makeFloat(values.length), p);
		usedVars.push(v);
		// assign expressions first
		var old = cur.exprs;
		cur.exprs = [];
		var write = [];
		for( i in 0...values.length ) {
			var e = exprs[i];
			var c = [X, Y, Z, W][i];
			if( e == null ) {
				write.push(c);
				continue;
			}
			addAssign( { d : CVar(v, [c]), t : TFloat, p : e.p }, e, p);
		}
		// assign constants if any
		if( write.length > 0 ) {
			if( isUnsupportedWriteMask(write) )
				for( i in 0...write.length )
					addAssign( { d : CVar(v, [write[i]]), t : TFloat, p : p }, allocConst([consts[i]], p), p);
			else
				addAssign( { d : CVar(v, write), t : Tools.makeFloat(write.length), p : p }, allocConst(consts, p), p);
		}
		// return temporary
		var ret = { d : CVar(v), t : v.type, p : p };
		var sub = { d : CSubBlock(cur.exprs, ret), t : ret.t, p : p };
		cur.exprs = old;
		return sub;
	}
	
}