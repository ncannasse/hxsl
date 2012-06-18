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

// Responsible for taking intermediate code and compiling final code, given a set of constants at runtime.
class RuntimeCompiler
{
	var _compileVars : Hash<Variable>;
	var compileValues : Hash<Array<Float>>;
	var vars : Array<IntHash<Variable>>;
	var literals : IntHash<Variable>;
	var consts : Array<Array<Float>>;
	var tempMapping : IntHash<Int>;
	var cur : Code;
	var vertex : Code;
	var fragment : Code;
	var indexes : Array<Int>;
	var exprs : Array< {v:Null<CodeValue>, e:CodeValue} >;
	var evaluations : IntHash< {writeBits:Int, vals:Array<Float>} >;

	public function new() {
	}

	public function compile( intermediate:Data, compileValues : {} ) : Data {
		indexes = [0, 0, 0, 0, 0, 0];
		evaluations = new IntHash();
		vars = [];
		for ( i in 0...7 ) vars.push( new IntHash() );

		_compileVars = new Hash();
		for( c in intermediate.vars )
			switch( c.kind ) {
			case VCompileConstant: _compileVars.set(c.name, c);
			default:
			}
		this.compileValues = new Hash();
		if( compileValues != null ) {
			for ( c in Reflect.fields(compileValues) ) {
				var vr = _compileVars.get(c);
				if( vr == null ) {
					var pos = { file:"", min:0, max:0 };
					error("Unsupported compilation var: " + c, #if macro haxe.macro.Context.makePosition(pos) #else pos #end);
				}
				this.compileValues.set(c, convertCompileVar(Reflect.field(compileValues, c), vr));
			}
		}

		vertex = compileShader(intermediate.vertex);
		fragment = compileShader(intermediate.fragment);

		var newVars = [];
		var nextInputIndex = 0;
		for ( v in intermediate.vars ) {
			switch( v.kind ) {
			case VInput:
				v = getUsedVar(v);
				if( v != null ) {
					reindexVar(v, nextInputIndex);
					nextInputIndex += Tools.regSize(v.type);
				}
			case VCompileConstant:
				continue;
			default:
			}
			if( v != null )
				newVars.push(v);
		}
		padVaryingRegisterWrites();
		return { vertex : vertex, fragment : fragment, vars:newVars };
	}

	function convertCompileVar(val:Dynamic, v:Variable) : Array<Float> {
		switch ( v.type ) {
		case TBool:
			if ( !Std.is(val, Bool) ) error("Expected bool for compile var: " + v.name, v.pos);
			return [val ? 1.0 : 0.0];
		case TInt:
			if ( !Std.is(val, Int) ) error("Expected int for compile var: " + v.name, v.pos);
			var val : Int = val;
			return [val * 1.0];
		case TFloat:
			if ( !Std.is(val, Float) ) error("Expected float for compile var: " + v.name, v.pos);
			var val : Float = val;
			return [val];
		case TFloat2:
			if ( val == null ) {
				return [0.0, 0.0];
			#if flash
			} else if ( Std.is(val, flash.geom.Vector3D) ) {
				var val : flash.geom.Vector3D = val;
				return [val.x, val.y];
			#end
			} else if ( Std.is(val, Array) ) {
				var val:Array<Float> = val;
				if ( val.length != 2 ) error("Expected Float2 for compile var: " + v.name, v.pos);
				return val;
			} else {
				error("Bad input for compile var: " + v.name, v.pos);
			}
		case TFloat3:
			if ( val == null ) {
				return [0.0, 0.0, 0.0];
			#if flash
			} else if ( Std.is(val, flash.geom.Vector3D) ) {
				var val : flash.geom.Vector3D = val;
				return [val.x, val.y, val.z];
			#end
			} else if ( Std.is(val, Array) ) {
				var val:Array<Float> = val;
				if ( val.length != 3 ) error("Expected Float3 for compile var: " + v.name, v.pos);
				return val;
			} else {
				error("Bad input for compile var: " + v.name, v.pos);
			}
		case TFloat4:
			if ( val == null ) {
				return [0.0, 0.0, 0.0, 0.0];
			#if flash
			} else if ( Std.is(val, flash.geom.Vector3D) ) {
				var val : flash.geom.Vector3D = val;
				return [val.x, val.y, val.z, val.w];
			#end
			} else if ( Std.is(val, Array) ) {
				var val:Array<Float> = val;
				if ( val.length != 4 ) error("Expected Float4 for compile var: " + v.name, v.pos);
				return val;
			} else {
				error("Bad input for compile var: " + v.name, v.pos);
			}
		default:
			throw "assert";
		}
		return null;
	}

	function compileShader(code:Code) : Code {
		cur = code;
		consts = [];
		exprs = [];
		literals = new IntHash();

		var byIndex = function(l, r) {
			if ( l.index < r.index ) return -1;
			else if ( l.index > r.index ) return 1;
			else return 0;
		}

		for ( expr in code.exprs ) {
			processAssign(expr.v, expr.e);
		}

		// add referenced arguments
		var args = [];
		for ( arg in code.args ) {
			arg = getUsedVar(arg);
			if ( arg != null ) {
				args.push(arg);
			}
		}

		// offset index for literals based on referenced arguments
		var literalOffset = indexes[Type.enumIndex(VParam)];
		for ( v in literals ) {
			v.index += literalOffset;
		}

		// add referenced textures
		var tex = [];
		for ( t in code.tex ) {
			t = getUsedVar(t);
			if ( t != null ) {
				tex.push(t);
			}
		}

		var tempSize = indexes[Type.enumIndex(VTmp)];
		indexes[Type.enumIndex(VParam)] = 0;
		indexes[Type.enumIndex(VTmp)] = 0;
		vars[Type.enumIndex(VOut)].get(0).write = 0;

		// resort args list by register index
		args.sort(byIndex);
		tex.sort(byIndex);

		for ( i in 0...vars.length ) {
			switch (i) {
			case Type.enumIndex(VInput), Type.enumIndex(VVar), Type.enumIndex(VOut):
				continue;
			default: vars[i] = new IntHash();
			}
		}

		return {
			vertex : code.vertex,
			pos : code.pos,
			args : args,
			tex : tex,
			consts : consts,
			exprs : exprs,
			tempSize : tempSize
		};
	}

	function processAssign( v : CodeValue, e : CodeValue ) {
		if ( v == null ) switch ( e.d ) {
		case CIf(cond, eif, eelse):
			if ( andVec(evaluate(cond)) ) {
				for ( expr in eif ) {
					processAssign(expr.v, expr.e);
				}
			} else if ( eelse != null ) {
				for ( expr in eelse ) {
					processAssign(expr.v, expr.e);
				}
			}
			return;
		case CFor(it, start, end, exprs):
			// unroll the loop
			it = processVar(it);
			var starti = checkIntCast(evaluate(start)[0], start.p);
			var endi = checkIntCast(evaluate(end)[0], end.p);
			for ( i in starti...endi ) {
				var fi:Float = i;
				evaluations.set(it.id, {writeBits:1, vals:[fi]});
				for ( expr in exprs ) {
					processAssign(expr.v, expr.e);
				}
			}
			evaluations.remove(it.id);
			return;
		default:
		}


		exprs.push( {v:processDest(v), e:process(e)} );



		// track write of known values
		if ( v != null ) switch ( v.d ) {
		case CVar(vr, writeMask):
			var writeBits = Tools.swizBits(writeMask, vr.type);

			vr = processVar(vr);
			vr.assign = null;
			if ( writeBits == Tools.fullBits(vr.type) ) {
				switch( e.d ) {
				case CVar(v2, s2):
					v2 = processVar(v2);
					if( v2.kind != VTmp ) {
						if( s2 == null ) {
							s2 = [X, Y, Z, W];
							while( (1<<s2.length)-1 > writeBits )
								s2.pop();
						}
						vr.assign = { v : v2, s : s2 };
					}
				default:
				}
			}

			if ( vr.kind == VTmp && Tools.isFloat(vr.type) ) {
				if ( evaluatable(e) ) {
					var values = evaluate(e);
					var eval = null;
					if ( evaluations.exists(vr.id) ) {
						eval = evaluations.get(vr.id);
					} else {
						var vals = [];
						for ( i in 0...Tools.floatSize(vr.type) ) vals.push(0.);
						eval = {writeBits:0, vals:vals};
						evaluations.set(vr.id, eval);
					}

					eval.writeBits |= writeBits;
					if ( writeMask != null ) {
						for ( i in 0...writeMask.length ) {
							eval.vals[Type.enumIndex(writeMask[i])] = values[i];
						}
					} else {
						eval.vals = values;
					}
				} else {
					if ( evaluations.exists(vr.id) ) {
						var eval = evaluations.get(vr.id);
						eval.writeBits &= ~writeBits;
						if ( eval.writeBits == 0 ) {
							evaluations.remove(vr.id);
						}
					}
				}
			}
		default:
		}
	}

	function checkIntCast( f:Float, pos:Position ) : Int {
		var fi = Std.int(f);
		if ( f != fi ) error("Expected integer value", pos);
		return fi;
	}

	function andVec( vals:Array<Float> ) : Bool {
		if ( vals == null ) return false;
		for ( v in vals ) if ( v == 0 ) return false;
		return true;
	}

	function evaluateOp( a:Array<Float>, b:Array<Float>, op:Float->Float->Float ) {
		var maxLen = (a != null) ? a.length : 1;
		if ( b != null && b.length > maxLen ) maxLen = b.length;

		var out = [];
		for ( i in 0...maxLen ) {
			var q = (a != null && i < a.length) ? a[i] : 0;
			var r = (b != null && i < b.length) ? b[i] : 0;
			out.push( op(q, r) );
		}
		return out;
	}

	function getCompileValue( name:String, swiz:Array<Comp> ) {
		var vals = compileValues.get(name);
		if ( swiz != null ) {
			var out = [];
			for ( s in swiz ) {
				out.push( vals != null ? vals[Type.enumIndex(s)] : 0.0 );
			}
			return out;
		} else {
			return vals;
		}
	}

	function varEvaluatable( v : Variable, swiz ) {
		if ( v.kind == VCompileConstant ) return true;
		if ( v.kind == VParam && literals.exists(v.id) && Tools.isFloat(v.type) ) return true;
		if ( evaluations.exists(v.id) ) {
			var readBits = Tools.swizBits(swiz, v.type);
			return (evaluations.get(v.id).writeBits & readBits) == readBits;
		}
		return false;
	}

	function evaluatable( e : CodeValue ) {
		switch (e.d) {
		case CLiteral(_): return true;
		case CVar(vr, swiz): return varEvaluatable(vr, swiz);
		case CSwiz(e, swiz): return evaluatable(e);
		case COp(op, e1, e2): return evaluatable(e1) && evaluatable(e2);
		case CUnop(op, e): return evaluatable(e);
		case CAccess(v, idx):
			if ( evaluatable(idx) ) {
				if ( v.kind == VCompileConstant ) return true;
				if ( evaluations.exists(v.id) ) {
					var idx = Std.int(evaluate(idx)[0]);
					return (evaluations.get(v.id).writeBits & (1<<idx)) != 0;
				}
			}
			return false;
		case CVector(values):
			for ( v in values ) {
				if ( !evaluatable(v) ) return false;
			}
			return true;
		case CTex(_), CTexE(_), CBlock(_): return false;
		default: throw "assert";
		}
	}

	inline function evalSwiz(type, vals:Array<Float>, swiz:Array<Comp>) : Array<Float> {
		var out = [];
		if ( swiz == null ) {
			for ( i in 0...Tools.floatSize(type) ) {
				out.push(vals[i]);
			}
		} else {
			for ( s in swiz ) {
				out.push(vals[Type.enumIndex(s)]);
			}
		}
		return out;
	}

	function evaluate( cond : CodeValue ) : Array<Float> {
		switch ( cond.d ) {
		case CVar(vr, swiz):
			var bits = Tools.swizBits(swiz, vr.type);
			if ( evaluations.exists(vr.id) && (evaluations.get(vr.id).writeBits & bits) == bits ) {
				var eval = evaluations.get(vr.id);
				return evalSwiz(vr.type, eval.vals, swiz);
			} else if ( vr.kind == VCompileConstant ) {
				return getCompileValue(vr.name, swiz);
			} else if ( Tools.isFloat(vr.type) ) {
				if ( vr.kind != VParam || !literals.exists(vr.id) ) throw "assert";
				var const = consts[literals.get(vr.id).index];
				return evalSwiz(vr.type, const, swiz);
			} else {
				throw "assert";
			}
		case CAccess(v, idx):
			var v = evaluate({d:CVar(v), t:cond.t, p:cond.p});
			var idx = Std.int( evaluate(idx)[0] );
			return [ (v != null && idx < v.length) ? v[idx] : 0.0 ];
		case CVector(values):
			var newVals = [];
			for ( v in values ) {
				var eval = evaluate(v);
				for ( ev in eval ) {
					newVals.push(ev);
				}
			}
			return newVals;
		case CSwiz(e, swiz):
			var f = evaluate(e);
			var result = [];
			for ( s in swiz ) {
				var i = Type.enumIndex(s);
				result.push( (f != null && i < f.length) ? f[i] : 0.0 );
			}
			return result;
		case COp(op, e1, e2):
			var f1 = evaluate(e1);
			var f2 = evaluate(e2);

			var fn = switch ( op ) {
			case CAdd: function(x:Float,y:Float) return x + y;
			case CSub: function(x:Float,y:Float) return x - y;
			case CMul: function(x:Float,y:Float) return x * y;
			case CMin: function(x:Float,y:Float) return x < y ? x : y;
			case CMax: function(x:Float,y:Float) return x < y ? y : x;
			case CLt: function(x:Float,y:Float) return x < y ? 1. : 0.;
			case CGte: function(x:Float,y:Float) return x >= y ? 1. : 0.;
			case CEq, CNeq: function(x:Float,y:Float) return x == y ? 1. : 0.;
			case CDot: function(x:Float,y:Float) return x * y;
			case CDiv: function(x:Float,y:Float) return x / y;
			case CPow: function(x:Float,y:Float) return Math.pow(x,y);
			case CMod: function(x:Float,y:Float) return x % y;
			case CAnd: function(x:Float,y:Float) { return (x > 0. && y > 0.) ? 1. : 0.; };
			case COr:  function(x:Float,y:Float) { return (x > 0. || y > 0.) ? 1. : 0.; };
			default: throw "assert";
			}
			var eval = evaluateOp(f1, f2, fn);
			switch ( op ) {
			case CDot:
				var sum = 0.;
				for ( v in eval ) sum += v;
				return [sum];
			case CEq:
				return [andVec(eval) ? 1. : 0.];
			case CNeq:
				return [!andVec(eval) ? 1. : 0.];
			default:
				return eval;
			}
		case CUnop(op, e):
			var f = evaluate(e);
			var fn = switch ( op ) {
			case CInt: function(x:Float, y:Float) return Std.int(x);
			case CFrac: function(x:Float, y:Float) return x % 1.;
			case CExp: function(x:Float, y:Float) return Math.exp(x);
			case CAbs: function(x:Float, y:Float) return Math.abs(x);
			case CRsq: function(x:Float, y:Float) return 1 / Math.sqrt(x);
			case CRcp: function(x:Float, y:Float) return 1 / x;
			case CLog: function(x:Float, y:Float) return Math.log(x);
			case CSqrt: function(x:Float, y:Float) return Math.sqrt(x);
			case CSin: function(x:Float, y:Float) return Math.sin(x);
			case CCos: function(x:Float, y:Float) return Math.cos(x);
			case CSat: function(x:Float, y:Float) return Math.min(1, Math.max(0, x));
			case CNeg: function(x:Float, y:Float) return -x;
			case CLen: function(x:Float, y:Float) return x;
			case CNot: function(x:Float, y:Float) return x == 0 ? 1.0 : 0.0;
			default: throw "assert";
			}
			return evaluateOp(f, null, fn);
		case CLiteral(vals):
			return vals;
		default:
			throw "Unexpected: " + Std.string(cond.d);
		}
	}

	function evaluatesToConstant( e : CodeValue, val : Float ) {
		if ( evaluatable(e) ) {
			var eval = evaluate(e);
			for ( v in eval ) if ( v != val ) return false;
			return true;
		}
		return false;
	}

	function process( e : CodeValue ) {
		if ( evaluatable(e) ) {
			return allocConst(evaluate(e), e.p);
		}

		switch ( e.d ) {
		case CVar(vr, swiz):
			vr = processVar(vr);
			if ( vr.assign != null ) {
				// FIX: Do not follow assignments from varying to input in pixel shader
				var allowAssign = (vr.assign.v.kind == VInput) ? (cur.vertex || vr.kind != VVar) : true;
				if ( allowAssign ) {
					vr.read = true;
					swiz = (swiz != null) ? mergeSwiz(vr.assign.s, swiz) : vr.assign.s;
					vr = vr.assign.v;
				}
			}
			vr = checkRead(vr, swiz, e.p);
			return { d : CVar(vr, swiz), t : e.t, p : e.p };
		case COp(op, e1, e2):
			// Optimize out math expressions that can be determined now
			switch ( op ) {
			case CAdd:
				if ( evaluatesToConstant(e1, 0) ) {
					return process(e2);
				} else if ( evaluatesToConstant(e2, 0) ) {
					return process(e1);
				}
			case CSub:
				if ( evaluatesToConstant(e1, 0) ) {
					return { d: CUnop(CNeg, process(e2)), t : e.t, p : e.p };
				} else if ( evaluatesToConstant(e2, 0) ) {
					return process(e1);
				}
			case CMul:
				if ( evaluatesToConstant(e1, 0) || evaluatesToConstant(e2, 0) ) {
					return allocConst([0.,0.,0.,0.].splice(0, Tools.floatSize(e.t)), e.p);
				} else if ( evaluatesToConstant(e1, 1) ) {
					return process(e2);
				} else if ( evaluatesToConstant(e2, 1) ) {
					return process(e1);
				}
			case CDot:
				if ( evaluatesToConstant(e1, 0) || evaluatesToConstant(e2, 0) ) {
					return allocConst([0.], e.p);
				}
			case CDiv:
				if ( evaluatesToConstant(e2, 1) ) {
					return process(e1);
				}
			default:
			}
			return { d : COp(op, process(e1), process(e2)), t : e.t, p : e.p };
		case CUnop(op, i):
			return { d : CUnop(op, process(i)), t : e.t, p : e.p };
		case CAccess(vr, idx):
			vr = processVar(vr);
			switch ( vr.type ) {
			case TFloat, TFloat2, TFloat3, TFloat4:
				// convert index access into swizzle
				var eval = evaluate(idx);
				if ( eval.length != 1 ) throw "assert";
				var idx = checkIntCast(eval[0], idx.p);
				if ( idx < 0 || idx >= Tools.floatSize(vr.type) ) {
					error("Invalid index " + idx + " for variable " + vr.name, e.p);
				}
				return { d : CVar(vr, [[X,Y,Z,W][idx]]), t : TFloat, p : e.p };
			case TArray(at, _):
				return { d : CAccess(vr, process(idx)), t : at, p : e.p };
			default:
				throw "assert";
			}
		case CVector(values):
			return makeVector(values, e.p);
		case CTex(vr, acc, flags):
			return { d : CTex(processVar(vr), process(acc), flags), t : e.t, p : e.p };
		case CTexE(vr, acc, flags):
			var newFlags = [];
			for ( f in flags ) {
				var val = evaluate(f.e);
				if ( val.length > 1 ) { throw "assert"; }
				switch ( f.t ) {
				case PMipMap:
					switch ( checkIntCast(val[0], f.e.p) ) {
					case 0: newFlags.push(TMipMapDisable);
					case 1: newFlags.push(TMipMapNearest);
					case 2: newFlags.push(TMipMapLinear);
					default: error("Invalid value for mipmap: " + val[0], f.e.p);
					}
				case PFilter:
					switch ( checkIntCast(val[0], f.e.p) ) {
					case 0: newFlags.push(TFilterNearest);
					case 1: newFlags.push(TFilterLinear);
					default: error("Invalid value for filter: " + val[0], f.e.p);
					}
				case PWrap:
					newFlags.push( val[0] > 0 ? TWrap : TClamp );
				case PClamp:
					newFlags.push( val[0] > 0 ? TClamp : TWrap );
				case PLodBias:
					newFlags.push( TLodBias(val[0]) );
				case PSingle:
					if ( val[0] > 0 ) newFlags.push(TSingle);
				}
			}
			return { d : CTex(processVar(vr), process(acc), newFlags), t : e.t, p : e.p };
		case CSwiz(v, swiz):
			var v = process(v);
			// build swizzling
			switch( v.d ) {
			case CVar(v, s2):
				var ns;
				if( s2 == null ) {
					ns = swiz;
				} else {
					// combine swizzlings
					ns = [];
					for( s in swiz )
						ns.push(s2[Type.enumIndex(s)]);
				}
				return { d : CVar(v, ns), t : Tools.makeFloat(swiz.length), p : e.p };
			default:
				return { d : CSwiz(v, swiz), t : Tools.makeFloat(swiz.length), p : e.p };
			}
		case CBlock(exprs, vr):
			var old = this.exprs;
			this.exprs = [];
			for ( expr in exprs ) {
				processAssign(expr.v, expr.e);
			}
			var block = this.exprs;
			this.exprs = old;
			return { d : CBlock(block, process(vr)), t : e.t, p : e.p };
		default:
			error("Unsupported operation", e.p);
			return null;
		}
	}

	function makeVector(values:Array<CodeValue>, p) {
		var consts = [];
		var exprs = [];
		var constant = true;
		var numFloats = 0;
		for( i in 0...values.length ) {
			var e = values[i];
			switch( e.d ) {
			case CLiteral(c):
				for ( ic in c ) {
					consts.push(ic);
				}
				numFloats += c.length;
			default:
				constant = false;
				exprs[i] = process(e);
				numFloats += Tools.floatSize(exprs[i].t);
			}
		}
		// all values are constants
		if( constant ) {
			return allocConst(consts, p);
		}

		// declare a new temporary
		var v = allocTemp(Tools.makeFloat(numFloats), p);

		// assign expressions first
		var old = this.exprs;
		this.exprs = [];
		var write = [];
		var swiz = [X, Y, Z, W];
		var nexts = 0;
		for( i in 0...values.length ) {
			var e = exprs[i];
			var writeMask = [];
			for ( j in 0...Tools.floatSize(values[i].t) ) {
				if ( swiz[nexts] == null ) throw "assert";
				writeMask.push(swiz[nexts++]);
			}
			if( e == null ) {
				write = write.concat(writeMask);
				continue;
			}
			if ( isUnsupportedWriteMask(writeMask) ) {
				// store expression to a temporary and write each component
				var vv = allocTemp(e.t, e.p);
				processAssign( { d : CVar(vv), t : e.t, p : e.p }, e );
				for ( i in 0...writeMask.length ) {
					processAssign( { d : CVar(v, [writeMask[i]]), t : TFloat, p : p }, { d : CVar(vv, [swiz[i]]), t : TFloat, p : p } );
				}
			} else {
				processAssign( { d : CVar(v, writeMask), t : e.t, p : e.p }, e );
			}
		}
		// assign constants if any
		if( write.length > 0 ) {
			if( isUnsupportedWriteMask(write) ) {
				var nextc = 0;
				for( i in 0...write.length ) {
					processAssign( { d : CVar(v, [write[i]]), t : TFloat, p : p }, allocConst([consts[nextc++]], p) );
				}
			} else {
				processAssign( { d : CVar(v, write), t : Tools.makeFloat(write.length), p : p }, allocConst(consts, p) );
			}
		}
		// return temporary
		var ret = { d : CVar(v), t : v.type, p : p };
		var sub = { d : CBlock(this.exprs, ret), t : ret.t, p : p };
		this.exprs = old;
		return sub;
	}

	function allocTemp( type, pos ) {
		var id = 1000; while ( vars[Type.enumIndex(VTmp)].exists(id) ) ++id;
		var v = processVar({
			name : "$tmp" + id,
			kind : VTmp,
			type : type,
			id : id,
			index : indexes[Type.enumIndex(VTmp)],
			refId : -1,
			pos : pos,
			read : true,
			write : 0,
			assign : null
		});
		indexes[Type.enumIndex(VTmp)] += Tools.regSize(v.type);
		return v;
	}

	function isUnsupportedWriteMask( s : Array<Comp> ) {
		return s != null && s.length > 1 && (s[0] != X || s[1] != Y || (s.length > 2 && (s[2] != Z || (s.length > 3 && s[3] != W))));
	}

	function checkRead(vr:Variable, swiz, p) : Variable {
		if ( vr.kind == VTmp ) {
			if( vr.write == 0 ) error("Variable '"+vr.name+"' has not been initialized", p);
			var bits = Tools.swizBits(swiz, vr.type);
			if( vr.write & bits != bits ) error("Some fields of '"+vr.name+"' have not been initialized", p);
		} else if ( vr.kind == VVar ) {
			var bits = Tools.fullBits(vr.type);
			if ( vr.write & bits != bits ) error("Variable '"+vr.name+"' was not fully written to in vertex shader", p);
		} else if ( !cur.vertex && vr.kind == VInput ) {
			// auto-allocate a varying register of the appropriate type, and add an assignment in the vertex shader
			var varying = {
				name : vr.name,
				kind : VVar,
				type : vr.type,
				id : vr.id,
				refId : -1,
				index : 0,
				pos : vr.pos,
				read : true,
				write : 0,
				assign : null,
			};
			if ( getUsedVar(varying) == null ) {
				varying = processVar(varying);
				this.vertex.exprs.push({v:{d:CVar(varying), t:TFloat4, p:p}, e:{d:CVar(vr), t:TFloat4, p:p}});
			}
			vr = varying;
		}
		return vr;
	}

	function processDest( v : CodeValue ) : CodeValue {
		if ( v == null ) {
			return null;
		}

		switch ( v.d ) {
		case CVar(vr, swiz):
			vr = processVar(vr);
			var bits = Tools.swizBits(swiz, vr.type);

			switch ( vr.kind ) {
			case VVar:
				if( vr.write & bits != 0  ) error("Multiple writes to the same variable are not allowed", v.p);
			case VOut:
				if( !cur.vertex && vr.write != 0 ) error("You must use a single write for fragment shader output", v.p);
			case VParam:
				error("You can not write to an input param", v.p);
			default:
			}
			vr.write |= bits;
			return { d:CVar(vr, swiz), t:v.t, p:v.p };
		default:
			throw "assert";
		}
	}

	function padVaryingRegisterWrites() {
		// Make sure all varying registers are fully written
		for ( v in vars[Type.enumIndex(VVar)] ) {
			if ( v.write != 15 ) {
				padWrite(v);
			}
		}
	}

	function padWrite( v : Variable ) {
		// if we already have a partial "mov" copy, we can simply extend the writing on other components
		for( e in vertex.exprs ) {
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
							vn.type = TFloat4;
							e.v.d = CVar(vn);
							// remove swizzle on read
							if( isGoodSwiz(sv2) ) {
								var vn2 = Reflect.copy(v2);
								vn2.type = TFloat4;
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
			ones.push(1.0);
		}

		var c = allocConst(ones, v.pos);
		vertex.exprs.push( { v : { d : CVar(v, missing), t : Tools.makeFloat(missing.length), p : v.pos }, e : c } );
	}

	function isGoodSwiz( s : Array<Comp> ) {
		if( s == null ) return true;
		var cur = 0;
		for( x in s )
			if( Type.enumIndex(x) != cur++ )
				return false;
		return true;
	}


	function getUsedVar(v : Variable) : Variable {
		var ikind = Type.enumIndex(v.kind);
		return vars[Type.enumIndex(v.kind)].get(v.id);
	}

	function reindexVar(v : Variable, index:Int ) {
		if ( v.refId >= 0 ) throw "assert";
		if ( v.index != index ) {
			// reindex vars that refer to this var
			for ( ref in vars[Type.enumIndex(v.kind)] ) {
				if ( ref.refId == v.id ) {
					ref.index = (ref.index-v.index) + index;
				}
			}
			v.index = index;
		}
	}

	function processVar(v : Variable) : Variable {
		var kind = switch(v.kind) {
		case VCompileConstant: VParam;
		default: v.kind;
		}

		var ikind = Type.enumIndex(kind);
		if ( vars[ikind].exists(v.id) ) {
			return vars[ikind].get(v.id);
		}

		var newVar = null;
		switch ( v.kind ) {
		case VCompileConstant:
			var literal = compileValues.get(v.name);
			if ( literal == null ) {
				literal = [0.,0,0,0].splice(0, Tools.floatSize(v.type));
			}
			var swiz = [X, Y, Z, W].splice(0, Tools.floatSize(v.type));
			var e = makeConst(consts.length, swiz, v.pos);
			consts.push(literal);
			newVar = switch ( e.d ) {
			case CVar(v, _): v;
			default : throw "assert";
			}
		default:
			var index = v.index;
			if ( v.refId >= 0 ) {
				// row access of existing var
				index += vars[ikind].get(v.refId).index;
			} else {
				// new var
				index += indexes[Type.enumIndex(kind)];
				indexes[Type.enumIndex(kind)] += Tools.regSize(v.type);
			}
			newVar = {
				name : v.name,
				kind : kind,
				type : v.type,
				id : v.id,
				index : index,
				pos : v.pos,
				refId : -1,
				read : true,
				write : 0,
				assign : null,
			}
		}

		vars[ikind].set(v.id, newVar);
		return newVar;
	}

	function allocConst( cvals : Array<Float>, p : Position ) : CodeValue {
		var swiz = [X, Y, Z, W];
		var dup = [];
		var dvals = [];

		// de-dupe the input values
		for ( i in 0...cvals.length ) {
			var found = false;
			for ( j in 0...dvals.length ) {
				if ( cvals[i] == dvals[j] ) {
					dup.push(swiz[j]);
					found = true;
					break;
				}
			}
			if ( !found ) {
				dup.push(swiz[dvals.length]);
				dvals.push(cvals[i]);
			}
		}

		// find an already existing constant
		for( index in 0...consts.length ) {
			var c = consts[index];
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
		for( i in 0...consts.length ) {
			var c = consts[i];
			if( c.length + dvals.length <= 4 ) {
				var s = [];
				for( v in dvals ) {
					s.push(swiz[c.length]);
					c.push(v);
				}
				return makeConst(i,mergeSwiz(s,dup),p);
			}
		}
		var index = consts.length;
		consts.push(dvals);
		return makeConst(index, mergeSwiz(swiz.splice(0, dvals.length),dup), p);
	}

	function mergeSwiz(from:Array<Comp>, to:Array<Comp>) {
		var out = [];
		for ( s in to ) {
			out.push( from[Type.enumIndex(s)] );
		}
		return out;
	}

	function makeConst( index:Int, swiz, p:Position ) {
		var paramVars = vars[Type.enumIndex(VParam)];
		var id=5000; while ( paramVars.exists(id) ) ++id;
		var v : Variable = {
			name : "$c" + id,
			kind : VParam,
			type : TFloat4,
			id : id,
			index : index, // offset later
			refId : -1,
			pos : p,
			read : true,
			write : 0,
			assign : null,
		};
		vars[Type.enumIndex(VParam)].set(v.id, v);
		literals.set(v.id, v);
		return { d : CVar(v, swiz), t : Tools.makeFloat(swiz.length), p : p };
	}

	function error(msg:String, p) : Dynamic {
		throw new Error(msg, p);
		return null;
	}
}
