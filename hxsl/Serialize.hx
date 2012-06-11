/*
 * format - haXe File Formats
 *
 * Copyright (c) 2008, The haXe Project Contributors
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

// Helpers for serializing/unserializing intermediate hxsl opcodes
class Serialize
{
	static inline var CVAR = 0;
	static inline var COP = 1;
	static inline var CUNOP = 2;
	static inline var CACCESS = 3;
	static inline var CTEX = 4;
	static inline var CSWIZ = 5;
	static inline var CBLOCK = 6;
	static inline var CIF = 7;
	static inline var CLITERAL = 8;
	static inline var CFOR = 9;
	static inline var CTEXE = 10;
	static inline var CVECTOR = 11;

	public static function serialize( data : Data ) : String {
		var s = new haxe.Serializer();
		s.useCache = false;
		s.useEnumIndex = true;

		s.serialize(data.allVars.length);
		for ( v in data.allVars ) {
			s.serialize(v.refuid+1);
			if ( v.refuid != -1 ) {
				s.serialize(v.index);
			}
			serializeVarType(v.type, s);
			s.serialize(Type.enumIndex(v.kind));
			switch ( v.kind ) {
				case VTmp, VOut:
				default: s.serialize(v.name);
			}
			#if debug
				#if macro
				var posInfo = Context.getPosInfos(v.pos);
				#else
				var posInfo = v.pos;
				#end
			s.serialize(posInfo.file);
			s.serialize(posInfo.min);
			s.serialize(posInfo.max);
			#end
		}

		s.serialize(data.input.length);
		for ( v in data.input ) {
			s.serialize(v.uid);
		}

		var cv = [];
		for ( v in data.compileVars ) cv.push(v.uid);
		s.serialize(cv);

		serializeCode(data.vertex, s);
		serializeCode(data.fragment, s);
		return s.toString();
	}

	static function serializeCode( code:Code, s:haxe.Serializer ) : Void {
		#if debug
			#if macro
			var posInfo = Context.getPosInfos(code.pos);
			#else
			var posInfo = code.pos;
			#end
		s.serialize(posInfo.file);
		s.serialize(posInfo.min);
		s.serialize(posInfo.max);
		#end

		s.serialize(code.args.length);
		for ( arg in code.args ) {
			s.serialize(arg.uid);
		}

		if ( !code.vertex ) {
			s.serialize(code.tex.length);
			for ( tex in code.tex ) {
				s.serialize(tex.uid);
			}
		}

		s.serialize(code.exprs.length);
		for ( expr in code.exprs ) {
			serializeCodeValue(expr.v, s);
			serializeCodeValue(expr.e, s);
		}

		s.serialize(code.consts);
	}

	static function serializeCodeValue(v:CodeValue, s:haxe.Serializer) : Void {
		if ( v == null ) {
			s.serialize(null);
			return;
		}

		switch ( v.d ) {
		case CVar(v, swiz):
			s.serialize(CVAR);
			s.serialize(v.uid);
			serializeSwiz(swiz, s);
		case COp(op, e1, e2):
			s.serialize(COP);
			s.serialize(Type.enumIndex(op));
			serializeCodeValue(e1, s);
			serializeCodeValue(e2, s);
		case CUnop(op, e):
			s.serialize(CUNOP);
			s.serialize(Type.enumIndex(op));
			serializeCodeValue(e, s);
		case CAccess(v, idx):
			s.serialize(CACCESS);
			s.serialize(v.uid);
			serializeCodeValue(idx, s);
		case CTex(v, acc, flags):
			s.serialize(CTEX);
			s.serialize(v.uid);
			serializeTexFlags(flags, s);
			serializeCodeValue(acc, s);
		case CTexE(v, acc, flags):
			s.serialize(CTEXE);
			s.serialize(v.uid);
			s.serialize(flags.length);
			for ( f in flags ) {
				s.serialize(Type.enumIndex(f.t));
				serializeCodeValue(f.e, s);
			}
			serializeCodeValue(acc, s);
		case CSwiz(e, swiz):
			s.serialize(CSWIZ);
			serializeSwiz(swiz, s);
			serializeCodeValue(e, s);
		case CBlock(exprs, v):
			s.serialize(CBLOCK);
			s.serialize(exprs.length);
			for ( expr in exprs ) {
				serializeCodeValue(expr.v, s);
				serializeCodeValue(expr.e, s);
			}
			serializeCodeValue(v, s);
		case CIf(cond, eif, eelse):
			s.serialize(CIF);
			serializeCodeValue(cond, s);
			s.serialize(eif.length);
			for ( expr in eif ) {
				serializeCodeValue(expr.v, s);
				serializeCodeValue(expr.e, s);
			}
			s.serialize(eelse != null ? eelse.length : 0);
			if ( eelse != null ) for ( expr in eelse ) {
				serializeCodeValue(expr.v, s);
				serializeCodeValue(expr.e, s);
			}
		case CFor(it, start, end, exprs):
			s.serialize(CFOR);
			s.serialize(it.uid);
			serializeCodeValue(start, s);
			serializeCodeValue(end, s);
			s.serialize(exprs.length);
			for ( expr in exprs ) {
				serializeCodeValue(expr.v, s);
				serializeCodeValue(expr.e, s);
			}
		case CVector(values):
			s.serialize(CVECTOR);
			s.serialize(values.length);
			for ( v in values ) serializeCodeValue(v, s);
		case CLiteral(value):
			s.serialize(CLITERAL);
			s.serialize(value);
		}

		serializeVarType(v.t, s);

		#if debug
			#if macro
			var posInfo = Context.getPosInfos(v.p);
			#else
			var posInfo = v.p;
			#end
		s.serialize(posInfo.file);
		s.serialize(posInfo.min);
		s.serialize(posInfo.max);
		#end
	}

	static function serializeVarType(type:VarType, s:haxe.Serializer) {
		if ( type == null ) {
			s.serialize(null);
		} else {
			s.serialize(Type.enumIndex(type));
			switch ( type ) {
			case TMatrix(r, c, t):
				s.serialize(r);
				s.serialize(c);
				s.serialize(t.t);
			case TTexture(cube):
				s.serialize(cube);
			case TArray(atype, size):
				serializeVarType(atype, s);
				s.serialize(size);
			default:
			}
		}
	}

	static inline function serializeSwiz( swiz:Array<Comp>, s:haxe.Serializer ) : Void {
		if ( swiz != null ) {
			s.serialize(swiz.length);
			for ( c in swiz ) s.serialize(Type.enumIndex(c));
		} else {
			s.serialize(0);
		}
	}

	static inline function serializeTexFlags( flags:Array<TexFlag>, s:haxe.Serializer ) : Void {
		if ( flags != null ) {
			s.serialize(flags.length);
			for ( c in flags ) {
				s.serialize(Type.enumIndex(c));
				switch ( c ) {
				case TLodBias(bias):
					s.serialize(bias);
				default:
				}
			}
		} else {
			s.serialize(0);
		}
	}

	public static function unserialize( s:String ) : Data {
		var s = new haxe.Unserializer(s);
		var varmap = new IntHash<Variable>();
		var numVars = s.unserialize();
		var allVars = [];
		for ( i in 0...numVars ) {
			var id:Int = i;
			var refId:Int = s.unserialize();
			var index = 0;
			if ( refId != 0 ) {
				index = s.unserialize();
			}
			var type = unserializeVarType(s);
			var kind = Type.createEnumIndex(VarKind, s.unserialize());
			var name = switch ( kind ) {
				case VTmp, VOut: "";
				default: s.unserialize();
			}
			var pos = unserializePos(s);

			var v:Variable = {
				name:name,
				id:id,
				refId:refId-1,
				index:index,
				type:type,
				kind:kind,
				pos:pos,
				kindInferred:false,
				read:false,
				write:0,
				assign:null
			};
			varmap.set(id, v);
			allVars.push(v);
		}

		var vertex = unserializeCode(varmap, s, true);
		var fragment = unserializeCode(varmap, s, false);

		return { vars : allVars, vertex:vertex, fragment:fragment };
	}

	static function unserializeCode( varmap:IntHash<Variable>, s:haxe.Unserializer, vertex:Bool ) : Code {
		var pos = unserializePos(s);

		var numArgs = s.unserialize();
		var args = [];
		for ( i in 0...numArgs ) {
			args.push( varmap.get(s.unserialize()) );
		}

		var tex = [];
		if ( !vertex ) {
			var numTex = s.unserialize();
			for ( i in 0...numTex ) {
				tex.push( varmap.get(s.unserialize()) );
			}
		}

		var numExprs = s.unserialize();
		var exprs = [];
		for ( i in 0...numExprs ) {
			exprs.push( {v:unserializeCodeValue(varmap, s), e:unserializeCodeValue(varmap, s)} );
		}

		var consts:Array<Array<Float>> = s.unserialize();

		return { pos:pos, args:args, tex:tex, exprs:exprs, consts:consts, vertex:vertex, tempSize:0 };
	}

	static function unserializeCodeValue( varmap:IntHash<Variable>, s:haxe.Unserializer ) : CodeValue {
		var dindex:Null<Int> = s.unserialize();
		if ( dindex == null ) {
			return null;
		}
		var decl = switch ( dindex ) {
		case CVAR:
			var v = varmap.get(s.unserialize());
			var swiz = unserializeSwiz(s);
			CVar(v, swiz);
		case COP:
			var op = Type.createEnumIndex(CodeOp, s.unserialize());
			var e1 = unserializeCodeValue(varmap, s);
			var e2 = unserializeCodeValue(varmap, s);
			COp(op, e1, e2);
		case CUNOP:
			var op = Type.createEnumIndex(CodeUnop, s.unserialize());
			var e = unserializeCodeValue(varmap, s);
			CUnop(op, e);
		case CACCESS:
			var v = varmap.get(s.unserialize());
			var idx = unserializeCodeValue(varmap, s);
			CAccess(v, idx);
		case CTEX:
			var v = varmap.get(s.unserialize());
			var flags = unserializeTexFlags(s);
			var acc = unserializeCodeValue(varmap, s);
			CTex(v, acc, flags);
		case CTEXE:
			var v = varmap.get(s.unserialize());
			var numFlags:Int = s.unserialize();
			var flags = [];
			for ( i in 0...numFlags ) {
				var t = Type.createEnumIndex(TexParam, s.unserialize());
				var e = unserializeCodeValue(varmap, s);
				flags.push({t:t, e:e});
			}
			var acc = unserializeCodeValue(varmap, s);
			CTexE(v, acc, flags);
		case CSWIZ:
			var swiz = unserializeSwiz(s);
			var e = unserializeCodeValue(varmap, s);
			CSwiz(e, swiz);
		case CBLOCK:
			var numExprs:Int = s.unserialize();
			var exprs = [];
			for ( i in 0...numExprs ) {
				var v = unserializeCodeValue(varmap, s);
				var e = unserializeCodeValue(varmap, s);
				exprs.push( {v : v, e : e} );
			}
			var v = unserializeCodeValue(varmap, s);
			CBlock(exprs, v);
		case CIF:
			var cond = unserializeCodeValue(varmap, s);
			var eifLen:Int = s.unserialize();
			var eif = [];
			for ( i in 0...eifLen ) {
				var v = unserializeCodeValue(varmap, s);
				var e = unserializeCodeValue(varmap, s);
				eif.push({ v : v, e : e });
			}
			var eelseLen:Int = s.unserialize();
			var eelse = null;
			if ( eelseLen != 0 ) {
				eelse = [];
				for ( i in 0...eelseLen ) {
					var v = unserializeCodeValue(varmap, s);
					var e = unserializeCodeValue(varmap, s);
					eelse.push({ v : v, e : e });
				}
			}
			CIf(cond, eif, eelse);
		case CFOR:
			var it = varmap.get(s.unserialize());
			var start = unserializeCodeValue(varmap, s);
			var end = unserializeCodeValue(varmap, s);
			var numExprs:Int = s.unserialize();
			var exprs = [];
			for ( i in 0...numExprs ) {
				var v = unserializeCodeValue(varmap, s);
				var e = unserializeCodeValue(varmap, s);
				exprs.push( {v : v, e : e} );
			}
			CFor(it, start, end, exprs);
		case CVECTOR:
			var numVals = s.unserialize();
			var vals = [];
			for ( i in 0...numVals ) vals.push(unserializeCodeValue(varmap, s));
			CVector(vals);
		case CLITERAL:
			var value = s.unserialize();
			CLiteral(value);
		default:
			throw "assert";
		}

		var type = unserializeVarType(s);
		var pos = unserializePos(s);
		return {d:decl, t:type, p:pos};
	}

	static inline function unserializePos(s:haxe.Unserializer) {
		#if debug
		var file:String = s.unserialize();
		var min:Int = s.unserialize();
		var max:Int = s.unserialize();
		return makePosition({file:file, min:min, max:max});
		#else
		return makePosition({file:"", min:0, max:0});
		#end
	}

	static function makePosition(info) {
		#if macro
			return Context.makePosition(info);
		#else
			return info;
		#end
	}

	static function unserializeVarType(s:haxe.Unserializer) {
		var typeIndex:Null<Int> = s.unserialize();
		if ( typeIndex == null ) return null;
		switch ( typeIndex ) {
		case 6:
			var r:Int = s.unserialize();
			var c:Int = s.unserialize();
			var transpose = s.unserialize();
			return TMatrix(r,c,{t:transpose});
		case 7:
			var cube = s.unserialize();
			return TTexture(cube);
		case 8:
			var type = unserializeVarType(s);
			var size = s.unserialize();
			return TArray(type, size);
		default:
			return Type.createEnumIndex(VarType, typeIndex);
		}
	}

	static inline function unserializeSwiz(s:haxe.Unserializer) {
		var len:Int = s.unserialize();
		var swiz = null;
		if ( len != 0 ) {
			swiz = [];
			for ( i in 0...len ) swiz.push(Type.createEnumIndex(Comp, s.unserialize()));
		}
		return swiz;
	}

	static inline function unserializeTexFlags(s:haxe.Unserializer) {
		var len:Int = s.unserialize();
		var flags = [];
		for ( i in 0...len ) {
			var enumIndex:Int = s.unserialize();
			switch ( enumIndex ) {
			case Type.enumIndex(TLodBias(0)):
				var lod = s.unserialize();
				flags.push(TLodBias(lod));
			default:
				flags.push(Type.createEnumIndex(TexFlag, enumIndex));
			}
		}
		return flags;
	}
}
