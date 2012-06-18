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

private class CustomU extends haxe.Unserializer {
	public function resetCache() {
		cache = [];
		scache = [];
		pos++; // skip #
	}
}

class Unserialize {

	var s : CustomU;
	var vars : IntHash<Variable>;
	var debug : Bool;
	
	function new(str) {
		s = new CustomU(str);
		vars = new IntHash();
	}
	
	public static function unserialize( s : String ) : Data {
		return new Unserialize(s).doUnserialize();
	}
	
	function doUnserialize() {
		debug = s.unserialize();
		var numVars = s.unserialize();
		var allVars = [];
		var varKinds = Type.allEnums(VarKind);
		for ( i in 0...numVars ) {
			var id:Int = s.unserialize();
			var refId:Int = s.unserialize();
			var index = 0;
			if( refId != 0 )
				index = s.unserialize();
			var type = unserializeVarType();
			var kind = varKinds[s.unserialize()];
			var name = switch ( kind ) {
				case VTmp, VOut: "";
				default: s.unserialize();
			}
			var pos = unserializePos();
			var v:Variable = {
				name:name,
				id:id,
				refId:refId-1,
				index:index,
				type:type,
				kind:kind,
				pos:pos,
				read:false,
				write:0,
				assign:null
			};
			vars.set(id, v);
			if( v.kind != VTmp )
				allVars.push(v);
		}
		
		s.resetCache();
		
		var vertex = unserializeCode(true);
		var fragment = unserializeCode(false);

		return { vars : allVars, vertex:vertex, fragment:fragment };
	}

	function unserializeCode( vertex:Bool ) : Code {
		var numArgs = s.unserialize();
		var args = [];
		for ( i in 0...numArgs )
			args.push( unserializeVar() );
		var tex = [];
		if ( !vertex ) {
			var numTex = s.unserialize();
			for ( i in 0...numTex )
				tex.push( unserializeVar() );
		}

		var numExprs = s.unserialize();
		var exprs = [];
		for ( i in 0...numExprs )
			exprs.push( {v:unserializeCodeValue(), e:unserializeCodeValue()} );

		var consts:Array<Array<Float>> = s.unserialize();
		var pos = unserializePos();
		return { pos:pos, args:args, tex:tex, exprs:exprs, consts:consts, vertex:vertex, tempSize:0 };
	}

	inline function unserializeVar() {
		return vars.get(s.unserialize());
	}
	
	function unserializeCodeValue() : CodeValue {
		var dindex:Null<Int> = s.unserialize();
		if( dindex == null )
			return null;
		var decl = switch ( dindex ) {
		case 0:
			var v = unserializeVar();
			var swiz = unserializeSwiz();
			CVar(v, swiz);
		case 1:
			var op = Type.createEnumIndex(CodeOp, s.unserialize());
			var e1 = unserializeCodeValue();
			var e2 = unserializeCodeValue();
			COp(op, e1, e2);
		case 2:
			var op = Type.createEnumIndex(CodeUnop, s.unserialize());
			var e = unserializeCodeValue();
			CUnop(op, e);
		case 3:
			var v = unserializeVar();
			var idx = unserializeCodeValue();
			CAccess(v, idx);
		case 4:
			var v = unserializeVar();
			var flags = unserializeTexFlags();
			var acc = unserializeCodeValue();
			CTex(v, acc, flags);
		case 5:
			var swiz = unserializeSwiz();
			var e = unserializeCodeValue();
			CSwiz(e, swiz);
		case 6:
			var numExprs:Int = s.unserialize();
			var exprs = [];
			for ( i in 0...numExprs ) {
				var v = unserializeCodeValue();
				var e = unserializeCodeValue();
				exprs.push( {v : v, e : e} );
			}
			var v = unserializeCodeValue();
			CBlock(exprs, v);
		case 7:
			var cond = unserializeCodeValue();
			var eifLen:Int = s.unserialize();
			var eif = [];
			for ( i in 0...eifLen ) {
				var v = unserializeCodeValue();
				var e = unserializeCodeValue();
				eif.push({ v : v, e : e });
			}
			var eelseLen:Int = s.unserialize();
			var eelse = null;
			if ( eelseLen != 0 ) {
				eelse = [];
				for ( i in 0...eelseLen ) {
					var v = unserializeCodeValue();
					var e = unserializeCodeValue();
					eelse.push({ v : v, e : e });
				}
			}
			CIf(cond, eif, eelse);
		case 8:
			var value = s.unserialize();
			CLiteral(value);
		case 9:
			var it = unserializeVar();
			var start = unserializeCodeValue();
			var end = unserializeCodeValue();
			var numExprs:Int = s.unserialize();
			var exprs = [];
			for ( i in 0...numExprs ) {
				var v = unserializeCodeValue();
				var e = unserializeCodeValue();
				exprs.push( {v : v, e : e} );
			}
			CFor(it, start, end, exprs);
		case 10:
			var v = unserializeVar();
			var numFlags:Int = s.unserialize();
			var flags = [];
			for ( i in 0...numFlags ) {
				var t = Type.createEnumIndex(TexParam, s.unserialize());
				var e = unserializeCodeValue();
				flags.push({t:t, e:e});
			}
			var acc = unserializeCodeValue();
			CTexE(v, acc, flags);
		case 11:
			var numVals = s.unserialize();
			var vals = [];
			for ( i in 0...numVals ) vals.push(unserializeCodeValue());
			CVector(vals);
		default:
			throw "assert";
		}

		var type = unserializeVarType();
		var pos = unserializePos();
		return {d:decl, t:type, p:pos};
	}

	function unserializePos() {
		if( !debug )
			return null;
		var file:String = s.unserialize();
		var min:Int = s.unserialize();
		var max:Int = s.unserialize();
		return #if macro haxe.macro.Context.makePosition #end ({file:file, min:min, max:max});
	}

	function unserializeVarType() {
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
			var type = unserializeVarType();
			var size = s.unserialize();
			return TArray(type, size);
		default:
			return Type.createEnumIndex(VarType, typeIndex);
		}
	}

	function unserializeSwiz() {
		var len:Int = s.unserialize();
		var swiz = null;
		if( len != 0 ) {
			swiz = [];
			for ( i in 0...len ) swiz.push(Type.createEnumIndex(Comp, s.unserialize()));
		}
		return swiz;
	}

	function unserializeTexFlags() {
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