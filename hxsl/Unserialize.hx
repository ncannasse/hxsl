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

class Unserialize {

	var s : haxe.Unserializer;
	var vars : IntHash<Variable>;
	var debug : Bool;
	
	function new(str) {
		s = new haxe.Unserializer(str);
		vars = new IntHash();
	}
	
	public static function unserialize( s : String ) : Data {
		return new Unserialize(s).doUnserialize();
	}
	
	function doUnserialize() {
		debug = s.unserialize();
		var globals = [];
		for( i in 0...s.unserialize() )
			globals.push(unserializeVar());
		var vertex = unserializeCode(true);
		var fragment = unserializeCode(false);
		return { globals : globals, vertex:vertex, fragment:fragment };
	}

	function unserializeCode( vertex:Bool ) : Code {
		var args = [];
		for( i in 0...s.unserialize() )
			args.push( unserializeVar() );
		var exprs = [];
		for( i in 0...s.unserialize() )
			exprs.push( { v:unserializeCodeValue(), e:unserializeCodeValue() } );
		var consts:Array<Array<Float>> = s.unserialize();
		var pos = unserializePos();
		return { pos:pos, args:args, exprs:exprs, consts:consts, vertex:vertex, tempSize:0 };
	}

	function unserializeVar() : Variable {
		var id : Int = s.unserialize();
		var v = vars.get(id);
		if( v != null ) return v;
		var type = unserializeVarType();
		var kind = Type.createEnumIndex(VarKind,s.unserialize());
		var name = switch ( kind ) {
			case VTmp, VOut: "";
			default: s.unserialize();
		}
		var pos = unserializePos();
		var v:Variable = {
			id:id,
			name:name,
			type:type,
			kind:kind,
			pos:pos,
			index:0,
		};
		vars.set(v.id, v);
		return v;
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
			var acc = unserializeCodeValue();
			var flags = unserializeTexFlags();
			CTex(v, acc, flags);
		case 5:
			var e = unserializeCodeValue();
			var swiz = unserializeSwiz();
			CSwiz(e, swiz);
		case 6:
			var exprs = [];
			for( i in 0...s.unserialize() )
				exprs.push( {
					v : unserializeCodeValue(),
					e : unserializeCodeValue(),
				});
			CSubBlock(exprs, unserializeCodeValue());
		case 7:
			CConst(switch( s.unserialize() ) {
			case 0: CNull;
			case 1: CInt(s.unserialize());
			case 2: CBool(s.unserialize());
			case 3: CFloat(s.unserialize());
			case 4: CFloats(s.unserialize());
			default:
				throw "assert";
			});
		case 8:
			var cond = unserializeCodeValue();
			var eifLen:Int = s.unserialize();
			var eif = [];
			for( i in 0...eifLen ) {
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
		case 9:
			var cond = unserializeCodeValue();
			var e1 = unserializeCodeValue();
			var e2 = unserializeCodeValue();
			CCond(cond, e1, e2);
		case 10:
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
		case 11:
			var numVals = s.unserialize();
			var vals = [];
			for ( i in 0...numVals ) vals.push(unserializeCodeValue());
			CVector(vals);
		case 12:
			var e1 = unserializeCodeValue();
			var e2 = unserializeCodeValue();
			CRow(e1, e2);
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
		case Type.enumIndex(TMatrix(0,0,null)):
			var r:Int = s.unserialize();
			var c:Int = s.unserialize();
			var transpose = s.unserialize();
			return TMatrix(r,c,{t:transpose});
		case Type.enumIndex(TTexture(true)):
			var cube = s.unserialize();
			return TTexture(cube);
		case Type.enumIndex(TArray(null,0)):
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
		for( i in 0...len ) {
			var f = if( !s.unserialize() ) {
				CTFlag(Type.createEnumIndex(TexFlag, s.unserialize()));
			} else {
				var p = Type.createEnumIndex(TexParam, s.unserialize());
				CTParam(p, unserializeCodeValue());
			}
			flags.push( { f : f, p : unserializePos() } );
		}
		return flags;
	}

}