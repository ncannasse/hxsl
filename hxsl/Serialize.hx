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
class Serialize {
	
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

	var debug : Bool;
	var s : haxe.Serializer;
	var tmpVars : IntHash<Variable>;
	
	function new(debug) {
		this.debug = debug;
	}
	
	function doSerialize( data : Data ) {
		tmpVars = new IntHash();
		
		// serialize code and collect vars
		s = new haxe.Serializer();
		s.useCache = false;
		s.useEnumIndex = true;
		
		serializeCode(data.vertex);
		serializeCode(data.fragment);
		var code = s.toString();
		
		// serialize collected vars
		s = new haxe.Serializer();
		s.useCache = false;
		s.useEnumIndex = true;
		
		// header
		s.serialize(debug);
		
		// vars
		var tmpVars = Lambda.array(tmpVars);
		tmpVars.sort(function(v1, v2) return v1.id - v2.id);
		var allVars = data.vars.concat(tmpVars);
		s.serialize(allVars.length);
		for( v in allVars ) {
			s.serialize(v.id);
			s.serialize(v.refId+1);
			if( v.refId != -1 )
				s.serialize(v.index);
			serializeVarType(v.type);
			s.serialize(Type.enumIndex(v.kind));
			switch ( v.kind ) {
			case VTmp, VOut:
			default: s.serialize(v.name);
			}
			serializePosition(v.pos);
		}

		return s.toString() + "#" + code;
	}
	
	function serializePosition( p : Position ) {
		if( !debug ) return;
		var p = #if macro haxe.macro.Context.getPosInfos #end (p);
		s.serialize(p.file);
		s.serialize(p.min);
		s.serialize(p.max);
	}

	function serializeCode( code:Code ) {
		s.serialize(code.args.length);
		for( arg in code.args )
			s.serialize(arg.id);
		if( !code.vertex ) {
			s.serialize(code.tex.length);
			for ( tex in code.tex )
				s.serialize(tex.id);
		}

		s.serialize(code.exprs.length);
		for ( expr in code.exprs ) {
			serializeCodeValue(expr.v);
			serializeCodeValue(expr.e);
		}

		s.serialize(code.consts);
		serializePosition(code.pos);
	}

	function serializeCodeValue(v:CodeValue) {
		if( v == null ) {
			s.serialize(null);
			return;
		}
		switch ( v.d ) {
		case CVar(v, swiz):
			s.serialize(CVAR);
			serializeVar(v);
			serializeSwiz(swiz);
		case COp(op, e1, e2):
			s.serialize(COP);
			s.serialize(Type.enumIndex(op));
			serializeCodeValue(e1);
			serializeCodeValue(e2);
		case CUnop(op, e):
			s.serialize(CUNOP);
			s.serialize(Type.enumIndex(op));
			serializeCodeValue(e);
		case CAccess(v, idx):
			s.serialize(CACCESS);
			serializeVar(v);
			serializeCodeValue(idx);
		case CTex(v, acc, flags):
			s.serialize(CTEX);
			serializeVar(v);
			serializeTexFlags(flags);
			serializeCodeValue(acc);
		case CTexE(v, acc, flags):
			s.serialize(CTEXE);
			serializeVar(v);
			s.serialize(flags.length);
			for ( f in flags ) {
				s.serialize(Type.enumIndex(f.t));
				serializeCodeValue(f.e);
			}
			serializeCodeValue(acc);
		case CSwiz(e, swiz):
			s.serialize(CSWIZ);
			serializeSwiz(swiz);
			serializeCodeValue(e);
		case CBlock(exprs, v):
			s.serialize(CBLOCK);
			s.serialize(exprs.length);
			for ( expr in exprs ) {
				serializeCodeValue(expr.v);
				serializeCodeValue(expr.e);
			}
			serializeCodeValue(v);
		case CIf(cond, eif, eelse):
			s.serialize(CIF);
			serializeCodeValue(cond);
			s.serialize(eif.length);
			for ( expr in eif ) {
				serializeCodeValue(expr.v);
				serializeCodeValue(expr.e);
			}
			s.serialize(eelse != null ? eelse.length : 0);
			if ( eelse != null ) for ( expr in eelse ) {
				serializeCodeValue(expr.v);
				serializeCodeValue(expr.e);
			}
		case CFor(it, start, end, exprs):
			s.serialize(CFOR);
			s.serialize(it.id);
			serializeCodeValue(start);
			serializeCodeValue(end);
			s.serialize(exprs.length);
			for ( expr in exprs ) {
				serializeCodeValue(expr.v);
				serializeCodeValue(expr.e);
			}
		case CVector(values):
			s.serialize(CVECTOR);
			s.serialize(values.length);
			for ( v in values ) serializeCodeValue(v);
		case CLiteral(value):
			s.serialize(CLITERAL);
			s.serialize(value);
		}

		serializeVarType(v.t);
		serializePosition(v.p);
	}

	function serializeVar( v : Variable ) {
		s.serialize(v.id);
		if( v.kind == VTmp )
			tmpVars.set(v.id, v);
	}
	
	function serializeVarType(type:VarType) {
		s.serialize(Type.enumIndex(type));
		switch ( type ) {
		case TMatrix(r, c, t):
			s.serialize(r);
			s.serialize(c);
			s.serialize(t.t);
		case TTexture(cube):
			s.serialize(cube);
		case TArray(atype, size):
			serializeVarType(atype);
			s.serialize(size);
		default:
		}
	}

	function serializeSwiz( swiz:Array<Comp> ) {
		if ( swiz != null ) {
			s.serialize(swiz.length);
			for ( c in swiz ) s.serialize(Type.enumIndex(c));
		} else {
			s.serialize(0);
		}
	}

	function serializeTexFlags( flags:Array<TexFlag> ) : Void {
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
	
	public static function serialize( data : Data, ?debug = #if debug true #else false #end  ) : String {
		return new Serialize(debug).doSerialize(data);
	}
	
}
