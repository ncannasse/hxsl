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
	
	var debug : Bool;
	var s : haxe.Serializer;
	var savedVars : Map<Int,Bool>;
	
	function new(debug) {
		savedVars = new Map();
		this.debug = debug;
	}
	
	function doSerialize( data : Data ) {
		s = new haxe.Serializer();
		s.useCache = false;
		s.useEnumIndex = true;
		s.serialize(debug);
		s.serialize(data.globals.length);
		for( v in data.globals )
			serializeVar(v);
		serializeCode(data.vertex);
		serializeCode(data.fragment);
		return s.toString();
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
			serializeVar(arg);

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
		s.serialize(Type.enumIndex(v.d));
		switch ( v.d ) {
		case CConst(c):
			s.serialize(Type.enumIndex(c));
			switch( c ) {
			case CNull:
			case CBool(b): s.serialize(b);
			case CInt(i): s.serialize(i);
			case CFloat(f): s.serialize(f);
			case CFloats(a): s.serialize(a);
			case CObject(fields): s.serialize(fields);
			case CArray(ar): s.serialize(ar);
			}
		case CVar(v, swiz):
			serializeVar(v);
			serializeSwiz(swiz);
		case COp(op, e1, e2):
			s.serialize(Type.enumIndex(op));
			serializeCodeValue(e1);
			serializeCodeValue(e2);
		case CUnop(op, e):
			s.serialize(Type.enumIndex(op));
			serializeCodeValue(e);
		case CAccess(v, idx):
			serializeVar(v);
			serializeCodeValue(idx);
		case CTex(v, acc, flags):
			serializeVar(v);
			serializeCodeValue(acc);
			serializeTexFlags(flags);
		case CSwiz(e, swiz):
			serializeCodeValue(e);
			serializeSwiz(swiz);
		case CSubBlock(tmp, v):
			s.serialize(tmp.length);
			for( expr in tmp ) {
				serializeCodeValue(expr.v);
				serializeCodeValue(expr.e);
			}
			serializeCodeValue(v);
		case CIf(cond, eif, eelse):
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
		case CFor(v, it, exprs):
			serializeVar(v);
			serializeCodeValue(it);
			s.serialize(exprs.length);
			for ( expr in exprs ) {
				serializeCodeValue(expr.v);
				serializeCodeValue(expr.e);
			}
		case CVector(values):
			s.serialize(values.length);
			for ( v in values ) serializeCodeValue(v);
		case CCond(c,e1,e2):
			serializeCodeValue(c);
			serializeCodeValue(e1);
			serializeCodeValue(e2);
		case CRow(e1, e2):
			serializeCodeValue(e1);
			serializeCodeValue(e2);
		case CField(e, f):
			serializeCodeValue(e);
			s.serialize(f);
		}

		serializeVarType(v.t);
		serializePosition(v.p);
	}

	function serializeVar( v : Variable ) {
		s.serialize(v.id);
		if( savedVars.exists(v.id) )
			return;
		savedVars.set(v.id, true);
		serializeVarType(v.type);
		s.serialize(Type.enumIndex(v.kind));
		switch ( v.kind ) {
		case VTmp, VOut:
		default: s.serialize(v.name);
		}
		serializePosition(v.pos);
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
		case TObject(fields):
			s.serialize(fields.length);
			for( f in fields ) {
				s.serialize(f.name);
				serializeVarType(f.t);
			}
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

	function serializeTexFlags( flags:Array<{ f : CodeTexFlag, p : Position }> ) : Void {
		s.serialize(flags.length);
		for( c in flags ) {
			switch( c.f ) {
			case CTFlag(f):
				s.serialize(false);
				s.serialize(Type.enumIndex(f));
			case CTParam(p,v):
				s.serialize(true);
				s.serialize(Type.enumIndex(p));
				serializeCodeValue(v);
			}
			serializePosition(c.p);
		}
	}
	
	public static function serialize( data : Data, ?debug = #if debug true #else false #end  ) : String {
		return new Serialize(debug).doSerialize(data);
	}
	
}
