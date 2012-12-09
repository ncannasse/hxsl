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

class Debug {
	
	public static function dataStr( d : Data ) {
		var lines = [];
		lines.push("globals:");
		for( v in d.globals )
			lines.push("\t" + varStr(v));
		lines.push("");
		lines.push("vertex:");
		lines.push(codeStr(d.vertex));
		lines.push("fragment:");
		lines.push(codeStr(d.fragment));
		return lines.join("\n");
	}

	public static function varStr( v : Variable ) {
		return v.name+"#"+v.id+" : "+Tools.typeStr(v.type)+" = "+Std.string(v.kind).substr(1)+"#"+v.index;
	}
	
	public static function codeStr( c : Code ) {
		var lines = [];
		lines.push("args:");
		for( v in c.args )
			lines.push("\t" + varStr(v));
		if( c.consts.length > 0 ) {
			lines.push("consts:");
			for( c in c.consts )
				lines.push("\t" + Std.string(c));
		}
		lines.push("code:");
		for( e in c.exprs )
			lines.push("\t" + exprStr(e.v, e.e));
		return lines.join("\n");
	}
	
	public static function exprStr( v : CodeValue, e : CodeValue ) {
		if( v == null )
			return valueStr(e);
		return valueStr(v) +" = " + valueStr(e);
	}
	
	public static function valueStr( v : CodeValue ) {
		if( v == null )
			return "NULL";
		return switch( v.d ) {
		case CConst(c):
			switch(c) {
			case CNull: "null";
			case CInt(i): i + "i";
			case CFloat(v): v + "f";
			case CBool(b): b ? "true":"false";
			case CFloats(a): Std.string(a);
			}
		case CVar(v, swiz):
			var str;
			if( v.name.charAt(0) == "$" )
				str = "Const#" + v.index;
			else
				str = v.name + "#" + v.id;
			if( swiz != null ) {
				str += ".";
				for( s in swiz )
					str += Std.string(s).toLowerCase();
			}
			str;
		case CSwiz(v, swiz):
			var str = valueStr(v) + ".";
			if( swiz == null )
				str += "NULL";
			else
				for( s in swiz )
					str += Std.string(s).toLowerCase();
			str;
		case CVector(vals):
			"[" + Lambda.map(vals, valueStr).join(", ") + "]";
		case CUnop(op, v):
			Std.string(op).substr(1).toLowerCase() + "(" + valueStr(v) + ")";
		case CTex(v, acc, mode):
			var str = "tex(" + v.name + "#" + v.id + "[" + valueStr(acc) + "]";
			for( m in mode ) {
				str += ",";
				switch( m.f ) {
				case CTFlag(f):
					str += Std.string(f).substr(1).toLowerCase();
				case CTParam(p, v):
					str += Std.string(p).substr(1).toLowerCase() + "=" + valueStr(v);
				}
			}
			str + ")";
		case CSubBlock(tmp, v):
			var str = "{";
			for( e in tmp )
				str += exprStr(e.v, e.e) + ";";
			str += "} " + valueStr(v);
			str;
		case CRow(e1, e2):
			valueStr(e1) + "[" + valueStr(e2) + "]";
		case COp(op, e1, e2):
			function makeOp(op) {
				return "(" + valueStr(e1) + " " + op + " " + valueStr(e2) + ")";
			}
			switch( op ) {
			case CAdd: makeOp("+");
			case CSub: makeOp("-");
			case CMul: makeOp("*");
			case CDiv: makeOp("/");
			case CMod: makeOp("%");
			default:
				Std.string(op).substr(1).toLowerCase() + "(" + valueStr(e1) + "," + valueStr(e2) + ")";
			}
		case CIf(cond, eif, eelse):
			var str = "(if( " + valueStr(cond) +" ) " + blockStr(eif);
			if( eelse != null ) str += " else " + blockStr(eelse);
			str+")";
		case CFor(it, start, end, exprs):
			"for( " + it.name + "#" + it.id + " in " + valueStr(start) + "..." + valueStr(end) + " ) " + blockStr(exprs);
		case CCond(cond, eif, eelse):
			"(" + valueStr(cond) + " ? " + valueStr(eif) + " : " + valueStr(eelse) + ")";
		case CAccess(v, idx):
			"~" + v.name+"#"+v.id + "[" + valueStr(idx) + "]";
		}
	}
	
	public static function blockStr( b : Array<{ e : CodeValue, v : CodeValue }> ) {
		var str = "{";
		for( e in b )
			str += "\n\t" + exprStr(e.v, e.e);
		str += "\n}";
		return str;
	}

}