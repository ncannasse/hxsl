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

#if macro
import haxe.macro.Expr;
import haxe.macro.Context;
import format.agal.Data.RegType;
import hxsl.Data;
#end

class PicoBuild {
	@:macro public static function shader() : Array<Field> {
		var cl = Context.getLocalClass().get();
		var fields = Context.getBuildFields();
		var shader = null;
		var debug = false;
		for( m in cl.meta.get() )
			if( m.name == ":shader" ) {
				if( m.params.length != 1 )
					Context.error("@:shader metadata should only have one parameter", m.pos);
				shader = m.params[0];
				break;
			}
		if( shader == null ) {
			for( f in fields )
				if( f.name == "SRC" ) {
					switch( f.kind ) {
					case FVar(_, e):
						if( e != null ) {
							shader = e;
							fields.remove(f);
							haxe.macro.Compiler.removeField(Context.getLocalClass().toString(), "SRC", true);
							break;
						}
					default:
					}
				}
		}
		if( shader == null )
			Context.error("Missing SRC shader", cl.pos);

		for ( f in fields ) {
			if ( f.name == "DEBUG" ) {
				debug = true;
				break;
			}
		}

		var p = new Parser();
		p.includeFile = function(file) {
			var f = Context.resolvePath(file);
			return Context.parse("{"+neko.io.File.getContent(f)+"}", Context.makePosition( { min : 0, max : 0, file : f } ));
		};
		var v = try p.parse(shader) catch( e : Error ) haxe.macro.Context.error(e.message, e.pos);
		var c = new Compiler();
		c.warn = Context.warning;
		var d = try c.compile(v) catch( e : Error ) haxe.macro.Context.error(e.message, e.pos);
		var s = hxsl.Serialize.serialize(d);

		var decls = [
			"override function getData() return '"+s+"'",
		];
		var e = Context.parse("{ var x : {" + decls.join("\n") + "}; }", shader.pos);
		var fdecls = switch( e.expr ) {
			case EBlock(el):
				switch( el[0].expr ) {
				case EVars(vl):
					switch( vl[0].type) {
					case TAnonymous(fl): fl;
					default: null;
					}
				default: null;
				}
			default: null;
		};
		if( fdecls == null ) throw "assert";

		return fields.concat(fdecls);
	}
}
