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

class Build {

	#if macro
	static function realType( t : VarType ) {
		return switch( t ) {
		case TBool: "Bool";
		case TFloat: "Float";
		case TFloat2, TFloat3, TFloat4: "flash.geom.Vector3D";
		case TInt: "Int";
		case TMatrix(_): "flash.geom.Matrix3D";
		case TTexture(cube): "flash.display3D.textures." + (cube ? "CubeTexture" : "Texture");
		case TArray(t, size): "format.hxsl.Shader.Array<" + realType(t) + ","+size+">";
		};
	}

	static function buildShaderInfos( shader : Code ) {
		var inf = {
			vars : [],
			setup : [],
		};
		var vcount = 0;
		function add(v) {
			inf.setup.push("cst[pos++] = "+v+";");
			vcount++;
		}
		for( c in shader.args.concat(shader.tex) ) {
			var t = realType(c.type);
			inf.vars.push(c.name + " : " + t);
			function addType( n : String, t : VarType )	{
				switch( t ) {
				case TBool:
					throw "assert";
				case TFloat:
					add(n);
					add(n);
					add(n);
					add(n);
				case TFloat2:
					add(n + ".x");
					add(n + ".y");
					add("1.");
					add("1.");
				case TFloat3:
					add(n + ".x");
					add(n + ".y");
					add(n + ".z");
					add("1.");
				case TFloat4:
					add(n + ".x");
					add(n + ".y");
					add(n + ".z");
					add(n + ".w");
				case TMatrix(w,h,t):
					var tmp = "raw_" + c.name;
					inf.setup.push("var " + tmp + " = " + n + ".rawData;");
					for( y in 0...w )
						for( x in 0...h ) {
							var index = if( t.t ) y + x * 4 else x + y * 4;
							add(tmp + "[" + index + "]");
						}
				case TTexture(_):
					inf.setup.push("texture(" + c.index + "," + n + ");");
				case TInt:
					add("((" + n + ">>16) & 0xFF) / 255.0");
					add("((" + n + ">>8) & 0xFF) / 255.0");
					add("(" + n + " & 0xFF) / 255.0");
					add("(" + n + ">>>24) / 255.0");
				case TArray(t, count):
					var old = vcount;
					inf.setup.push("for( _i in 0..." + count + " ) {");
					addType(n + "[_i]", t);
					inf.setup.push("}");
					vcount += (vcount - old) * (count - 1);
				}
			}
			addType("vars." + c.name, c.type);
		}
		for( c in shader.consts ) {
			for( f in c )
				add(f+"");
			for( i in c.length...4 )
				add("0");
		}

		if( vcount >> 2 >= format.agal.Tools.getProps(RConst, !shader.vertex).count )
			Context.error("This shader has reached the maximum number of allowed parameters/constants", shader.pos);

		return inf;
	}
	#end

	@:macro public static function shader() : Array<Field> {
		var cl = Context.getLocalClass().get();
		var fields = Context.getBuildFields();
		var shader = null;
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

		var p = new Parser();
		p.includeFile = function(file) {
			var f = Context.resolvePath(file);
			return Context.parse("{"+neko.io.File.getContent(f)+"}", Context.makePosition( { min : 0, max : 0, file : f } ));
		};
		var v = try p.parse(shader) catch( e : Error ) haxe.macro.Context.error(e.message, e.pos);
		var c = new Compiler();
		c.warn = Context.warning;
		var v = try c.compile(v) catch( e : Error ) haxe.macro.Context.error(e.message, e.pos);

		v = new RuntimeCompiler().compile(v, { } );

		var c = new hxsl.AgalCompiler();
		c.error = Context.error;

		var vscode = c.compile(v.vertex);
		var fscode = c.compile(v.fragment);

		var max = 200;
		if( vscode.code.length > max )
			Context.error("This vertex shader uses " + vscode.code.length + " opcodes but only " + max + " are allowed by Flash11", v.vertex.pos);
		if( fscode.code.length > max )
			Context.error("This fragment shader uses " + fscode.code.length + " opcodes but only " + max + " are allowed by Flash11", v.fragment.pos);

		var o = new haxe.io.BytesOutput();
		new format.agal.Writer(o).write(vscode);
		var vsbytes = haxe.Serializer.run(o.getBytes());

		var o = new haxe.io.BytesOutput();
		new format.agal.Writer(o).write(fscode);
		var fsbytes = haxe.Serializer.run(o.getBytes());

		var vs = buildShaderInfos(v.vertex);
		var fs = buildShaderInfos(v.fragment);

		var initCode =
			"select();\n" +
			"send(true,getVertexConstants(vertex));\n" +
			"send(false,getFragmentConstants(fragment));\n"
		;

		var bindCode =
			"bindInit(buf);\n" +
			Lambda.map(v.vars, function(v) return v.kind == VInput ? "bindReg(" + (v.type == TInt ? 0 : Tools.floatSize(v.type)) + ");\n" : "").join("") +
			"bindDone();\n"
		;

		var unbindCode = null;
		if( v.fragment.tex.length > 0 ) {
			unbindCode = "super.unbind();\n";
			for( t in v.fragment.tex )
				unbindCode += "unbindTex(" + t.index + ");\n";
		}

		#if (debug && shaderDebug)
		trace("VERTEX");
		for( o in vscode.code )
			trace(format.agal.Tools.opStr(o));
		trace("FRAGMENT");
		for( o in fscode.code )
			trace(format.agal.Tools.opStr(o));
		// trace("INIT CODE");
		// for( s in initCode.split("\n") )
		// 	trace(s);
		#end
		var decls = [
			"override function getVertexData() return format.agal.Tools.ofString('" + vsbytes + "')",
			"override function getFragmentData() return format.agal.Tools.ofString('" + fsbytes + "')",
			"public function getVertexConstants(vars:{" + vs.vars.join(",") + "}) { var cst = new flash.Vector<Float>(); var pos = 0; " + vs.setup.join("\n") + "return cst; }",
			"public function getFragmentConstants(vars:{" + fs.vars.join(",") + "}) { var cst = new flash.Vector<Float>(); var pos = 0; " + fs.setup.join("\n") + "return cst; }",
			"override function bind(buf) {"+bindCode+"}",
			"public function init( vertex : {" + vs.vars.join(",") + "}, fragment : {" + fs.vars.join(",") + "} ) {" + initCode + "}",
		];
		if( unbindCode != null )
			decls.push("override function unbind() {" + unbindCode + "}");

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