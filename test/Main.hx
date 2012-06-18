package test;
import haxe.macro.Expr;
import hxsl.Unserialize;
import hxsl.Serialize;
#if macro
import haxe.macro.Context;
#end

class Main {

	#if macro
	static function agalToString( c : format.agal.Data ) {
		var lines = [];
		for( o in c.code )
			lines.push(format.agal.Tools.opStr(o));
		return lines.join("\n");
	}
	
	static function agalToBytes( c : format.agal.Data ) {
		var o = new haxe.io.BytesOutput();
		new format.agal.Writer(o).write(c);
		return o.getBytes();
	}
	
	static function compileShader( shader : Expr, params : {} ) {
		var p = new hxsl.Parser().parse(shader);
		var data = new hxsl.Compiler().compile(p);
		data = new hxsl.RuntimeCompiler().compile(data, params);
		var vert = new hxsl.AgalCompiler().compile(data.vertex);
		var frag = new hxsl.AgalCompiler().compile(data.fragment);
		var vexpr = { expr : EConst(CString(haxe.Serializer.run(agalToBytes(vert)))), pos : shader.pos };
		var fexpr = { expr : EConst(CString(haxe.Serializer.run(agalToBytes(frag)))), pos : shader.pos };
		var chk = macro testRuntimeShader($vexpr,$fexpr);
		return { str : agalToString(vert) + "\n\n" + agalToString(frag), chk : chk };
	}
	#end
	
	@:macro static function test( shader : Expr, out : String, ?params : { } ) {
		var s = null;
		try {
			s = compileShader(shader, params);
		} catch( e : hxsl.Data.Error ) {
			Context.error(e.message, e.pos);
		}
		var out = StringTools.trim(out.split("\r\n").join("\n").split("\t").join(""));
		if( s.str != out )
			Context.error("Wrong AGAL output :\n" + s.str, shader.pos);
		return s.chk;
	}
	
	@:macro static function error( shader : Expr, msg : String, ?params : { } ) {
		try {
			compileShader(shader, params);
			Context.error("Shader compilation should give an error", shader.pos);
		} catch( e : hxsl.Data.Error ) {
			if( e.message.indexOf(msg) < 0 ) Context.error("Unexpected error : " + e.message, e.pos);
		}
		return { expr : EBlock([]), pos : shader.pos };
	}
	
	#if !macro

	static function stringToBytes( str : String ) {
		var bytes : haxe.io.Bytes = haxe.Unserializer.run(str);
		var bytes = bytes.getData();
		bytes.endian = flash.utils.Endian.LITTLE_ENDIAN;
		return bytes;
	}
	
	static function testRuntimeShader( vertex : String, fragment : String ) {
		var p = ctx.createProgram();
		p.upload(stringToBytes(vertex), stringToBytes(fragment));
		p.dispose();
	}

	static function checkShaders() {
		
		
		// basic shader
		test({
			var input : {
				pos : Float3,
				uv : Float2,
			};
			var tuv : Float2;
			function vertex( mproj : Matrix ) {
				var tmp = pos.xyzw * mproj;
				out = tmp;
				tuv = uv;
			}
			function fragment( tex : Texture ) {
				out = tex.get(tuv);
			}
		},"
			m44 t0, a0.xyzw, c0
			mov out, t0
			mov v0, a1

			tex t0, tex0[v0.xy]
			mov out, t0
		");
		
		// check that we can read all from a varying
		test( {
			var input : {
				pos : Float3,
			};
			var color : Float3;
			function vertex( mpos : M44, mproj : M44 ) {
				out = pos.xyzw * mpos * mproj;
				color = pos;
			}
			function fragment() {
				out = color.xyzw;
			}
		},"
			m44 t0, a0.xyzw, c0
			dp4 out.x, t0, c4
			dp4 out.y, t0, c5
			dp4 out.z, t0, c6
			dp4 out.w, t0, c7
			mov v0, a0
			
			mov out, v0.xyzw
		");
		
		// check that we correcly perform additional reads on inputs/textures
		// even if runtime shader skip some reads
		test( {
			var input : {
				pos : Float3,
				n : Float3,
			};
			var flag : Bool;
			function vertex() {
				out = pos.xyzw + (flag ? n.xyzw : [0,0,0,0]);
			}
			function fragment( tex : Texture ) {
				out = (flag ? tex.get(pos.xy) : [0,0,0,0]);
			}
		},"
			mov out, pos
			
			mov out, c0
		");
		
		
		error({
		},"Missing vertex function");
		
		error( {
			function vertex() {
			}
		},"Missing fragment function");

		error( {
			function vertex() {
			}
			function fragment() {
			}
		},"Missing input variable");
	
		error( {
			var input : {
				pos : Float3,
			};
			function vertex() {
			}
			function fragment() {
			}
		},"Output is not written by vertex shader");
		
		error( {
			var input : {
				pos : Float3,
			};
			function vertex() {
				out = pos.xyzw;
			}
			function fragment() {
			}
		},"Output is not written by fragment shader");

		error( {
			var input : {
				pos : Float3,
			};
			function vertex() {
				out = xpos.xyzw;
			}
			function fragment() {
			}
		},"Unknown variable 'xpos'");
				
	}
	
	static var ctx : flash.display3D.Context3D;
		
	static function main() {
		var stage = flash.Lib.current.stage.stage3Ds[0];
		stage.addEventListener(flash.events.Event.CONTEXT3D_CREATE, function(_) {
			ctx = stage.context3D;
			checkShaders();
		});
		stage.requestContext3D();
	}
	#end
	
}