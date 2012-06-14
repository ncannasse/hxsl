package test;
import haxe.macro.Expr;
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
	
	static function compileShader( shader : Expr, params : {} ) {
		var p = new hxsl.Parser().parse(shader);
		var data = new hxsl.Compiler().compile(p);
		data = new hxsl.RuntimeCompiler().compile(data, params);
		var vert = new hxsl.AgalCompiler().compile(data.vertex);
		var frag = new hxsl.AgalCompiler().compile(data.fragment);
		return agalToString(vert) + "\n\n" + agalToString(frag);
	}
	#end
	
	@:macro static function test( shader : Expr, out : String, ?params : { } ) {
		var str = null;
		try {
			str = compileShader(shader, params);
		} catch( e : hxsl.Data.Error ) {
			Context.error(e.message, e.pos);
		}
		var out = StringTools.trim(out.split("\r\n").join("\n").split("\t").join(""));
		if( str != out )
			Context.error("Wrong AGAL output :\n" + str, shader.pos);
		return { expr : EBlock([]), pos : shader.pos };
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
	static function main() {
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
		
		test( {
			var input : {
				pos : Float3,
			};
			var color : Float4;
			function vertex( mpos : M44, mproj : M44 ) {
				out = pos.xyzw * mpos * mproj;
				color = pos.xyzw;
			}
			function fragment() {
				out = color;
			}
		},"
			m44 t0, a0.xyzw, c0
			dp4 out.x, t0, c4
			dp4 out.y, t0, c5
			dp4 out.z, t0, c6
			dp4 out.w, t0, c7
			mov v0, a0.xyzw
			
			mov out, v0
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
	#end
	
}