package test;

class Shader extends hxsl.Shader {
	
	static var SRC = {
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
	}
	
}

class Main {

	static function main() {
		new hxsl.RuntimeCompiler();
	}
	
}