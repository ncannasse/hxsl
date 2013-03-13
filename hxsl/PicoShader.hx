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

/** Simple shader builder that simply exports information about the shader. */
@:autoBuild(hxsl.PicoBuild.shader()) class PicoShader {
	public var idata:Data;

	/** Name to type map for compile vars */
	public var compileVarInfo : Map<String,VarType>;
	/** Name to type map for input vars */
	public var inputVarInfo : Map<String,VarType>;
	/** Name to type map for vertex uniform constants */
	public var vertexConstantInfo : Map<String,VarType>;
	/** Name to type map for fragment uniform constants */
	public var fragmentConstantInfo : Map<String,VarType>;
	/** Name to type map for textures */
	public var textureInfo : Map<String,VarType>;

	public function new() {
		this.idata = Unserialize.unserialize(getData());

		compileVarInfo = createTypeMap(this.idata.globals, VConst);
		inputVarInfo = createTypeMap(this.idata.globals, VInput);
		vertexConstantInfo = createTypeMap(this.idata.vertex.args);
		fragmentConstantInfo = createTypeMap(this.idata.fragment.args);
		textureInfo = createTypeMap(this.idata.fragment.args, VTexture);
	}

	function createTypeMap(ar:Iterable<Variable>, kind:VarKind=null ) : Map<String,VarType> {
		var out = new Map();
		for ( c in ar ) {
			if ( kind == null || Type.enumEq(kind, c.kind) ) {
				out.set(c.name, c.type);
			}
		}
		return out;
	}

	public function createInstance(compileVars:Dynamic=null) : PicoShaderInstance {
		return new PicoShaderInstance(this, new RuntimeCompiler().compile(idata, compileVars));
	}

	function getData() : String {
		throw "needs subclass";
		return null;
	}
}
