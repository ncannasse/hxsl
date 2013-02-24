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

enum ProgramType { Vertex; Fragment; }

/**
 *  Represents a shader, compiled to the platform-specific format.
 *  Simply provides the compiled code and reflection info.
 */
class PicoShaderInstance
{
	public function new(shader:PicoShader, data:Data) {
		var c = new hxsl.AgalCompiler();
		var vscode = c.compile(data.vertex);
		var fscode = c.compile(data.fragment);

		var max = 200;
		if( vscode.code.length > max )
			throw "This vertex shader uses " + vscode.code.length + " opcodes but only " + max + " are allowed by Flash11";
		if( fscode.code.length > max )
			throw "This fragment shader uses " + fscode.code.length + " opcodes but only " + max + " are allowed by Flash11";

		#if (debug && shaderDebug)
		trace("VERTEX");
		for( o in vscode.code )
			trace(format.agal.Tools.opStr(o));
		trace("FRAGMENT");
		for( o in fscode.code )
			trace(format.agal.Tools.opStr(o));
		#end

		var vsbytes = new haxe.io.BytesOutput();
		new format.agal.Writer(vsbytes).write(vscode);
		var fsbytes = new haxe.io.BytesOutput();
		new format.agal.Writer(fsbytes).write(fscode);

		this.vertexCode = vsbytes.getBytes().getData();
		this.fragmentCode = fsbytes.getBytes().getData();

		this.vertexLiterals = data.vertex.consts;
		this.fragmentLiterals = data.fragment.consts;

		this.inputRegisters = new Map();
		for ( v in data.globals ) {
			if ( v.kind == VInput ) {
				this.inputRegisters.set(v.name, v.index);
			}
		}

		this.vertexLiteralStart = 0;
		this.vertexParamRegisters = new Map();
		for ( arg in data.vertex.args ) {
			this.vertexParamRegisters.set(arg.name, arg.index);
			this.vertexLiteralStart = arg.index + Tools.regSize(arg.type);
		}

		this.fragmentLiteralStart = 0;
		this.fragmentParamRegisters = new Map();
		this.textureRegisters = new Map();
		for ( arg in data.fragment.args ) {
			switch( arg.kind ) {
			case VTexture:
				this.textureRegisters.set(arg.name, arg.index);
			default:
				this.fragmentParamRegisters.set(arg.name, arg.index);
				this.fragmentLiteralStart = arg.index + Tools.regSize(arg.type);
			}
		}

	}

	/** The shader this instance was created from */
	public var shader : PicoShader;
	/** Platform-specific vertex code */
	public var vertexCode : Dynamic;
	/** Platform-specific fragment code */
	public var fragmentCode : Dynamic;
	/** Map input name to register */
	public var inputRegisters:Map<String,Int>;
	/** Map vertex shader parameter name to register */
	public var vertexParamRegisters:Map<String,Int>;
	/** Map fragment shader parameter name to register */
	public var fragmentParamRegisters:Map<String,Int>;
	/** Map texture name to register */
	public var textureRegisters:Map<String,Int>;
	/** Literals to apply to vertex shader */
	public var vertexLiterals:Array<Array<Float>>;
	/** Literals to apply to fragment shader */
	public var fragmentLiterals:Array<Array<Float>>;
	/** Register that vertex shader literals should be uploaded to */
	public var vertexLiteralStart:Int;
	/** Register that fragment shader literals should be uploaded to */
	public var fragmentLiteralStart:Int;
}
