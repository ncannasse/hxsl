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

typedef Array<T,Const> = flash.Vector<T>

@:autoBuild(hxsl.Build.shader()) class Shader {

	var c : flash.display3D.Context3D;
	var p : flash.display3D.Program3D;
	var buf : flash.display3D.VertexBuffer3D;
	var regIndex : Int;
	var bufSize : Int;

	public function new(c) {
		this.c = c;
		this.p = c.createProgram();
		p.upload(getVertexData().getData(), getFragmentData().getData());
	}

	function getVertexData() : haxe.io.Bytes {
		throw "needs subclass";
		return null;
	}

	function getFragmentData() : haxe.io.Bytes {
		throw "needs subclass";
		return null;
	}

	function send(vertex:Bool,cst:flash.Vector<Float>) {
		var pt = vertex?flash.display3D.Context3DProgramType.VERTEX:flash.display3D.Context3DProgramType.FRAGMENT;
		c.setProgramConstantsFromVector(pt, 0, cst);
	}

	static var FORMATS = [
		flash.display3D.Context3DVertexBufferFormat.BYTES_4,
		flash.display3D.Context3DVertexBufferFormat.FLOAT_1,
		flash.display3D.Context3DVertexBufferFormat.FLOAT_2,
		flash.display3D.Context3DVertexBufferFormat.FLOAT_3,
		flash.display3D.Context3DVertexBufferFormat.FLOAT_4,
	];

	function bindInit(buf) {
		this.buf = buf;
		regIndex = 0;
		bufSize = 0;
	}

	function bindDone() {
		buf = null;
	}

	inline function bindReg(nfloats:Int) {
		c.setVertexBufferAt( regIndex, buf, bufSize, FORMATS[nfloats] );
		regIndex++;
		bufSize += if( nfloats == 0 ) 1 else nfloats;
	}

	public function bind(buf) {
		this.buf = buf;
		throw "needs subclass";
	}

	public function unbind() {
		while( regIndex-- > 0 )
			c.setVertexBufferAt(regIndex,null);
	}

	public function draw( vbuf, ibuf ) {
		bind(vbuf);
		c.drawTriangles(ibuf);
		unbind();
	}

	public function dispose() {
		if( p == null ) return;
		p.dispose();
		p = null;
	}

	public function select() {
		c.setProgram(p);
	}

	function texture( index : Int, t : flash.display3D.textures.TextureBase ) {
		c.setTextureAt(index, t);
	}

	inline function unbindTex(index) {
		c.setTextureAt(index, null);
	}

}