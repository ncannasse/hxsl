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

/**
	A ShaderInstance is a compiled version of a shader for a given set of parameters.
**/
class ShaderInstance {

	public var bits : Int;
	
	public var program : flash.display3D.Program3D;

	public var bufferFormat : Int;
	public var bufferNames : Array<String>;
	public var stride : Int;

	public var vertexVars : flash.Vector<Float>;
	public var fragmentVars : flash.Vector<Float>;
	public var textures : flash.Vector<ShaderTypes.Texture>;
	public var curShaderId : Int;

	public var vertexMap : flash.Vector<Int>;
	public var fragmentMap : flash.Vector<Int>;
	public var textureMap : flash.Vector<Int>;
	
	public var vertexBytes : haxe.io.Bytes;
	public var fragmentBytes : haxe.io.Bytes;
	
	public var varsChanged : Bool;
	
	public function new() {
		curShaderId = -1;
	}
	
}

/**
	ShaderGlobals is shared among all Shader instances of the same class.
	It stores all the ShaderInstance depending on applied parameters
**/
class ShaderGlobals {
	
	public var data : hxsl.Data;
	public var texSize : Int;
	public var hasParamVector : Bool;
	public var hasParamMatrix : Bool;

	var constCount : Int;
	var instances : IntHash<ShaderInstance>;
	var hparams : IntHash<hxsl.Data.Variable>;
	
	public function new( hxStr : String ) {
		this.data = hxsl.Unserialize.unserialize(hxStr);
		
		function checkType(t:Data.VarType) {
			switch( t ) {
			case TNull, TFloat, TBool, TInt, TTexture(_):
			case TFloat2, TFloat3, TFloat4: hasParamVector = true;
			case TArray(t, _): checkType(t);
			case TMatrix(_): hasParamMatrix = true;
			}
		}
		hparams = new IntHash();
		for( v in Tools.getAllVars(data) )
			switch( v.kind ) {
			case VParam:
				v.index = constCount++;
				checkType(v.type);
				hparams.set(v.id, v);
			case VConst:
				v.index = constCount++;
				hparams.set(v.id, v);
			case VTexture:
				v.index = texSize++;
				hparams.set(v.id, v);
			default:
			}
		
		instances = new IntHash();
	}
	
	function build( code : hxsl.Data.Code ) {
			
		// init map
		var nregs = 0;
		var map = new flash.Vector(constCount);
		for( i in 0...constCount )
			map[i] = -1;
			
		for( v in code.args ) {
			var realV = hparams.get(v.id);
			if( v == null ) throw "assert " + v.name;
			map[realV.index] = v.index * 4;
			nregs += Tools.regSize(v.type);
		}
		
		// add consts
		var pos = nregs * 4;
		nregs += code.consts.length;
		var consts = new flash.Vector(nregs * 4);
		for( c in code.consts ) {
			for( v in c )
				consts[pos++] = v;
			for( i in c.length...4 )
				consts[pos++] = 1.;
		}
		
		var agal = new hxsl.AgalCompiler().compile(code);
		var o = new haxe.io.BytesOutput();
		new format.agal.Writer(o).write(agal);
		return { bytes : o.getBytes(), consts : consts, map : map };
	}
	
	public function getInstance( bits : Int ) {
		var i = instances.get(bits);
		if( i != null )
			return i;
		var r = new hxsl.RuntimeCompiler();
		var params = { };
		var paramCount = 0;
		for( v in Tools.getAllVars(data) )
			if( v.kind == VParam ) {
				if( bits & (1 << paramCount) != 0 ) {
					var c : Dynamic;
					if( v.type == TBool ) c = true else c = 0;
					Reflect.setField(params, v.name, c);
				}
				paramCount++;
			}
		
		var data2 = r.compile(data, params);
		i = new ShaderInstance();
		i.bits = bits;
		
		var v = build(data2.vertex);
		i.vertexBytes = v.bytes;
		i.vertexMap = v.map;
		i.vertexVars = v.consts;
		
		var f = build(data2.fragment);
		i.fragmentBytes = f.bytes;
		i.fragmentMap = f.map;
		i.fragmentVars = f.consts;
				
		i.textureMap = new flash.Vector();
		for( v in data2.vertex.args.concat(data2.fragment.args) )
			if( v.kind == VTexture ) {
				var realV = hparams.get(v.id);
				i.textureMap.push(realV.index);
			}
		i.textures = new flash.Vector(i.textureMap.length);
		
		i.bufferFormat = 0;
		i.bufferNames = [];
		i.stride = 0;
		for( v in data2.globals )
			switch( v.kind ) {
			case VInput:
				var size = Tools.floatSize(v.type);
				switch( v.type ) {
				case TInt:
					// 0
				case TFloat, TFloat2, TFloat3, TFloat4:
					i.bufferFormat |= Tools.floatSize(v.type) << (3 * v.index);
				default:
					throw "Type not supported in input " + Type.enumConstructor(v.type).substr(1);
				}
				i.bufferNames.push(v.name);
				i.stride += size;
			case VVar:
				// ignore
			default:
				throw "assert " + v.kind;
			}

		instances.set(bits, i);
		
		return i;
	}
	
}


@:autoBuild(hxsl.ShaderMacros.buildShader())
class Shader {

	static var ID = 0;
	
	var shaderId : Int;
	var globals : ShaderGlobals;
	var modified : Bool;
	var paramBits : Int;
	var paramVectors : Array<ShaderTypes.Vector>;
	var paramMatrixes : Array<ShaderTypes.Matrix>;
	var allTextures : Array<ShaderTypes.Texture>;
	var instance : ShaderInstance;
	
	function new() {
		var c : { HXSL : String, GLOBALS : ShaderGlobals } = cast Type.getClass(this);
		globals = c.GLOBALS;
		if( globals == null ) {
			globals = new ShaderGlobals(c.HXSL);
			c.GLOBALS = globals;
		}
		if( globals.texSize > 0 ) allTextures = [];
		if( globals.hasParamVector ) paramVectors = [];
		if( globals.hasParamMatrix ) paramMatrixes = [];
		shaderId = ID++;
	}
	
	inline function makeMatrix() {
		var m = new ShaderTypes.Matrix();
		#if h3d m.identity(); #end
		return m;
	}
	
	inline function makeVector() {
		return new ShaderTypes.Vector();
	}

	inline function makeArray(size) {
		return new ShaderTypes.FixedArray(size);
	}

	public function getInstance() : ShaderInstance {
		if( instance == null || instance.bits != paramBits )
			instance = globals.getInstance(paramBits);
		if( modified || instance.curShaderId != shaderId ) {
			updateParams();
			modified = false;
		}
		return instance;
	}

	#if !h3d
	
	public function bind( ctx : flash.display3D.Context3D, buffer : flash.display3D.VertexBuffer3D ) {
		var i = getInstance();
		if( i.program == null ) {
			var vdata = i.vertexBytes.getData();
			var fdata = i.fragmentBytes.getData();
			vdata.endian = flash.utils.Endian.LITTLE_ENDIAN;
			fdata.endian = flash.utils.Endian.LITTLE_ENDIAN;
			i.program = ctx.createProgram();
			i.program.upload(vdata,fdata);
		}
		ctx.setProgram(i.program);
		ctx.setProgramConstantsFromVector(flash.display3D.Context3DProgramType.VERTEX, 0, i.vertexVars);
		ctx.setProgramConstantsFromVector(flash.display3D.Context3DProgramType.FRAGMENT, 0, i.fragmentVars);
		for( k in 0...i.textures.length )
			ctx.setTextureAt(k, i.textures[k]);
		var FORMAT = [
			flash.display3D.Context3DVertexBufferFormat.BYTES_4,
			flash.display3D.Context3DVertexBufferFormat.FLOAT_1,
			flash.display3D.Context3DVertexBufferFormat.FLOAT_2,
			flash.display3D.Context3DVertexBufferFormat.FLOAT_3,
			flash.display3D.Context3DVertexBufferFormat.FLOAT_4,
		];
		var pos = 0, offset = 0;
		var bits = i.bufferFormat;
		while( offset < i.stride ) {
			var size = bits & 7;
			ctx.setVertexBufferAt(pos++, buffer, offset, FORMAT[size]);
			offset += size == 0 ? 1 : size;
			bits >>= 3;
		}
	}
	
	public function unbind( ctx : flash.display3D.Context3D ) {
		var i = instance;
		if( i == null )
			return;
		for( k in 0...i.textures.length )
			ctx.setTextureAt(k, i.textures[k]);
		var pos = 0, offset = 0;
		var bits = i.bufferFormat;
		while( offset < i.stride ) {
			var size = bits & 7;
			ctx.setVertexBufferAt(pos++, null);
			offset += size == 0 ? 1 : size;
			bits >>= 3;
		}
	}
	
	#end
	
	function updateParams() {
		// copy vars from our local shader to the instance
		updateVertexParams(instance.vertexVars, instance.vertexMap);
		updateFragmentParams(instance.fragmentVars, instance.fragmentMap);
		for( i in 0...instance.textureMap.length )
			instance.textures[i] = allTextures[instance.textureMap[i]];
		instance.curShaderId = shaderId;
		instance.varsChanged = true;
	}
	
	function updateVertexParams( params : flash.Vector<Float>, map : flash.Vector<Int> ) {
	}

	function updateFragmentParams( params : flash.Vector<Float>, map : flash.Vector<Int> ) {
	}
	
	inline function saveFloats( params : flash.Vector<Float>, index : Int, v : ShaderTypes.Vector, n : Int ) {
		if( index >= 0 ) {
			params[index] = v.x;
			params[index + 1] = v.y;
			if( n > 2 ) params[index + 2] = v.z;
			if( n > 3 ) params[index + 3] = v.w;
		}
	}
	
	inline function saveInt( params : flash.Vector<Float>, index : Int, v : Int ) {
		if( index >= 0 ) {
			params[index] = ((v >> 16) & 0xFF) / 255;
			params[index + 1] = ((v >> 8) & 0xFF) / 255;
			params[index + 2] = (v & 0xFF) / 255;
			params[index + 3] = (v >>> 24) / 255;
		}
	}
	
	inline function saveFloat( params : flash.Vector<Float>, index : Int, v : Float ) {
		if( index >= 0 ) params[index] = v;
	}

	inline function saveMatrix( params : flash.Vector<Float>, index : Int, m : ShaderTypes.Matrix, r : Int, c : Int ) {
		if( index >= 0 ) {
			#if h3d
			params[index++] = m._11;
			if( c > 1 ) params[index++] = m._12;
			if( c > 2 ) params[index++] = m._13;
			if( c > 3 ) params[index++] = m._14;
			
			if( r > 1 ) {
				params[index++] = m._21;
				if( c > 1 ) params[index++] = m._22;
				if( c > 2 ) params[index++] = m._23;
				if( c > 3 ) params[index++] = m._24;

				if( r > 2 ) {
					params[index++] = m._31;
					if( c > 1 ) params[index++] = m._32;
					if( c > 2 ) params[index++] = m._33;
					if( c > 3 ) params[index++] = m._34;

					if( r > 3 ) {
						params[index++] = m._41;
						if( c > 1 ) params[index++] = m._42;
						if( c > 2 ) params[index++] = m._43;
						if( c > 3 ) params[index++] = m._44;
					}
				}
			}
			#else
			var m = m.rawData;
			params[index++] = m[0];
			if( c > 1 ) params[index++] = m[1];
			if( c > 2 ) params[index++] = m[2];
			if( c > 3 ) params[index++] = m[3];
			
			if( r > 1 ) {
				params[index++] = m[4];
				if( c > 1 ) params[index++] = m[5];
				if( c > 2 ) params[index++] = m[6];
				if( c > 3 ) params[index++] = m[7];

				if( r > 2 ) {
					params[index++] = m[8];
					if( c > 1 ) params[index++] = m[9];
					if( c > 2 ) params[index++] = m[10];
					if( c > 3 ) params[index++] = m[11];

					if( r > 3 ) {
						params[index++] = m[12];
						if( c > 1 ) params[index++] = m[13];
						if( c > 2 ) params[index++] = m[14];
						if( c > 3 ) params[index++] = m[15];
					}
				}
			}
			#end
		}
	}

	inline function saveMatrixT( params : flash.Vector<Float>, index : Int, m : ShaderTypes.Matrix, r : Int, c : Int ) {
		if( index >= 0 ) {
			#if h3d
			params[index++] = m._11;
			if( c > 1 ) params[index++] = m._21;
			if( c > 2 ) params[index++] = m._31;
			if( c > 3 ) params[index++] = m._41;
			
			if( r > 1 ) {
				params[index++] = m._12;
				if( c > 1 ) params[index++] = m._22;
				if( c > 2 ) params[index++] = m._32;
				if( c > 3 ) params[index++] = m._42;

				if( r > 2 ) {
					params[index++] = m._13;
					if( c > 1 ) params[index++] = m._23;
					if( c > 2 ) params[index++] = m._33;
					if( c > 3 ) params[index++] = m._43;

					if( r > 3 ) {
						params[index++] = m._14;
						if( c > 1 ) params[index++] = m._24;
						if( c > 2 ) params[index++] = m._34;
						if( c > 3 ) params[index++] = m._44;
					}
				}
			}
			#else
			var m = m.rawData;
			params[index++] = m[0];
			if( c > 1 ) params[index++] = m[4];
			if( c > 2 ) params[index++] = m[8];
			if( c > 3 ) params[index++] = m[12];
			
			if( r > 1 ) {
				params[index++] = m[1];
				if( c > 1 ) params[index++] = m[5];
				if( c > 2 ) params[index++] = m[9];
				if( c > 3 ) params[index++] = m[13];

				if( r > 2 ) {
					params[index++] = m[2];
					if( c > 1 ) params[index++] = m[6];
					if( c > 2 ) params[index++] = m[10];
					if( c > 3 ) params[index++] = m[14];

					if( r > 3 ) {
						params[index++] = m[3];
						if( c > 1 ) params[index++] = m[7];
						if( c > 2 ) params[index++] = m[11];
						if( c > 3 ) params[index++] = m[15];
					}
				}
			}
			#end
		}
	}

	inline function setParamBit( n : Int, v : Bool ) {
		if( v ) paramBits |= 1 << n else paramBits &= ~(1 << n);
	}
	
	public function toString() {
		return Type.getClassName(Type.getClass(this));
	}
	
}
