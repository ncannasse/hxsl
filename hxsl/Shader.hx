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
import haxe.ds.Vector;

/**
	A ShaderInstance is a compiled version of a shader for a given set of parameters.
**/
class ShaderInstance {

	public var bits : Int;
	public var lengths : Array<Array<Int>>;
	
	public var program : flash.display3D.Program3D;

	public var bufferFormat : Int;
	public var bufferNames : Array<String>;
	public var stride : Int;

	public var vertexVars : Vector<Float>;
	public var fragmentVars : Vector<Float>;
	public var textures : Vector<ShaderTypes.Texture>;
	public var curShaderId : Int;

	public var vertexMap : Vector<Int>;
	public var fragmentMap : Vector<Int>;
	public var textureMap : Vector<Int>;
	public var texHasConfig : Vector<Bool>;
	
	public var vertexBytes : haxe.io.Bytes;
	public var fragmentBytes : haxe.io.Bytes;
	
	public var varsChanged : Bool;
	
	public function new() {
		curShaderId = -1;
	}
	
	public function dispose() {
		if( program != null ) {
			program.dispose();
			program = null;
		}
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
	public var hasParamObject : Bool;
	public var hasParamLengths : Bool;

	var texHasConfig : Vector<Bool>;
	var constCount : Int;
	var instances : Map<String,ShaderInstance>;
	var hparams : Map<Int,hxsl.Data.Variable>;
	
	static var ALL = new Array<ShaderGlobals>();
	
	public function new( hxStr : String ) {
		this.data = hxsl.Unserialize.unserialize(hxStr);

		function checkSubType(t:Data.VarType) {
			switch( t ) {
			case TNull, TFloat, TBool, TInt, TTexture(_),TFloat2, TFloat3, TFloat4, TMatrix(_):
			case TArray(t, 0): hasParamLengths = true; checkSubType(t);
			case TArray(t, _): checkSubType(t);
			case TObject(fields):
				for( f in fields )
					checkSubType(f.t);
			}
		}
		
		function checkType(t:Data.VarType) {
			switch( t ) {
			case TNull, TFloat, TBool, TInt, TTexture(_):
			case TFloat2, TFloat3, TFloat4: hasParamVector = true;
			case TArray(_): throw "assert";
			case TMatrix(_): hasParamMatrix = true;
			case TObject(fields):
				hasParamObject = true;
				for( f in fields )
					checkSubType(f.t);
			}
		}
		hparams = new Map();
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
		
		texHasConfig = new Vector(texSize);
		Tools.iterBlock(data.fragment.exprs, lookupTextureAccess);
		
		instances = new Map();
		ALL.push(this);
	}
	
	function lookupTextureAccess( v : CodeValue ) {
		switch( v.d ) {
		case CTex(v, _, mode):
			var hasConfig = false, hasSampler = false;
			for( m in mode )
				switch( m.f ) {
				case CTFlag(TTypeDxt1 | TTypeDxt5 | TTypeRgba | TSingle), CTParam(PType, _), CTParam(PSingle, _):
				case CTFlag(TIgnoreSampler):
					hasSampler = true;
					hasConfig = false;
					break;
				case CTParam(PIgnoreSampler, _):
					hasSampler = true;
					hasConfig = true;
				default:
					hasConfig = true;
				}
			if( hasConfig )
				texHasConfig[v.index] = true;
			#if !advanced_telemetry
			// disable using TIgnoreSampler until Adobe Scout supports it
			else if( !hasSampler )
				mode.push( { f : CTFlag(TIgnoreSampler), p : v.pos } );
			#end
		default:
		}
		Tools.iter(v, lookupTextureAccess);
	}
	
	
	
	function build( code : hxsl.Data.Code ) {
			
		// init map
		var nregs = 0;
		var map = new Vector(constCount);
		for( i in 0...constCount )
			map[i] = -1;
			
		for( v in code.args ) {
			var realV = hparams.get(v.id);
			if( v == null ) throw "assert " + v.name;
			if( v.kind == VTexture ) continue;
			map[realV.index] = v.index * 4;
			nregs += Tools.regSize(v.type);
		}
		
		// add consts
		var pos = nregs * 4;
		nregs += code.consts.length;
		var consts = new Vector(nregs * 4);
		for( c in code.consts ) {
			for( v in c )
				consts[pos++] = v;
			for( i in c.length...4 )
				consts[pos++] = 1.;
		}
		
		var maxRegs = 128;
		if( nregs > maxRegs )
			throw "Shader has "+nregs+" parameters (max "+maxRegs+" allowed)";
		
		var agal = new hxsl.AgalCompiler().compile(code);
		var o = new haxe.io.BytesOutput();
		new format.agal.Writer(o).write(agal);
		return { bytes : o.getBytes(), consts : consts, map : map };
	}

	public function compileShader( bits, lengths : Array<Array<Int>> ) {
		var r = new hxsl.RuntimeCompiler();
		var params = { };
		var paramCount = 0;
		for( v in Tools.getAllVars(data) )
			if( v.kind == VParam ) {
				if( bits & (1 << paramCount) != 0 ) {
					var len = lengths == null ? null : lengths[paramCount];
					var lenPos = 0;
					function loop(t) : Dynamic {
						switch( t ) {
						case TBool: return true;
						case TArray(t, 0):
							return [for( i in 0...len[lenPos++] ) loop(t)];
						case TObject(fl):
							var o = { };
							for( f in fl )
								Reflect.setField(o, f.name, loop(f.t));
							return o;
						default:
							return 0;
						}
					}
					Reflect.setField(params, v.name, loop(v.type));
				}
				paramCount++;
			}
		return r.compile(data, params);
	}
	
	public function getInstance( bits : Int, lengths : Array<Array<Int>> ) {
		var signature = bits + ":" + lengths;
		var i = instances.get(signature);
		if( i != null )
			return i;
		var data2 = compileShader(bits, lengths);
		
		i = new ShaderInstance();
		i.bits = bits;
		i.lengths = lengths == null ? null : lengths.copy();
		
		var v = build(data2.vertex);
		i.vertexBytes = v.bytes;
		i.vertexMap = v.map;
		i.vertexVars = v.consts;
		
		var f = build(data2.fragment);
		i.fragmentBytes = f.bytes;
		i.fragmentMap = f.map;
		i.fragmentVars = f.consts;
				
		var tmap = new Array();
		for( v in data2.vertex.args.concat(data2.fragment.args) )
			if( v.kind == VTexture ) {
				var realV = hparams.get(v.id);
				tmap.push(realV.index);
			}
		i.textureMap = Vector.fromArrayCopy(tmap);
		i.textures = new Vector(i.textureMap.length);
		i.texHasConfig = texHasConfig;
		
		i.bufferFormat = 0;
		i.bufferNames = [];
		i.stride = 0;
		for( v in data2.globals )
			switch( v.kind ) {
			case VInput:
				function loop(name,t:VarType,index:Int) {
					var size = Tools.floatSize(t);
					switch( t ) {
					case TInt:
						// bufferFormat 0
						size = 1; // takes space of one float in buffer
					case TFloat, TFloat2, TFloat3, TFloat4:
						i.bufferFormat |= Tools.floatSize(t) << (3 * index);
					case TObject(fields):
						var tot = 0;
						for( f in fields )
							tot += loop(f.name, f.t, index++);
						return tot;
					default:
						throw "Type not supported in input " + Type.enumConstructor(t).substr(1);
					}
					i.bufferNames.push(name);
					i.stride += size;
					return size;
				}
				loop(v.name, v.type, v.index);
			case VVar:
				// ignore
			default:
				throw "assert " + v.kind;
			}

		instances.set(signature, i);
		
		return i;
	}
	
	public static function disposeAll( andCleanCache = false ) {
		for( g in ALL ) {
			for( i in g.instances ) {
				i.dispose();
				// this will force every getInstance() to lookup for a new one
				if( andCleanCache )
					i.bits++;
			}
			if( andCleanCache )
				g.instances = new Map();
		}
	}
	
}


@:autoBuild(hxsl.ShaderMacros.buildShader())
class Shader {

	static var ID = 0;
	
	var shaderId : Int;
	var globals : ShaderGlobals;
	var modified : Bool;
	var paramBits : Int;
	var paramLengths : Array<Array<Int>>;
	var paramLengthsModified : Bool;
	var paramVectors : Array<ShaderTypes.Vector>;
	var paramMatrixes : Array<ShaderTypes.Matrix>;
	var paramObjects : Array<Dynamic>;
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
		if( globals.hasParamObject ) paramObjects = [];
		if( globals.hasParamLengths ) {
			paramLengths = [];
			paramLengthsModified = true;
		}
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

	inline function makeArray<T>(size) : ShaderTypes.FixedArray<T,0> {
		return new ShaderTypes.FixedArray();
	}

	function arrayDiffer( a : Array<Dynamic>, b : Array<Dynamic> ) {
		if( a.length != b.length )
			return true;
		for( i in 0...a.length ) {
			var x = a[i];
			var y = a[i];
			if( x != y ) {
				var ax = Std.instance(x, Array);
				if( ax == null ) return true;
				var ay = Std.instance(y, Array);
				if( ay == null ) return true;
				return arrayDiffer(ax, ay);
			}
		}
		return false;
	}
	
	public function getInstance() : ShaderInstance {
		if( instance == null || instance.bits != paramBits || (paramLengthsModified && instance.lengths != null && arrayDiffer(instance.lengths,paramLengths)) ) {
			paramLengthsModified = false;
			instance = globals.getInstance(paramBits, paramLengths);
		}
		if( modified || instance.curShaderId != shaderId ) {
			updateParams();
			modified = false;
		}
		return instance;
	}

	#if debug
	public function getDebugShaderCode( bytecode = false ) {
		var data = globals.compileShader(paramBits,paramLengths);
		if( !bytecode )
			return hxsl.Debug.dataStr(data);
		function getCode(c) {
			var agal = new hxsl.AgalCompiler().compile(c);
			var lines = [];
			for( c in agal.code )
				lines.push("\t"+format.agal.Tools.opStr(c));
			return lines.join("\n");
		}
		return "vertex:\n" + getCode(data.vertex) + "\nfragment:\n" + getCode(data.fragment);
	}
	#end
	
	
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
		ctx.setProgramConstantsFromVector(flash.display3D.Context3DProgramType.VERTEX, 0, i.vertexVars.toData());
		ctx.setProgramConstantsFromVector(flash.display3D.Context3DProgramType.FRAGMENT, 0, i.fragmentVars.toData());
		for( k in 0...i.textures.length )
			ctx.setTextureAt(k, i.textures.get(k));
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
			ctx.setTextureAt(k, null);
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
			instance.textures.set(i,allTextures[instance.textureMap.get(i)]);
		instance.curShaderId = shaderId;
		instance.varsChanged = true;
	}
	
	function updateVertexParams( params : Vector<Float>, map : Vector<Int> ) {
	}

	function updateFragmentParams( params : Vector<Float>, map : Vector<Int> ) {
	}
	
	inline function saveFloats( params : Vector<Float>, index : Int, v : ShaderTypes.Vector, n : Int ) {
		if( index >= 0 ) {
			params[index] = v.x;
			params[index + 1] = v.y;
			if( n > 2 ) params[index + 2] = v.z;
			if( n > 3 ) params[index + 3] = v.w;
		}
	}
	
	inline function saveInt( params : Vector<Float>, index : Int, v : Int ) {
		if( index >= 0 ) {
			params[index] = ((v >> 16) & 0xFF) / 255;
			params[index + 1] = ((v >> 8) & 0xFF) / 255;
			params[index + 2] = (v & 0xFF) / 255;
			params[index + 3] = (v >>> 24) / 255;
		}
	}
	
	inline function saveFloat( params : Vector<Float>, index : Int, v : Float ) {
		if( index >= 0 ) params[index] = v;
	}

	inline function saveMatrix( params : Vector<Float>, index : Int, m : ShaderTypes.Matrix, r : Int, c : Int ) {
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

	inline function saveMatrixT( params : Vector<Float>, index : Int, m : ShaderTypes.Matrix, r : Int, c : Int ) {
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
	
	public function rebuildVars() {
		modified = true;
	}
	
	public function toString() {
		return Type.getClassName(Type.getClass(this));
	}
	
}
