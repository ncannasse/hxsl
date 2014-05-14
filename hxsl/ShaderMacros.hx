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
import haxe.macro.Context;
import haxe.macro.Expr;
import hxsl.Data;

class ShaderMacros {

	static function realType( t : VarType, p : Position ) : ComplexType {
		return switch( t ) {
		case TNull: throw "assert";
		case TBool: TPath({ pack : [], name : "Bool", params : [], sub : null });
		case TFloat: TPath({ pack : [], name : "Float", params : [], sub : null });
		case TFloat2, TFloat3, TFloat4: TPath({ pack : ["hxsl"], name : "ShaderTypes", params : [], sub : "Vector" });
		case TInt: TPath({ pack : [], name : "Int", params : [], sub : null });
		case TMatrix(_): TPath({ pack : ["hxsl"], name: "ShaderTypes", params : [], sub : "Matrix" });
		case TTexture(_): TPath( { pack : ["hxsl"], name : "ShaderTypes", params : [], sub : "Texture" } );
		case TArray(t,0): TPath( { pack : [], name : "Array", params : [TPType(realType(t, p))] } );
		case TArray(t, size): TPath( { pack : ["hxsl"], name : "ShaderTypes", sub : "FixedArray", params : [TPType(realType(t, p)), TPExpr( { expr : EConst(CInt("" + size)), pos : p } )] } );
		case TObject(fields): TAnonymous([for( f in fields ) { name : f.name, pos : p, kind : FVar(realType(f.t, p)), access : [], meta : [], doc :null }]);
		};
	}

	static function isMutable( t : VarType ) {
		return switch( t ) {
		case TNull,TFloat, TBool, TInt, TTexture(_): false;
		case TFloat2, TFloat3, TFloat4, TMatrix(_), TArray(_), TObject(_): true;
		};
	}

	static function freeVar( eindex : Expr, saves : Array<Expr> ) {
		switch( eindex.expr ) {
		case EArray(_, { expr : EConst(CInt(i)) } ):
			var vn = "_m" + i;
			saves.push(macro var $vn = $eindex);
			return macro $i{vn};
		default:
			return eindex;
		}
	}

	static function saveType( t : VarType, eindex : Expr, evar : Expr, pos, rec = 0 ) {
		var args = [{ expr : EConst(CIdent("_params")), pos : pos }, eindex, evar];
		var name = switch( t ) {
		case TBool, TNull, TTexture(_): throw "assert";
		case TInt: "Int";
		case TFloat: "Float";
		case TFloat2, TFloat3, TFloat4:
			args.push( { expr : EConst(CInt(Tools.floatSize(t) + "")), pos : pos } );
			"Floats";
		case TMatrix(r, c, t):
			args.push( { expr : EConst(CInt(r + "")), pos : pos } );
			args.push( { expr : EConst(CInt(c + "")), pos : pos } );
			if( t.t ) "MatrixT" else "Matrix";
		case TArray(t, size):
			var saves = [];
			eindex = freeVar(eindex,saves);
			var vi = "i" + rec, vk = "k" + rec;
			var ei = { expr : EConst(CIdent(vi)), pos : pos };
			var ek = { expr : EConst(CIdent(vk)), pos : pos };
			var eit = size == 0 ? evar : macro 0...($v{size});
			var save = saveType(t, eindex, ek, pos, rec + 1);

			switch( t ) {
			case TArray(_, 0), TObject(_):
			default:
				save = macro {
					$save;
					$eindex += ($v{Tools.regSize(t) * 4});
				}
			}

			if( size == 0 )
				saves.push(macro for( $ek in $eit ) $save);
			else
				saves.push(macro
					for( $ei in $eit ) {
						var $vk = $evar[$ei];
						if( $ek == null ) break;
						$save;
					}
				);
			return macro { $a{saves}; };
		case TObject(fields):
			var saves = [];
			eindex = freeVar(eindex,saves);
			var vo = "o" + rec;
			var eo = { expr : EConst(CIdent(vo)), pos : pos };
			for( f in fields ) {
				var save = saveType(f.t, eindex, { expr : EField(eo, f.name), pos : pos }, pos, rec + 1);
				saves.push(save);
				switch( f.t ) {
				case TArray(_, 0), TObject(_):
				default:
					saves.push(macro $eindex += ($v{Tools.regSize(f.t) * 4}));
				}
			}
			var vt = realType(t, pos);
			return macro {
				var $vo : $vt = $evar;
				if( $eo != null ) {$a{saves}}
			};
		}
		return { expr : ECall( { expr : EConst(CIdent("save" + name)), pos : pos }, args), pos : pos };
	}

	public static function buildShader() {
		var fields = Context.getBuildFields();
		var shaderCode = null;
		for( f in fields )
			if( f.name == "SRC" ) {
				switch( f.kind ) {
				case FVar(_, e):
					shaderCode = e;
				default:
				}
				fields.remove(f);
				// remove even if error
				haxe.macro.Compiler.removeField(Context.getLocalClass().toString(), "SRC", true);
				break;
			}
		var cl = switch( Context.getLocalType() ) {
		case TInst(c, _): c.get();
		default: throw "assert";
		}
		if( shaderCode == null ) {
			if( cl.meta.has(":skip") )
				return null;
			Context.error("Shader SRC not found", cl.pos);
		}

		var p = new hxsl.Parser();
		p.includeFile = function(file) {
			var f = Context.resolvePath(file);
			return Context.parse("{"+sys.io.File.getContent(f)+"}", Context.makePosition( { min : 0, max : 0, file : f } ));
		};

		var data = try p.parse(shaderCode) catch( e : hxsl.Data.Error ) haxe.macro.Context.error(e.message, e.pos);
		var c = new hxsl.Compiler();
		c.warn = Context.warning;
		var data = try c.compile(data) catch( e : hxsl.Data.Error ) haxe.macro.Context.error(e.message, e.pos);


		var pos = Context.currentPos();

		// store HXSL data for runtime compilation
		fields.push({
			name : "HXSL",
			kind : FVar(null,{ expr : EConst(CString(hxsl.Serialize.serialize(data))), pos : pos }),
			access : [AStatic],
			pos : pos,
			meta : [ { name:":keep", params : [], pos : pos } ],
		});
		fields.push( {
			name : "GLOBALS",
			kind : FVar(macro : hxsl.Shader.ShaderGlobals),
			access : [AStatic],
			pos : pos,
			meta : [ { name:":keep", params : [], pos : pos } ],
		});

		// create all the variables accessors
		var allVars = Tools.getAllVars(data);

		var updates = [], constructs = [];
		var paramCount = 0, texCount = 0, paramVectorCount = 0, paramMatrixCount = 0, paramObjectCount = 0, constCount = 0, lengthsCount = 0;

		for( v in allVars ) {
			var pos = v.pos;
			var t = realType(v.type, pos);
			switch( v.kind ) {
			case VConst:
				var mut = isMutable(v.type);
				var constIndex = constCount++;

				// creates a real field to store the data
				fields.push( {
					name : v.name+"_",
					kind : FVar(t),
					pos : pos,
					access : [],
				});

				// creates a virtual field to track changes
				fields.push({
					name : v.name,
					kind : FProp("get", "set", t),
					pos : pos,
					access : [APublic],
				});

				var evar = { expr : EConst(CIdent(v.name + "_")), pos : pos };

				var expr = if( !mut ) macro return $evar else {
					var args = [];
					var allocName = switch( v.type ) {
					case TArray(_, size):
						args.push( { expr : EConst(CInt("" + size)), pos : pos } );
						"Array";
					case TMatrix(_): "Matrix";
					case TFloat2, TFloat3, TFloat4: "Vector";
					default: throw "assert";
					}
					var make = { expr : EConst(CIdent("make" + allocName)), pos : pos };
					make = { expr : ECall(make, args), pos : pos };
					constructs.push(macro $evar = $make);
					macro {
						modified = true; // we can mute the components after reading
						return $evar;
					}
				};
				fields.push( {
					name : "get_" + v.name,
					kind : FFun({ ret : t, params : [], args : [], expr : expr }),
					pos : pos,
					access : mut ? [] : [AInline],
				});

				fields.push({
					name : "set_" + v.name,
					kind : FFun( {
						ret : t,
						params : [],
						args : [{ name : "v",  type : t, opt : false }],
						expr : macro {
							modified = true;
							$evar = v;
							return v;
						},
					}),
					pos : pos,
					access : [],
				});

				var mapIndex = { expr : EArray( { expr : EConst(CIdent("_map")), pos : pos }, { expr : EConst(CInt("" + constIndex)), pos:pos } ), pos : pos };
				var save = saveType(v.type, mapIndex, evar, pos);
				updates.push({ v : v, save : save });

			case VParam:

				if( paramCount == 32 )
					Context.error("Too many runtime parameters for this shader (max=32)", v.pos);
				var paramIndex = paramCount++;
				var constIndex = constCount++;
				var index = { expr : EConst(CInt("" + paramIndex)), pos : pos };

				// virtual field only (storage is indexed)
				var field : Field = {
					name : v.name,
					kind : FProp("get", "set", t),
					pos : pos,
					access : [APublic],
				};
				fields.push(field);

				switch( v.type ) {
				case TBool:
					// simply get set on the param bit
					fields.push( {
						name : "get_" + v.name,
						kind : FFun({
							args : [],
							ret : t,
							params : [],
							expr : macro return paramBits & (1 << $index) != 0,
						}),
						pos : pos,
						access : [AInline],
					});
					fields.push( {
						name : "set_" + v.name,
						kind : FFun( {
							args : [ { name : "v", type : t, opt : false } ],
							ret : t,
							params : [],
							expr : macro { setParamBit($index, v); return v; },
						}),
						pos : pos,
						access : [AInline],
					});
				case TFloat:
					var tnull = TPath( { name : "Null", pack : [], params : [TPType(t)] } );
					var evar = macro $i{v.name};
					field.meta = [];
					field.kind = FProp("default", "set", tnull);
					fields.push( {
						name : "set_" + v.name,
						kind : FFun( {
							args : [ { name : "v", type : tnull, opt : false } ],
							ret : t,
							params : [],
							expr : macro { setParamBit($index, v != null); $evar = v; return v; },
						}),
						pos : pos,
						access : [AInline],
					});

					var mapIndex = { expr :EArray( { expr : EConst(CIdent("_map")), pos : pos }, { expr : EConst(CInt("" + constIndex)), pos:pos } ), pos : pos };
					var save = saveType(v.type, mapIndex, evar, pos);
					updates.push( { v : v, save : save } );

				case TObject(_):

					var evar = { expr : EArray( { expr : EConst(CIdent("paramObjects")), pos : pos }, { expr : EConst(CInt("" + paramObjectCount++)), pos : pos } ), pos : pos };

					var lname = "_l" + paramIndex;
					var lvar = { expr : EConst(CIdent(lname)), pos : pos };

					function loop(v,t) {
						switch( t ) {
						case TBool:
							throw "Bool not supported in object";
						case TObject(fields):
							var a = [];
							for( f in fields ) {
								var v = loop({ expr : EField(v,f.name), pos : pos },f.t);
								if( v != null )
									a.push(v);
							}
							if( a.length == 0 )
								return null;
							return { expr : EBlock(a), pos : pos };
						case TArray(t, 0):
							var vs = macro sub;
							var sub = loop(vs,t);
							return if( sub == null )
								macro if( $v == null ) $lvar.push(0) else $lvar.push($v.length);
							else
								macro if( $v == null ) $lvar.push(0) else { $lvar.push($v.length); for( $vs in $v ) $sub; };
						default:
							// nothing
							return null;
						}
					}
					var saveLength = loop(macro v, v.type);
					if( saveLength == null )
						saveLength = macro { }
					else
						saveLength = macro { paramLengthsModified = true; if( v == null ) paramLengths[$v { paramIndex } ] = null else { var $lname = paramLengths[$v { paramIndex } ] = []; $saveLength; } };

					fields.push( {
						name : "get_" + v.name,
						kind : FFun({ ret : t, params : [], args : [], expr : macro { var tmp = $evar; if( tmp != null ) modified = true; return tmp; } }),
						pos : pos,
						access : [AInline],
					});

					fields.push( {
						name : "set_" + v.name,
						kind : FFun( {
							ret : t,
							params : [],
							args : [ { name : "v", type : t, opt : false } ],
							expr : macro {
								modified = true;
								$evar = v;
								setParamBit($index, v != null);
								$saveLength;
								return v;
							} }),
						pos : pos,
						access : [AInline],
					});

					var mapIndex = { expr : EArray( { expr : EConst(CIdent("_map")), pos : pos }, { expr : EConst(CInt("" + constIndex)), pos:pos } ), pos : pos };
					var save = saveType(v.type, mapIndex, evar, pos);
					updates.push( { v : v, save : save } );

				default:
					var evar = null;
					switch( v.type ) {
					case TBool, TFloat, TNull, TTexture(_), TObject(_): throw "assert";
					case TArray(_), TInt: Context.error("This type is not yet supported as shader parameter", v.pos);
					case TFloat2, TFloat3, TFloat4:
						evar = { expr : EArray( { expr : EConst(CIdent("paramVectors")), pos : pos }, { expr : EConst(CInt("" + paramVectorCount++)), pos : pos } ), pos : pos };
					case TMatrix(_):
						evar = { expr : EArray( { expr : EConst(CIdent("paramMatrixes")), pos : pos }, { expr : EConst(CInt("" + paramMatrixCount++)), pos : pos } ), pos : pos };
					}

					fields.push( {
						name : "get_" + v.name,
						kind : FFun({ ret : t, params : [], args : [], expr : macro { var tmp = $evar; if( tmp != null ) modified = true; return tmp; } }),
						pos : pos,
						access : [AInline],
					});

					fields.push( {
						name : "set_" + v.name,
						kind : FFun( {
							ret : t,
							params : [],
							args : [ { name : "v", type : t, opt : false } ],
							expr : macro {
								modified = true;
								$evar = v;
								setParamBit($index, v != null);
								return v;
							} }),
						pos : pos,
						access : [AInline],
					});

					var mapIndex = { expr : EArray( { expr : EConst(CIdent("_map")), pos : pos }, { expr : EConst(CInt("" + constIndex)), pos:pos } ), pos : pos };
					var save = saveType(v.type, mapIndex, evar, pos);
					updates.push( { v : v, save : save } );
				}

			case VTexture:
				var tid = { expr : EConst(CInt("" + texCount++)), pos : pos };

				fields.push( {
					name : v.name,
					kind : FProp("get", "set", t),
					pos : pos,
					access : [APublic],
				});

				fields.push({
					name : "get_" + v.name,
					kind : FFun( {
						ret : t,
						params : [],
						args : [],
						expr : macro return allTextures[$tid],
					}),
					pos : v.pos,
					access : [AInline],
				});

				fields.push({
					name : "set_" + v.name,
					kind : FFun( {
						ret : t,
						params : [],
						args : [{ name : "v", type : t, opt : false }],
						expr : macro { allTextures[$tid] = v; return v; },
					}),
					pos : v.pos,
					access : [AInline],
				});

			case VInput, VVar, VOut:
				// skip
			default:
				Context.warning(v.name, pos);
				throw "assert";
			}
		}

		// add constructor initializations
		var newFound = false;
		for( f in fields )
			if( f.name == "new" ) {
				switch( f.kind ) {
				case FFun(f):
					switch( f.expr.expr ) {
					case EBlock(el):
						for( c in constructs )
							el.unshift(c);
					default:
						constructs.push(f.expr);
						f.expr.expr = EBlock(constructs);
					}
				default:
				}
				newFound = true;
				break;
			}
		if( !newFound )  {
			constructs.unshift(macro super());
			fields.push({
				name : "new",
				kind : FFun( {
					ret : null,
					args : [],
					params : [],
					expr : { expr : EBlock(constructs), pos : pos },
				}),
				access : [APublic],
				pos : pos,
			});
		}

		// add updates
		var updateVertex = [], updateFragment = [];
		for( u in updates )
			if( u.v.index == 1 )
				updateFragment.push(u.save);
			else
				updateVertex.push(u.save);
		if( updateVertex.length > 0 )
			fields.push({
				name : "updateVertexParams",
				kind : FFun( {
					ret : null,
					args : [{ name : "_params", type : null, opt : false }, { name : "_map", type : macro : haxe.ds.Vector<Int>, opt : false }],
					params : [],
					expr : { expr : EBlock(updateVertex), pos : pos },
				}),
				access : [AOverride],
				pos : pos,
			});
		if( updateFragment.length > 0 )
			fields.push({
				name : "updateFragmentParams",
				kind : FFun( {
					ret : null,
					args : [{ name : "_params", type : null, opt : false }, { name : "_map", type : macro : haxe.ds.Vector<Int>, opt : false }],
					params : [],
					expr : { expr : EBlock(updateFragment), pos : pos },
				}),
				access : [AOverride],
				pos : pos,
			});

		return fields;
	}

}
#end
