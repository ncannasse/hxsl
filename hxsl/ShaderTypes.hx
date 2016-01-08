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

#if h3d
typedef Texture = h3d.mat.Texture;
typedef Vector = h3d.Vector;
typedef Matrix = h3d.Matrix;
#elseif dotpoint
typedef Texture = flash.display3D.textures.TextureBase;
typedef Vector = haxe.at.dotpoint.math.vector.Vector3;
typedef Matrix = haxe.at.dotpoint.math.vector.Matrix44;
#else
typedef Texture = flash.display3D.textures.TextureBase;
typedef Vector = flash.geom.Vector3D;
typedef Matrix = flash.geom.Matrix3D;
#end

typedef FixedArray<T,Const> = Array<T>;

