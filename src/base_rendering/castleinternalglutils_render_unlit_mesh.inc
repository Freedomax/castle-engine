{%MainUnit castleinternalglutils.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastleRenderUnlitMesh class. }

{$ifdef read_interface}

type
  { Render a set of vertexes, with optional indexes.
    Can be used multiple times, to render the same vertexes many times.

    This is a deliberately simple and @italic(thin) wrapper over creating
    and drawing a bunch of vertexes on modern GPUs.
    E.g. on OpenGL(ES) it creates 1 VBO (vertex buffer object) for position data
    and optional 1 VBO for indexes (if SetIndexes is ever called).

    Make sure all usage is within the same rendering context.
    This has to be created and destroyed while the OpenGL(ES) context is active.

    This is suitable to render only trivial unlit (or invisible) meshes.
    @italic(This is not a full-featured mesh renderer.)
    It doesn't define any per-vertex attributes other than vertex positions.
    It limits rendering to 16-bit indexes, and always passed vectors as 4D (in homogeneous
    coordinates).
    It follows some current @link(RenderContext) state,
    like @link(TRenderContext.DepthTest),
    @link(TRenderContext.LineWidth) -- it doesn't control this state.
    For a full-featured mesh rendering, just use TCastleScene, and construct your
    mesh as TIndexedTriangleSetNode or TIndexedFaceSetNode, see
    https://castle-engine.io/viewport_and_scenes_from_code#_building_a_mesh_using_code .

    The things you really have to set before rendering are:

    @unorderedList(
      @item(Vertexes, using @link(SetVertexes).)
      @item(Matrix (combined projection * camera * model transformation) @link(ModelViewProjection).)
      @item(That's it, the rest has sensible defaults. You can just call @link(Render).)
    )

    For shading this uses a simple shader:

    @unorderedList(
      @item(If @link(UseColor) (default @true) then the shader writes @link(Color)
        to the color buffer. Effectively this makes unlit rendering.

        Note: This class does not set up blending if @link(Color) alpha is less than 1.
        If you want blending, set it yourself using @link(TRenderContext.BlendingEnable)
        and disable using @link(TRenderContext.BlendingDisable).
      )

      @item(If @link(UseColor) is @false then the shader writes undefined
        value to the color buffer.

        But it still writes proper values to depth and stencil.

        This is esp. useful when you render things with color buffer disabled
        (using @code(RenderContext.ColorChannels := [])).
        The rendering may still be useful, e.g. to fill depth buffer (for shadow maps),
        stencil buffer, or define shape for GPU occlusion query.

        Note: This class does not configure @code(RenderContext.ColorChannels).
        It is up to you to decide how to hide from user the undefined effect
        this has on the color buffer.
      )

      @item(
        TODO: Maybe in the future it will be extended to optionally use a provided Shader.
        Potential docs:

        Uses the provided @link(Shader) for shading, which must be linked when calling @link(Draw).
        If the OpenGL(ES) does not support shaders or @link(Shader) is @nil,
        this will use built-in shader based on @link(UseColor).
      )
    )

    Example usage:

    @longCode(#
      SavedDepthTest := RenderContext.DepthTest;
      RenderContext.DepthTest := true;

      Mesh := TCastleRenderUnlitMesh.Create(true);
      try
        Mesh.Color := Yellow;
        Mesh.ModelViewProjection := RenderContext.ProjectionMatrix * RenderingCamera.CurrentMatrix;
        Mesh.SetVertexes([
          Vector3(0, 0, 0, 1),
          Vector3(10, 0, 0, 1),
          Vector3(0, 10, 0, 1)
        ]);
        Mesh.Render(pmTriangles);
      finally FreeAndNil(Mesh) end;

      RenderContext.DepthTest := SavedDepthTest;
    #)
  }
  TCastleRenderUnlitMesh = class
  strict private
    Shader: TGLSLProgram;
    UniformModelViewProjectionMatrix, UniformColor: TGLSLUniform;
    AttributeVertex: TGLSLAttribute;
    FUseColor: Boolean;
    FColor: TCastleColor;
    VboVertex, VboIndex: TGLuint;
    VertexAllocatedBytes, IndexAllocatedBytes,
      VertexAllocatedItems, IndexAllocatedItems: Cardinal;
    FixedFunctionVertexes: TVector4List;
    FixedFunctionIndexes: TUInt16List;
    Vao: TVertexArrayObject;
  public
    { By default all zeros, you must set this before any rendering. }
    ModelViewProjection: TMatrix4;

    constructor Create(const AUseColor: Boolean = true);
    destructor Destroy; override;

    { Define indexes for all future rendering.
      Note: Passing indexes that do not exist in vertexes you define in SetVertexes
      may result in crashes, neither CGE nor rendering API necessarily check it. }
    procedure SetIndexes(const Indexes: array of UInt16); overload;
    procedure SetIndexes(const Indexes: PUInt16; const IndexesCount: Cardinal); overload;

    procedure SetVertexes(const Vertexes: array of TVector4;
      const UsageDynamic: Boolean); overload;
    procedure SetVertexes(const Vertexes: TVector4List;
      const UsageDynamic: Boolean); overload;
    procedure SetVertexes(const Vertexes: PVector4; const VertexesCount: Cardinal;
      const UsageDynamic: Boolean); overload;

    property UseColor: Boolean read FUseColor;
    { Color used for unlit rendering, if only UseColor is @true. Opaque white by default. }
    property Color: TCastleColor read FColor write FColor;

    procedure Render(const Mode: TPrimitiveMode);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TODO: Constants duplicated from castleglutils_draw_primitive_2d.inc,
  maybe one day we'll join DrawPrimitive2D and TCastleRenderUnlitMesh into one unit
  (CastleRenderPrimitives?) which would make it natural to define this in one place. }

const
  PrimitiveModeToGL: array [TPrimitiveMode] of TGLEnum = (
    GL_POINTS,
    GL_LINE_STRIP,
    GL_LINE_LOOP,
    GL_LINES,
    GL_TRIANGLE_STRIP,
    GL_TRIANGLE_FAN,
    GL_TRIANGLES
  );

constructor TCastleRenderUnlitMesh.Create(const AUseColor: Boolean);
const
  SimplestVS = {$I simplest.vs.inc};
  SimplestFS = {$I simplest.fs.inc};
  SimplestUnlitFS = {$I simplest_unlit.fs.inc};
begin
  inherited Create;
  FUseColor := AUseColor;
  FColor := White;

  if not GLFeatures.EnableFixedFunction then
  begin
    Assert(GLFeatures.Shaders); // EnableFixedFunction and Shaders are now synchronized

    Shader := TGLSLProgram.Create;
    Shader.AttachShader(stVertex, SimplestVS);

    if AUseColor then
    begin
      Shader.AttachShader(stFragment, SimplestUnlitFS);
    end else
    begin
      { On desktop OpenGL, fragment shader doesn't need to exist now.
        https://www.khronos.org/opengl/wiki/Fragment_Shader#Optional }
      {$ifdef OpenGLES}
      Shader.AttachShader(stFragment, SimplestFS);
      {$endif}
    end;

    Shader.Link;

    AttributeVertex := Shader.Attribute('castle_Vertex');
    UniformModelViewProjectionMatrix := Shader.Uniform('castle_ModelViewProjectionMatrix');
    if AUseColor then
      UniformColor := Shader.Uniform('color');
  end;

  Vao := TVertexArrayObject.Create;
end;

destructor TCastleRenderUnlitMesh.Destroy;
begin
  FreeAndNil(Shader);
  FreeAndNil(Vao);
  glFreeBuffer(VboIndex);
  glFreeBuffer(VboVertex);
  inherited;
end;

procedure TCastleRenderUnlitMesh.Render(const Mode: TPrimitiveMode);
var
  SavedFixedFunctionLighting: Boolean;
  IndexPtr: Pointer;
begin
  if not GLFeatures.EnableFixedFunction then
  begin
    if VboVertex = 0 then
      raise EInternalError.Create('TCastleRenderUnlitMesh has no vertexes defined to render');

    RenderContext.CurrentProgram := Shader;
    RenderContext.CurrentVao := Vao;

    glBindBuffer(GL_ARRAY_BUFFER, VboVertex);
    // do this even if VboIndex = 0, to set this to 0
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VboIndex);

    if FUseColor then
      UniformColor.SetValue(Color);
    UniformModelViewProjectionMatrix.SetValue(ModelViewProjection);
    AttributeVertex.EnableArrayVector4(Vao, SizeOf(TVector4), 0);
  end else
  begin
    {$ifndef OpenGLES}
    if FUseColor then
      glColorv(Color);

    { We load as GL_MODELVIEW the complete matrix projection * modelview.
      This keeps our API simple (we just need 1 matrix, ModelViewProjection, already multiplied)
      and is good enough for rendering unlit stuff.
      So for GL_PROJECTION, we make sure to load identity. }

    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    glMatrixMode(GL_MODELVIEW);
    glLoadMatrix(ModelViewProjection);

    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(4, GL_FLOAT, SizeOf(TVector4), @(FixedFunctionVertexes.List^[0]));

    SavedFixedFunctionLighting := RenderContext.FixedFunctionLighting;
    RenderContext.FixedFunctionLighting := false;
    {$endif}
  end;

  if IndexAllocatedItems <> 0 then
  begin
    if not GLFeatures.EnableFixedFunction then
      IndexPtr := nil
    else
      IndexPtr := @(FixedFunctionIndexes.List^[0]);
    glDrawElements(PrimitiveModeToGL[Mode], IndexAllocatedItems, GL_UNSIGNED_SHORT, IndexPtr);
  end else
    glDrawArrays(PrimitiveModeToGL[Mode], 0, VertexAllocatedItems);

  if not GLFeatures.EnableFixedFunction then
  begin
    AttributeVertex.DisableArray;

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end else
  begin
    {$ifndef OpenGLES}
    { Restore GL_PROJECTION matrix value, other drawing code may depend on it.
      Don't restore GL_MODELVIEW, no need, other drawing code has to be prepared
      to always initialize GL_MODELVIEW. }
    glMatrixMode(GL_PROJECTION);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);

    glDisableClientState(GL_VERTEX_ARRAY);

    RenderContext.FixedFunctionLighting := SavedFixedFunctionLighting;
    {$endif}
  end;
end;

procedure TCastleRenderUnlitMesh.SetIndexes(const Indexes: array of UInt16);
begin
  { Note: This assumes that Indexes is actually a "packed array of UInt16",
    but it cannot be declared as such in Delphi.
    We verify it by test TTestCompiler.TestPackedOpenArray in CGE tests/ projects. }
  SetIndexes(@Indexes[0], High(Indexes) + 1);
end;

procedure TCastleRenderUnlitMesh.SetIndexes(const Indexes: PUInt16; const IndexesCount: Cardinal);
var
  NewVbo: Boolean;
  NewSize: Cardinal;
begin
  if not GLFeatures.EnableFixedFunction then
  begin
    NewVbo := VboIndex = 0;
    if NewVbo then
      glGenBuffers(1, @VboIndex);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VboIndex);

    NewSize := IndexesCount * SizeOf(UInt16);
    if NewVbo or (NewSize <> IndexAllocatedBytes) then
    begin
      // always GL_STATIC_DRAW for indexes, we assume we will not modify indexes very often
      glBufferData(GL_ELEMENT_ARRAY_BUFFER, NewSize, Indexes, GL_STATIC_DRAW);
      IndexAllocatedBytes := NewSize;
      IndexAllocatedItems := IndexesCount;
    end else
      glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, NewSize, Indexes);
  end else
  begin
    IndexAllocatedItems := IndexesCount;
    IndexAllocatedBytes := SizeOf(UInt16) * IndexesCount;
    if FixedFunctionIndexes = nil then
      FixedFunctionIndexes := TUInt16List.Create;
    FixedFunctionIndexes.Count := IndexesCount;
    Move(Indexes^, FixedFunctionIndexes.List^, IndexAllocatedBytes);
  end;
end;

procedure TCastleRenderUnlitMesh.SetVertexes(const Vertexes: PVector4; const VertexesCount: Cardinal;
  const UsageDynamic: Boolean);
var
  NewVbo: Boolean;
  NewSize: Cardinal;
  DataUsage: TGLEnum;
begin
  if not GLFeatures.EnableFixedFunction then
  begin
    NewVbo := VboVertex = 0;
    if NewVbo then
      glGenBuffers(1, @VboVertex);

    glBindBuffer(GL_ARRAY_BUFFER, VboVertex);

    NewSize := VertexesCount * SizeOf(TVector4);
    if NewVbo or (NewSize <> VertexAllocatedBytes) then
    begin
      if UsageDynamic then
          { GL_STREAM_DRAW is most suitable if we will modify this every frame,
            according to
            https://www.khronos.org/opengl/wiki/Buffer_Object
            https://computergraphics.stackexchange.com/questions/5712/gl-static-draw-vs-gl-dynamic-draw-vs-gl-stream-draw-does-it-matter
          }
          DataUsage := GL_STREAM_DRAW
        else
          DataUsage := GL_STATIC_DRAW;
        glBufferData(GL_ARRAY_BUFFER, NewSize, Vertexes, DataUsage);
      VertexAllocatedBytes := NewSize;
      VertexAllocatedItems := VertexesCount;
    end else
      glBufferSubData(GL_ARRAY_BUFFER, 0, NewSize, Vertexes);
  end else
  begin
    VertexAllocatedItems := VertexesCount;
    VertexAllocatedBytes := SizeOf(TVector4) * VertexesCount;
    if FixedFunctionVertexes = nil then
      FixedFunctionVertexes := TVector4List.Create;
    FixedFunctionVertexes.Count := VertexesCount;
    Move(Vertexes^, FixedFunctionVertexes.List^, VertexAllocatedBytes);
  end;
end;

procedure TCastleRenderUnlitMesh.SetVertexes(const Vertexes: array of TVector4;
  const UsageDynamic: Boolean);
begin
  { Note: This assumes that Vertexes is actually a "packed array of TVector4",
    but it cannot be declared as such in Delphi.
    We verify it by test TTestCompiler.TestPackedOpenArray in CGE tests/ projects. }
  SetVertexes(@Vertexes[0], High(Vertexes) + 1, UsageDynamic);
end;

procedure TCastleRenderUnlitMesh.SetVertexes(const Vertexes: TVector4List;
  const UsageDynamic: Boolean);
begin
  SetVertexes(PVector4(Vertexes.L), Vertexes.Count, UsageDynamic);
end;

{$endif read_implementation}