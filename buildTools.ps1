cd tools/build-tool/ # first enter the build tool directory
Set-ExecutionPolicy Bypass -Scope Process
./castle-engine_compile.ps1

$BinDir_Exists = (Test-Path bin)
if (!$BinDir_Exists)
{
    mkdir bin
}

$LAZBuildPath = "D:\soft\fpcupdeluxe\lazarus\lazbuild.exe"

copy tools/build-tool/data/external_libraries/x86_64-win64/*.dll bin/
copy tools/build-tool/data/external_libraries/x86_64-win64/openssl/*.dll bin/
copy tools/build-tool/castle-engine.exe bin/

& $LAZBuildPath -B "packages/castle_base.lpk"
& $LAZBuildPath -B "packages/castle_components.lpk"

& $LAZBuildPath "tools/castle-editor/castle_editor.lpr"
& $LAZBuildPath "tools/castle-curves/castle-curves.lpr"
& $LAZBuildPath "tools/image-to-pascal/image-to-pascal.lpr"
& $LAZBuildPath "tools/internal/x3d-nodes-to-pascal/x3d-nodes-to-pascal.lpr"
& $LAZBuildPath "tools/internal/generate-persistent-vectors/generate-persistent-vectors.lpr"
& $LAZBuildPath "tools/texture-font-to-pascal/texture-font-to-pascal.lpr"
& $LAZBuildPath "tools/to-data-uri/to-data-uri.lpr"

& $LAZBuildPath "../castle-view-image/castle-view-image.lpi"
& $LAZBuildPath "../view3dscene/view3dscene.lpr"
& $LAZBuildPath "../view3dscene/tovrmlx3d.lpr"




copy tools/castle-curves/castle-curves.exe bin/
copy tools/castle-editor/castle-editor.exe bin/
copy tools/image-to-pascal/image-to-pascal.exe bin/
copy tools/internal/x3d-nodes-to-pascal/x3d-nodes-to-pascal.exe bin/
copy tools/texture-font-to-pascal/texture-font-to-pascal.exe bin/
copy tools/to-data-uri/to-data-uri.exe bin/

copy ../castle-view-image/castle-view-image.exe bin/
copy ../view3dscene/view3dscene.exe bin/
copy ../view3dscene/tovrmlx3d.exe bin/

pause