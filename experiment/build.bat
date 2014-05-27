ocamlopt -strict-sequence -unsafe -ffast-math -c Prelude.mli
@if errorlevel 1 goto bye
ocamlopt -strict-sequence -unsafe -ffast-math -c Prelude.ml
@if errorlevel 1 goto bye
ocamlopt -strict-sequence -unsafe -ffast-math -c Bitmap.mli
@if errorlevel 1 goto bye
ocamlopt -strict-sequence -unsafe -ffast-math -c Bitmap.ml
@if errorlevel 1 goto bye
ocamlopt -strict-sequence -unsafe -ffast-math -c JpegParser.ml
@if errorlevel 1 goto bye
ocamlopt -strict-sequence -unsafe -ffast-math -o a.exe Prelude.cmx Bitmap.cmx JpegParser.cmx
@if errorlevel 1 goto bye
D:\code\clock a teatime.jpg
:bye
