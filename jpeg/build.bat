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
ocamlopt -strict-sequence -unsafe -ffast-math -o JpegParser.exe Prelude.cmx Bitmap.cmx JpegParser.cmx
@if errorlevel 1 goto bye
ocamlopt -strict-sequence -unsafe -ffast-math -c BmpParser.ml
@if errorlevel 1 goto bye
ocamlopt -strict-sequence -unsafe -ffast-math -o BmpParser.exe Prelude.cmx Bitmap.cmx BmpParser.cmx
@if errorlevel 1 goto bye
D:\code\clock jpegparser teatime.jpg
D:\code\clock bmpparser  out.bmp
:bye
@rem bmpparser gig-sn01.bmp gig-sn01.bmp_out.jpg && bmpparser gig-sn08.bmp gig-sn08.bmp_out.jpg && bmpparser monalisa.bmp monalisa.bmp_out.jpg && bmpparser teatime.bmp teatime.bmp_out.jpg
