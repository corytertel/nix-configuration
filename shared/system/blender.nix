{ config, stdenv, lib, fetchurl, boost, cmake, ffmpeg_3, gettext, glew
, ilmbase, libXi, libX11, libXext, libXrender
, libjpeg, libpng, libsamplerate, libsndfile
, libtiff, libGLU, libGL, openal, opencolorio, openexr, openimagedenoise, openimageio2, openjpeg, python3Packages
, openvdb, libXxf86vm, tbb, alembic
, zlib, fftw, opensubdiv, freetype, jemalloc, ocl-icd, addOpenGLRunpath
, jackaudioSupport ? false, libjack2
, cudaSupport ? config.cudaSupport or false, cudatoolkit
, colladaSupport ? true, opencollada
, makeWrapper
, pugixml, SDL, Cocoa, CoreGraphics, ForceFeedback, OpenAL, OpenGL
}:

let
  python = python3Packages.python;
in
{
  nixpkgs.overlays = [
    (final: prev: {
      blender = prev.blender.overrideAttrs ( old: {
        pname = "blender";
        version = "2.83.5";

        src = fetchurl {
          url = "https://download.blender.org/source/blender-2.83.5.tar.xz";
          sha256 = "0xyawly00a59hfdb6b7va84k5fhcv2mxnzd77vs22bzi9y7sap43";
        };

        nativeBuildInputs = [ cmake ];
        buildInputs =
          [ boost ffmpeg_3 gettext glew ilmbase
            freetype libjpeg libpng libsamplerate libsndfile libtiff
            opencolorio openexr openimagedenoise openimageio2 openjpeg python zlib fftw jemalloc
            alembic
            (opensubdiv.override { inherit cudaSupport; })
            tbb
            makeWrapper
            libXi libX11 libXext libXrender
            libGLU libGL openal
            libXxf86vm
            openvdb
          ];

        postPatch = ''
                    substituteInPlace extern/clew/src/clew.c --replace '"libOpenCL.so"' '"${ocl-icd}/lib/libOpenCL.so"'
                    '';

        cmakeFlags =
          [
            "-DWITH_ALEMBIC=ON"
            "-DWITH_MOD_OCEANSIM=ON"
            "-DWITH_CODEC_FFMPEG=ON"
            "-DWITH_CODEC_SNDFILE=ON"
            "-DWITH_INSTALL_PORTABLE=OFF"
            "-DWITH_FFTW3=ON"
            "-DWITH_SDL=OFF"
            "-DWITH_OPENCOLORIO=ON"
            "-DWITH_OPENSUBDIV=ON"
            "-DPYTHON_LIBRARY=${python.libPrefix}"
            "-DPYTHON_LIBPATH=${python}/lib"
            "-DPYTHON_INCLUDE_DIR=${python}/include/${python.libPrefix}"
            "-DPYTHON_VERSION=${python.pythonVersion}"
            "-DWITH_PYTHON_INSTALL=OFF"
            "-DWITH_PYTHON_INSTALL_NUMPY=OFF"
            "-DPYTHON_NUMPY_PATH=${python3Packages.numpy}/${python.sitePackages}"
            "-DWITH_OPENVDB=ON"
            "-DWITH_TBB=ON"
            "-DWITH_IMAGE_OPENJPEG=ON"
            "-DWITH_OPENCOLLADA=${if colladaSupport then "ON" else "OFF"}"
          ];

        NIX_CFLAGS_COMPILE = "-I${ilmbase.dev}/include/OpenEXR -I${python}/include/${python.libPrefix}";

        enableParallelBuilding = true;

        blenderExecutable =
          placeholder "out" + ("/bin/blender");
        # --python-expr is used to workaround https://developer.blender.org/T74304
        postInstall = ''
    wrapProgram $blenderExecutable \
      --prefix PYTHONPATH : ${python3Packages.numpy}/${python.sitePackages} \
      --add-flags '--python-use-system-env'
  '';

        meta = with stdenv.lib; {
          description = "3D Creation/Animation/Publishing System";
          homepage = "https://www.blender.org";
          # They comment two licenses: GPLv2 and Blender License, but they
          # say: "We've decided to cancel the BL offering for an indefinite period."
          license = licenses.gpl2Plus;
          platforms = [ "x86_64-linux" ];
          maintainers = with maintainers; [ goibhniu veprbl ];
        };
      });
    })
  ];
}
