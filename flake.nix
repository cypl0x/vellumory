{
  description = "Vellumory: Lean PDF tooling and native Emacs integration";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    lean-cli = {
      url = "github:leanprover/lean4-cli/6b907cf12b2e445ccb7c24bc208ef04a1f39e84c";
      flake = false;
    };
    # Its own nixpkgs is kept: emacs-git-pgtk here evaluates to the Emacs 32.0.50
    # (master of 2026-08-08) the GUI and Doom reviews run on.
    emacs-overlay.url = "github:nix-community/emacs-overlay/6c66dd7f97b5afdbd20a95988ff3c7bd4fb4bfb0";
    # Emacs master of 2026-10-27, with canvas images (d5f515e, 2026-08-16): a
    # second Emacs 32 to test against (=VELLUMORY_EMACS_MASTER=), cached by
    # nix-community; the pin above stays the main test build.
    emacs-master.url = "github:nix-community/emacs-overlay/144bf3e9b1e366fbb60660145896d39ad1c0af9c";
    doom = {
      url = "github:marienz/nix-doom-emacs-unstraightened/413ef4bbedd5cd107ba0dfb2dcb199ee789c0619";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.emacs-overlay.follows = "emacs-overlay";
    };
    # The PyMuPDF of May 2022 (1.19.6, on Python 3.10 for the server's
    # `match'), for the screen benchmark's pdf-tools with the vimura server of
    # that month: PyMuPDF 1.27 refuses the extensionless file it saves each
    # page to.
    nixpkgs-2205.url = "github:NixOS/nixpkgs/nixos-22.05";
  };
  outputs =
    {
      nixpkgs,
      lean-cli,
      emacs-overlay,
      emacs-master,
      doom,
      nixpkgs-2205,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      each = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs { inherit system; }));
      # The Emacs Lisp package, byte-compiled by Emacs' own package builder.
      # What building the worker and running its tests need, for both shells.
      buildEnvFor = pkgs: {
        VELLUMORY_CLI_SRC = lean-cli;
        VELLUMORY_EVIL_LISP = "${pkgs.emacsPackages.evil}/share/emacs/site-lisp/elpa/evil-${pkgs.emacsPackages.evil.version}";
        VELLUMORY_PACKAGE_LINT_LISP = "${pkgs.emacsPackages.package-lint}/share/emacs/site-lisp/elpa/package-lint-${pkgs.emacsPackages.package-lint.version}";
        VELLUMORY_ORG_NOTER_LISP = "${pkgs.emacsPackages.org-noter}/share/emacs/site-lisp/elpa/org-noter-${pkgs.emacsPackages.org-noter.version}";
        VELLUMORY_OX_TYPST_LISP = "${pkgs.emacsPackages.ox-typst}/share/emacs/site-lisp/elpa/ox-typst-${pkgs.emacsPackages.ox-typst.version}";
        VELLUMORY_MUPDF_INCLUDE = "${pkgs.mupdf.dev}/include";
        VELLUMORY_MUPDF_LIB = "${pkgs.lib.getLib pkgs.mupdf}/lib";
        VELLUMORY_OPENSSL_INCLUDE = "${pkgs.openssl.dev}/include";
        VELLUMORY_OPENSSL_LIB = "${pkgs.lib.getLib pkgs.openssl}/lib";
        VELLUMORY_SYNCTEX_INCLUDE = "${pkgs.texlive.bin.core.dev}/include";
        VELLUMORY_SYNCTEX_LIB = "${pkgs.texlive.bin.core}/lib";
        VELLUMORY_EXEMPI_INCLUDE = "${pkgs.exempi}/include/exempi-2.0";
        VELLUMORY_EXEMPI_LIB = "${pkgs.lib.getLib pkgs.exempi}/lib";
      };
      lispFor =
        pkgs: epkgs:
        epkgs.trivialBuild {
          pname = "vellumory";
          version = "0.1.0";
          src = ./lisp;
          nativeBuildInputs = [ pkgs.texinfo ];
          # The autoloads are generated, not committed, and the manual is built
          # beside the Lisp, where `vellumory-info' looks first.
          postInstall = ''
            cd $out/share/emacs/site-lisp
            emacs --batch -Q --eval '(loaddefs-generate "." (expand-file-name "vellumory-autoloads.el") (list "vellumory-autoloads.el") nil nil t)'
            makeinfo --no-split -o vellumory.info ${./docs/vellumory.texi}
          '';
        };
      # Doom with the review profile in tests/doomdir and the :tools vellumory
      # module from doom/, on the pinned Emacs 32 build.
      doomFor =
        system: pkgs:
        let
          withDoom = import nixpkgs {
            inherit system;
            overlays = [ doom.overlays.default ];
          };
          doomDir = pkgs.linkFarm "vellumory-doomdir" [
            {
              name = "init.el";
              path = ./tests/doomdir/init.el;
            }
            {
              name = "config.el";
              path = ./tests/doomdir/config.el;
            }
            {
              name = "packages.el";
              path = ./tests/doomdir/packages.el;
            }
            {
              name = "modules/tools/vellumory";
              path = ./doom/tools/vellumory;
            }
          ];
        in
        withDoom.emacsWithDoom {
          inherit doomDir;
          doomLocalDir = "~/.local/share/vellumory-doom-review";
          profileName = "vellumory-review";
          emacs = emacs-overlay.packages.${system}.emacs-git-pgtk;
          extraPackages = epkgs: [ (lispFor pkgs epkgs) ];
          extraBinPackages = [ (packageFor pkgs) ];
        };
      # The worker and the Lisp, with the programs the worker runs. Overridable:
      # .override { tesseractLanguages = [ "eng" "deu" ]; } keeps only those of
      # Tesseract's OCR languages (all of them take a gigabyte).
      packageFor =
        pkgs:
        pkgs.lib.makeOverridable (
          {
            tesseractLanguages ? null,
          }:
          let
            ocrmypdf =
              if tesseractLanguages == null then
                pkgs.ocrmypdf
              else
                pkgs.ocrmypdf.override {
                  # osd: orientation and script detection, which OCRmyPDF uses.
                  tesseract = pkgs.tesseract.override {
                    enableLanguages = pkgs.lib.unique (tesseractLanguages ++ [ "osd" ]);
                  };
                };
          in
          builtins.derivation {
            name = "vellumory-0.1.0";
            system = pkgs.stdenv.hostPlatform.system;
            # Source preparation, build, tests and installation are Lean code.
            builder = "${pkgs.lean4}/bin/lean";
            args = [
              "--run"
              ./scripts/Build.lean
              "nix"
            ];
            src = pkgs.lib.cleanSourceWith {
              src = ./.;
              filter =
                path: type:
                !(builtins.elem (builtins.baseNameOf path) [
                  ".git"
                  ".lake"
                  "target"
                  "vendor"
                ])
                && !(pkgs.lib.hasSuffix ".elc" path)
                && pkgs.lib.cleanSourceFilter path type;
            };
            VELLUMORY_CLI_SRC = lean-cli;
            VELLUMORY_QPDF = "${pkgs.qpdf}/bin/qpdf";
            VELLUMORY_VERAPDF = "${pkgs.verapdf}/bin/verapdf";
            VELLUMORY_OCRMYPDF = "${ocrmypdf}/bin/ocrmypdf";
            VELLUMORY_FIX_QDF = "${pkgs.qpdf}/bin/fix-qdf";
            VELLUMORY_EVIL_LISP = "${pkgs.emacsPackages.evil}/share/emacs/site-lisp/elpa/evil-${pkgs.emacsPackages.evil.version}";
            VELLUMORY_OX_TYPST_LISP = "${pkgs.emacsPackages.ox-typst}/share/emacs/site-lisp/elpa/ox-typst-${pkgs.emacsPackages.ox-typst.version}";
            VELLUMORY_ORG_NOTER_LISP = "${pkgs.emacsPackages.org-noter}/share/emacs/site-lisp/elpa/org-noter-${pkgs.emacsPackages.org-noter.version}";
            VELLUMORY_MUPDF_INCLUDE = "${pkgs.mupdf.dev}/include";
            VELLUMORY_MUPDF_LIB = "${pkgs.lib.getLib pkgs.mupdf}/lib";
            VELLUMORY_OPENSSL_INCLUDE = "${pkgs.openssl.dev}/include";
            VELLUMORY_OPENSSL_LIB = "${pkgs.lib.getLib pkgs.openssl}/lib";
            VELLUMORY_SYNCTEX_INCLUDE = "${pkgs.texlive.bin.core.dev}/include";
            VELLUMORY_SYNCTEX_LIB = "${pkgs.texlive.bin.core}/lib";
            VELLUMORY_EXEMPI_INCLUDE = "${pkgs.exempi}/include/exempi-2.0";
            VELLUMORY_EXEMPI_LIB = "${pkgs.lib.getLib pkgs.exempi}/lib";
            VELLUMORY_SYNCTEX_FIXTURE = synctexFixtureFor pkgs;
            VELLUMORY_PDFA4_FIXTURE = pdfa4FixtureFor pkgs;
            VELLUMORY_SIGNED_FIXTURE = signedFixtureFor pkgs;
            VELLUMORY_EPUB_FIXTURE = epubFixtureFor pkgs;
            VELLUMORY_CONVERT_FIXTURES = convertFixturesFor pkgs;
            VELLUMORY_FORM_FIXTURE = formFixtureFor pkgs;
            # coreutils: Dired lists directories with ls.
            # texinfo: the manual is built into the package (makeinfo).
            PATH = pkgs.lib.makeBinPath [
              pkgs.lean4
              pkgs.stdenv.cc
              pkgs.binutils
              pkgs.coreutils
              pkgs.qpdf
              pkgs.emacs-nox
              (exportTexFor pkgs)
              pkgs.typst
              pkgs.ghostscript
              pkgs.djvulibre
              pkgs.texinfo
            ];
            LEAN_CC = "${pkgs.stdenv.cc}/bin/cc";
          }
        ) { };
      # TeX Live for Org export to tagged PDF 2.0 (PDF/UA-2, PDF/A-4f): LaTeX
      # 2025-11-01 with the tagging code and the packages Org's preamble loads.
      exportTexFor =
        pkgs:
        pkgs.texliveBasic.withPackages (
          ps: with ps; [
            latex-lab
            tagpdf
            pdfmanagement-testphase
            luamml
            lm
            fontspec
            amsfonts
            wrapfig
            ulem
            capt-of
            latexmk
          ]
        );
      # Tagged PDF 2.0, PDF/UA-2 and PDF/A-4f, compiled by the export TeX Live.
      pdfa4FixtureFor =
        pkgs:
        pkgs.runCommand "vellumory-pdfa4-fixture"
          {
            nativeBuildInputs = [ (exportTexFor pkgs) ];
            SOURCE_DATE_EPOCH = "0";
            FORCE_SOURCE_DATE = "1";
          }
          ''
            export HOME=$TMPDIR TEXMFVAR=$TMPDIR/texmf-var
            cp ${./tests/fixtures/pdfa4/doc.tex} doc.tex
            lualatex -interaction=nonstopmode -halt-on-error doc.tex
            mkdir -p $out && cp doc.pdf $out/doc.pdf
          '';
      # A form made by hyperref under the export TeX Live: text, checkbox, combo
      # box and a read-only text field.
      formFixtureFor =
        pkgs:
        pkgs.runCommand "vellumory-form-fixture"
          {
            nativeBuildInputs = [ (exportTexFor pkgs) ];
            SOURCE_DATE_EPOCH = "0";
            FORCE_SOURCE_DATE = "1";
          }
          ''
            export HOME=$TMPDIR TEXMFVAR=$TMPDIR/texmf-var
            cp ${./tests/fixtures/form/form.tex} form.tex
            lualatex -interaction=nonstopmode -halt-on-error form.tex
            mkdir -p $out && cp form.pdf $out/form.pdf
          '';
      # Documents DocView reads through converters: DVI (latex), PostScript (dvips,
      # as TeX writes it) and DjVu (a page rendered by ghostscript, c44).
      convertFixturesFor =
        pkgs:
        pkgs.runCommand "vellumory-convert-fixtures"
          {
            nativeBuildInputs = [
              (exportTexFor pkgs)
              pkgs.ghostscript
              pkgs.djvulibre
            ];
            SOURCE_DATE_EPOCH = "0";
            FORCE_SOURCE_DATE = "1";
          }
          ''
            export HOME=$TMPDIR TEXMFVAR=$TMPDIR/texmf-var
            mkdir -p $out && cd $TMPDIR
            cp ${./tests/fixtures/convert/doc.tex} doc.tex
            cp ${./tests/fixtures/convert/short.tex} short.tex
            latex -interaction=nonstopmode -halt-on-error doc.tex
            latex -interaction=nonstopmode -halt-on-error short.tex
            dvipdfmx -q -o doc.pdf doc.dvi
            dvips -q -o doc.ps doc.dvi
            gs -q -dSAFER -dBATCH -dNOPAUSE -sDEVICE=ppmraw -r100 -dFirstPage=1 -dLastPage=1 -sOutputFile=page.ppm doc.pdf
            c44 page.ppm doc.djvu
            cp doc.dvi short.dvi doc.ps doc.djvu $out/
          '';
      # A reflowable book (EPUB 3, by pandoc) for layout, text size and stylesheets.
      epubFixtureFor =
        pkgs:
        pkgs.runCommand "vellumory-epub-fixture"
          {
            nativeBuildInputs = [ pkgs.pandoc ];
            SOURCE_DATE_EPOCH = "0";
          }
          ''
            mkdir -p $out
            pandoc ${./tests/fixtures/epub/book.md} -o $out/book.epub
          '';
      # A signed PDF: a test certificate (self-signed), a signature field added
      # to the SyncTeX fixture by qpdf (tests/fixtures/signature/field.json), and
      # a signature by MuPDF's mutool.
      signedFixtureFor =
        pkgs:
        pkgs.runCommand "vellumory-signed-fixture"
          {
            nativeBuildInputs = [
              pkgs.openssl
              pkgs.qpdf
              pkgs.mupdf
            ];
          }
          ''
            mkdir -p $out && cd $TMPDIR
            openssl req -x509 -newkey rsa:2048 -nodes -keyout key.pem -out cert.pem -days 36500 \
              -subj "/CN=Vellumory Test Signer/O=Vellumory"
            openssl pkcs12 -export -inkey key.pem -in cert.pem -out cert.pfx -passout pass:test
            cp ${synctexFixtureFor pkgs}/doc.pdf base.pdf && chmod u+w base.pdf
            qpdf base.pdf --update-from-json=${./tests/fixtures/signature/field.json} $out/unsigned.pdf
            mutool sign -s cert.pfx -P test -o $out/signed.pdf $out/unsigned.pdf
            cp cert.pem $out/cert.pem
          '';
      # A LaTeX document compiled with SyncTeX by the pinned TeX Live; the
      # recorded source path is this store path, valid wherever tests run.
      synctexFixtureFor =
        pkgs:
        pkgs.runCommand "vellumory-synctex-fixture"
          {
            nativeBuildInputs = [ pkgs.texliveBasic ];
            SOURCE_DATE_EPOCH = "0";
            FORCE_SOURCE_DATE = "1";
          }
          ''
            mkdir -p $out && cd $out
            cp ${./tests/fixtures/synctex/doc.tex} doc.tex
            pdflatex -interaction=nonstopmode -halt-on-error -synctex=1 doc.tex
            rm doc.aux doc.log
          '';
      # Pinned real-world documents for benchmarks and GUI review. Nothing here
      # is redistributed; fetchurl verifies each file against its fixed hash.
      # The Emacs Reader (MuPDF dynamic module, EmacsConf 2025), pinned for the
      # comparative benchmark; it is not in nixpkgs.
      emacsReaderFor =
        pkgs:
        pkgs.stdenv.mkDerivation {
          pname = "emacs-reader";
          version = "0-unstable-4fca3aa";
          src = pkgs.fetchgit {
            url = "https://codeberg.org/MonadicSheep/emacs-reader.git";
            rev = "4fca3aaedddeea7f672ee97e5a91ef3002240260";
            hash = "sha256-P1dFDtoioh6uPzUMvoDRVcr2Ph6UipXu2EkyOGvb3h0=";
          };
          nativeBuildInputs = [ pkgs.gnumake ];
          buildInputs = [ pkgs.mupdf ];
          # Only the module; its Makefile's Lisp targets need a writable HOME.
          buildPhase = "make render-core.so USE_PKGCONFIG=no";
          installPhase = "mkdir -p $out && cp *.el render-core.so $out/";
        };

      corpusFor =
        pkgs:
        pkgs.linkFarm "vellumory-corpus" [
          {
            name = "pdf32000-2008.pdf";
            path = pkgs.fetchurl {
              url = "https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/PDF32000_2008.pdf";
              sha256 = "1zqp2hsk11ydjypc6gzwmivc6vpbx1dkb2nlifg21mkhhngcmq4x";
            };
          }
          {
            name = "tracemonkey.pdf";
            path = pkgs.fetchurl {
              url = "https://raw.githubusercontent.com/mozilla/pdf.js/279175b948b23077808d744435c07b32d23aa5bc/test/pdfs/tracemonkey.pdf";
              sha256 = "04z1cz9vwyjxjvs06czqs9zr2arvihfk1gsj0m910n28kr8zyqin";
            };
          }
          {
            name = "alphatrans.pdf";
            path = pkgs.fetchurl {
              url = "https://raw.githubusercontent.com/mozilla/pdf.js/279175b948b23077808d744435c07b32d23aa5bc/test/pdfs/alphatrans.pdf";
              sha256 = "0qwgaszs45sim841a3m8w8mvkvkhc6vd421v2cblv3s5x6w0lk13";
            };
          }
        ];
    in
    {
      packages = nixpkgs.lib.genAttrs systems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = packageFor pkgs;
          vellumory = packageFor pkgs;
          lisp = pkgs.emacsPackages.callPackage (
            { trivialBuild }: lispFor pkgs { inherit trivialBuild; }
          ) { };
          corpus = corpusFor pkgs;
          synctex-fixture = synctexFixtureFor pkgs;
          pdfa4-fixture = pdfa4FixtureFor pkgs;
          signed-fixture = signedFixtureFor pkgs;
          form-fixture = formFixtureFor pkgs;
          doom-review = doomFor system pkgs;
        }
      );
      devShells = each (
        pkgs:
        let
          shells = {
            default = pkgs.mkShell (
              buildEnvFor pkgs
              // {
                packages = [
                  pkgs.lean4
                  pkgs.qpdf
                  pkgs.poppler-utils
                  pkgs.mupdf
                  pkgs.tesseract
                  pkgs.imagemagick
                  pkgs.xdotool
                  pkgs.emacs-pgtk
                  pkgs.emacsPackages.nael
                  pkgs.emacsPackages.evil
                  pkgs.emacsPackages.pdf-tools
                  pkgs.jdk
                  pkgs.git
                  pkgs.verapdf
                  pkgs.ocrmypdf
                  (exportTexFor pkgs)
                  pkgs.xvfb
                  pkgs.typst
                  pkgs.ghostscript
                  pkgs.djvulibre
                  pkgs.ffmpeg-full
                  # Reviews under Wayland: a headless compositor, screenshots, keys.
                  pkgs.sway
                  pkgs.grim
                  pkgs.wtype
                  # README images: lossless PNG optimization.
                  pkgs.oxipng
                  # The manual, and `lean --run scripts/Build.lean lint'.
                  pkgs.texinfo
                  pkgs.nixfmt
                  pkgs.statix
                  pkgs.deadnix
                  pkgs.typos
                  pkgs.vale
                  pkgs.actionlint
                  pkgs.zizmor
                ];
                VELLUMORY_CORPUS = corpusFor pkgs;
                VELLUMORY_PDF_TOOLS_LISP = "${pkgs.emacsPackages.pdf-tools}/share/emacs/site-lisp/elpa/pdf-tools-${pkgs.emacsPackages.pdf-tools.version}";
                VELLUMORY_TABLIST_LISP = "${pkgs.emacsPackages.tablist}/share/emacs/site-lisp/elpa/tablist-${pkgs.emacsPackages.tablist.version}";
                VELLUMORY_READER_LISP = "${emacsReaderFor pkgs}";
                VELLUMORY_EMACS_MASTER = "${
                  emacs-master.packages.${pkgs.stdenv.hostPlatform.system}.emacs-git-pgtk
                }/bin/emacs";
                # doc-tools (dalanicolai): its last complete version, main, with the
                # MuPDF and poppler backends it loads from the author's own paths
                # (its dev branch, 2026-05, opens only the author's test book).
                VELLUMORY_DOC_TOOLS = pkgs.lib.concatStringsSep ":" [
                  (pkgs.fetchFromGitHub {
                    owner = "dalanicolai";
                    repo = "doc-tools";
                    rev = "a9c1b0fa9ee60a86f48d4d0ac0844285e70b45b8";
                    hash = "sha256-LDpRpfggt1RROVzS2GQzXM2zmU77x2XdLTLRJ/dAHZY=";
                  })
                  (pkgs.fetchFromGitHub {
                    owner = "dalanicolai";
                    repo = "doc-tools-mupdf";
                    rev = "e6147524a2076f0de4a7f4f12cae3261918569ff";
                    hash = "sha256-l4Xpl+nQNa9BEQgMbzRIwrNqT8v5ph6elTRDRujqJ7E=";
                  })
                  (pkgs.fetchFromGitHub {
                    owner = "dalanicolai";
                    repo = "doc-tools-poppler";
                    rev = "0f5f086b675345cdbc0c29662ceca75c0ed9fe42";
                    hash = "sha256-mO0n/RPazma+CA2svyLN02Ezj2/6MjkxZKf4unbTuKk=";
                  })
                ];
                # pdf-tools with the vimura server (dalanicolai's fork, production
                # branch, and vimura-server 1.3 from PyPI: PyMuPDF instead of
                # epdfinfo): "FORK-LISP:PYTHON-BIN:USER-BASE:IMAGE-ROLL".  The fork finds
                # the server's startup file in Python's user site, hence the user base;
                # its pdf-view requires the author's image-roll of that month.
                VELLUMORY_VIMURA =
                  let
                    old = nixpkgs-2205.legacyPackages.${pkgs.stdenv.hostPlatform.system};
                    # Its wrapper would set PYTHONNOUSERSITE, hiding the server.
                    python = old.python310.buildEnv.override {
                      extraLibs = [
                        old.python310Packages.pymupdf
                        old.python310Packages.pillow
                        old.python310Packages.pytz
                      ];
                      permitUserSite = true;
                    };
                    fork = pkgs.fetchFromGitHub {
                      owner = "dalanicolai";
                      repo = "pdf-tools";
                      rev = "6808471a51800a224b94df7cf7542716e0596826";
                      hash = "sha256-/96LEPMCdhZUcNQYZC6M7eQJgQ3XKH+rTCkTDCaf4n0=";
                    };
                    userBase = pkgs.runCommand "vimura-server-1.3-user-base" { nativeBuildInputs = [ pkgs.unzip ]; } ''
                      site=$out/lib/python${old.python310.pythonVersion}/site-packages
                      mkdir -p $site
                      unzip -q ${
                        pkgs.fetchurl {
                          url = "https://files.pythonhosted.org/packages/14/d6/64bddf28be6b8384905f6bc6dc5727e5e51d46afecb5c27aafb1bee855d8/vimura_server-1.3-py3-none-any.whl";
                          hash = "sha256-LkGSPAvLQ7fmLZjOI6GTwSEI6o7VbdGz/joFGCSu1I0=";
                        }
                      } 'vimura_server/*' -d $site
                    '';
                    imageRoll = pkgs.fetchFromGitHub {
                      owner = "dalanicolai";
                      repo = "image-roll.el";
                      rev = "c05cb95d767903c0d74193f25398fc6222ab6f7f";
                      hash = "sha256-ADfFo7FxrbpS9j/JoLIkcdvtl6aB7VJw63glltdB6ic=";
                    };
                  in
                  "${fork}/lisp:${python}/bin:${userBase}:${imageRoll}";
                # EAF's PDF viewer (PyMuPDF in Qt, outside Emacs' display), for the screen benchmark.
                # Symbols Nerd Font for the review Dooms' mode line and icons.
                VELLUMORY_NERD_FONTS = "${pkgs.nerd-fonts.symbols-only}/share/fonts";
                VELLUMORY_EAF_EMACS = "${
                  (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (e: [
                    (e.eaf.withApplications [ e.eaf-pdf-viewer ])
                  ])
                }/bin/emacs";
                # Desktop readers for the screen benchmark: GNOME's Papers and Evince, KDE's Okular.
                VELLUMORY_PAPERS = "${pkgs.papers}/bin/papers";
                VELLUMORY_EVINCE = "${pkgs.evince}/bin/evince";
                VELLUMORY_OKULAR = "${pkgs.kdePackages.okular}/bin/okular";
                VELLUMORY_SYNCTEX_FIXTURE = synctexFixtureFor pkgs;
                VELLUMORY_PDFA4_FIXTURE = pdfa4FixtureFor pkgs;
                VELLUMORY_SIGNED_FIXTURE = signedFixtureFor pkgs;
                VELLUMORY_FORM_FIXTURE = formFixtureFor pkgs;
                VELLUMORY_EPUB_FIXTURE = epubFixtureFor pkgs;
                VELLUMORY_CONVERT_FIXTURES = convertFixturesFor pkgs;
              }
            );
            # CI: building the worker, its tests and the lints, without the
            # development shell's viewers, benchmarks and TeX Live.
            ci = pkgs.mkShell (
              buildEnvFor pkgs
              // {
                packages = [
                  pkgs.lean4
                  pkgs.qpdf
                  pkgs.emacs-nox
                  pkgs.git
                  pkgs.texinfo
                  pkgs.nixfmt
                  pkgs.statix
                  pkgs.deadnix
                  pkgs.typos
                  pkgs.vale
                  pkgs.actionlint
                  pkgs.zizmor
                ];
              }
              # macOS: Emacs's Cocoa build, for the GUI suite (Review.lean guidirect).
              // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
                VELLUMORY_GUI_EMACS = "${pkgs.emacs}/bin/emacs";
              }
            );
          };
        in
        # The default shell reviews under X and Wayland and measures desktop
        # readers, all Linux programs: on macOS it is the CI shell, which builds
        # the worker and runs the tests.
        shells // pkgs.lib.optionalAttrs (!pkgs.stdenv.hostPlatform.isLinux) { default = shells.ci; }
      );
      checks = each (pkgs: {
        tooling = packageFor pkgs;
      });
    };
}
