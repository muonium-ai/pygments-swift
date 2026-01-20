import Foundation

public enum BuiltinLanguage: String, CaseIterable, Sendable {
    case swift
    case json
    case jsonld
    case python
    case javascript
    case java
    case typescript
    case c
    case cpp
    case csharp
    case go
    case rust
    case kotlin
    case ruby
    case php
    case shell
    case scala
    case r
    case markdown
    case yaml
    case toml
    case html
    case xml
    case css
    case sql
    case diff
    case dockerfile
    case makefile

    case lua
    case perl
    case haskell
    case clojure
    case elixir
    case erlang
    case dart
    case julia
    case powershell
    case groovy

    case cmake
    case graphql
    case terraform
    case nginx
    case objectivec
    case objectivecpp
    case vim
    case zig
    case nim
    case solidity

    case coffeescript
    case scss
    case less
    case haml
    case pug
    case protobuf
    case ocaml
    case fsharp
    case fortran
    case assembly

    case restructuredtext
    case latex
    case gitignore
    case editorconfig
    case properties
    case csv
    case graphviz
    case plantuml
    case mermaid
    case apacheconf

    case tcl
    case awk
    case sed
    case batchfile
    case vbscript
    case pascal
    case racket
    case scheme
    case commonlisp
    case emacslisp

    case d
    case crystal
    case elm
    case haxe
    case v
    case nix
    case fish
    case raku
    case cue
    case nushell

    case ada
    case cobol
    case prolog
    case smalltalk
    case eiffel
    case visualbasic
    case systemverilog
    case vhdl
    case llvm
    case glsl
}

public enum LexerRegistry {
    public static func makeLexer(language: BuiltinLanguage, options: LexerOptions = .init()) -> Lexer {
        switch language {
        case .swift:
            return SwiftLexer(options: options)
        case .json:
            return JsonLexer(options: options)
        case .jsonld:
            return JsonLdLexer(options: options)
        case .python:
            return PythonLexer(options: options)
        case .javascript:
            return JavaScriptLexer(options: options)
        case .java:
            return JavaLexer(options: options)
        case .typescript:
            return TypeScriptLexer(options: options)
        case .c:
            return CLexer(options: options)
        case .cpp:
            return CppLexer(options: options)
        case .csharp:
            return CSharpLexer(options: options)
        case .go:
            return GoLexer(options: options)
        case .rust:
            return RustLexer(options: options)
        case .kotlin:
            return KotlinLexer(options: options)
        case .ruby:
            return RubyLexer(options: options)
        case .php:
            return PHPLexer(options: options)
        case .shell:
            return ShellLexer(options: options)
        case .scala:
            return ScalaLexer(options: options)
        case .r:
            return RLexer(options: options)
        case .markdown:
            return MarkdownLexer(options: options)
        case .yaml:
            return YamlLexer(options: options)
        case .toml:
            return TomlLexer(options: options)
        case .html:
            return HtmlLexer(options: options)
        case .xml:
            return XmlLexer(options: options)
        case .css:
            return CssLexer(options: options)
        case .sql:
            return SqlLexer(options: options)
        case .diff:
            return DiffLexer(options: options)
        case .dockerfile:
            return DockerfileLexer(options: options)
        case .makefile:
            return MakefileLexer(options: options)

        case .lua:
            return LuaLexer(options: options)
        case .perl:
            return PerlLexer(options: options)
        case .haskell:
            return HaskellLexer(options: options)
        case .clojure:
            return ClojureLexer(options: options)
        case .elixir:
            return ElixirLexer(options: options)
        case .erlang:
            return ErlangLexer(options: options)
        case .dart:
            return DartLexer(options: options)
        case .julia:
            return JuliaLexer(options: options)
        case .powershell:
            return PowerShellLexer(options: options)
        case .groovy:
            return GroovyLexer(options: options)

        case .cmake:
            return CMakeLexer(options: options)
        case .graphql:
            return GraphQLLexer(options: options)
        case .terraform:
            return TerraformLexer(options: options)
        case .nginx:
            return NginxLexer(options: options)
        case .objectivec:
            return ObjectiveCLexer(options: options)
        case .objectivecpp:
            return ObjectiveCppLexer(options: options)
        case .vim:
            return VimLexer(options: options)
        case .zig:
            return ZigLexer(options: options)
        case .nim:
            return NimLexer(options: options)
        case .solidity:
            return SolidityLexer(options: options)

        case .coffeescript:
            return CoffeeScriptLexer(options: options)
        case .scss:
            return ScssLexer(options: options)
        case .less:
            return LessLexer(options: options)
        case .haml:
            return HamlLexer(options: options)
        case .pug:
            return PugLexer(options: options)
        case .protobuf:
            return ProtobufLexer(options: options)
        case .ocaml:
            return OcamlLexer(options: options)
        case .fsharp:
            return FSharpLexer(options: options)
        case .fortran:
            return FortranLexer(options: options)
        case .assembly:
            return AssemblyLexer(options: options)

        case .restructuredtext:
            return ReStructuredTextLexer(options: options)
        case .latex:
            return LatexLexer(options: options)
        case .gitignore:
            return GitIgnoreLexer(options: options)
        case .editorconfig:
            return EditorConfigLexer(options: options)
        case .properties:
            return PropertiesLexer(options: options)
        case .csv:
            return CsvLexer(options: options)
        case .graphviz:
            return GraphvizLexer(options: options)
        case .plantuml:
            return PlantUmlLexer(options: options)
        case .mermaid:
            return MermaidLexer(options: options)
        case .apacheconf:
            return ApacheConfLexer(options: options)

        case .tcl:
            return TclLexer(options: options)
        case .awk:
            return AwkLexer(options: options)
        case .sed:
            return SedLexer(options: options)
        case .batchfile:
            return BatchfileLexer(options: options)
        case .vbscript:
            return VBScriptLexer(options: options)
        case .pascal:
            return PascalLexer(options: options)
        case .racket:
            return RacketLexer(options: options)
        case .scheme:
            return SchemeLexer(options: options)
        case .commonlisp:
            return CommonLispLexer(options: options)
        case .emacslisp:
            return EmacsLispLexer(options: options)

        case .d:
            return DLexer(options: options)
        case .crystal:
            return CrystalLexer(options: options)
        case .elm:
            return ElmLexer(options: options)
        case .haxe:
            return HaxeLexer(options: options)
        case .v:
            return VLexer(options: options)
        case .nix:
            return NixLexer(options: options)
        case .fish:
            return FishLexer(options: options)
        case .raku:
            return RakuLexer(options: options)
        case .cue:
            return CueLexer(options: options)
        case .nushell:
            return NushellLexer(options: options)

        case .ada:
            return AdaLexer(options: options)
        case .cobol:
            return CobolLexer(options: options)
        case .prolog:
            return PrologLexer(options: options)
        case .smalltalk:
            return SmalltalkLexer(options: options)
        case .eiffel:
            return EiffelLexer(options: options)
        case .visualbasic:
            return VisualBasicLexer(options: options)
        case .systemverilog:
            return SystemVerilogLexer(options: options)
        case .vhdl:
            return VhdlLexer(options: options)
        case .llvm:
            return LlvmLexer(options: options)
        case .glsl:
            return GlslLexer(options: options)
        }
    }

    /// Returns a lexer for a language name (e.g. "python", "js", "swift").
    public static func makeLexer(languageName: String, options: LexerOptions = .init()) -> Lexer? {
        let normalized = languageName.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        switch normalized {
        case "swift":
            return makeLexer(language: .swift, options: options)
        case "json":
            return makeLexer(language: .json, options: options)
        case "jsonld", "json-ld":
            return makeLexer(language: .jsonld, options: options)
        case "python", "py":
            return makeLexer(language: .python, options: options)
        case "javascript", "js":
            return makeLexer(language: .javascript, options: options)
        case "java":
            return makeLexer(language: .java, options: options)
        case "typescript", "ts":
            return makeLexer(language: .typescript, options: options)
        case "c":
            return makeLexer(language: .c, options: options)
        case "c++", "cpp", "cplusplus", "cxx":
            return makeLexer(language: .cpp, options: options)
        case "c#", "csharp", "cs":
            return makeLexer(language: .csharp, options: options)
        case "go", "golang":
            return makeLexer(language: .go, options: options)
        case "rust", "rs":
            return makeLexer(language: .rust, options: options)
        case "kotlin", "kt":
            return makeLexer(language: .kotlin, options: options)
        case "ruby", "rb":
            return makeLexer(language: .ruby, options: options)
        case "php":
            return makeLexer(language: .php, options: options)
        case "shell", "sh", "bash", "zsh", "ksh":
            return makeLexer(language: .shell, options: options)
        case "scala", "sc":
            return makeLexer(language: .scala, options: options)
        case "r":
            return makeLexer(language: .r, options: options)
        case "markdown", "md":
            return makeLexer(language: .markdown, options: options)
        case "yaml", "yml":
            return makeLexer(language: .yaml, options: options)
        case "toml":
            return makeLexer(language: .toml, options: options)
        case "html", "xhtml":
            return makeLexer(language: .html, options: options)
        case "xml":
            return makeLexer(language: .xml, options: options)
        case "css":
            return makeLexer(language: .css, options: options)
        case "sql":
            return makeLexer(language: .sql, options: options)
        case "diff", "patch":
            return makeLexer(language: .diff, options: options)
        case "dockerfile", "docker":
            return makeLexer(language: .dockerfile, options: options)
        case "make", "makefile", "mk":
            return makeLexer(language: .makefile, options: options)

        case "lua":
            return makeLexer(language: .lua, options: options)
        case "perl", "pl":
            return makeLexer(language: .perl, options: options)
        case "haskell", "hs":
            return makeLexer(language: .haskell, options: options)
        case "clojure", "clj", "cljs":
            return makeLexer(language: .clojure, options: options)
        case "elixir", "ex":
            return makeLexer(language: .elixir, options: options)
        case "erlang", "erl":
            return makeLexer(language: .erlang, options: options)
        case "dart":
            return makeLexer(language: .dart, options: options)
        case "julia", "jl":
            return makeLexer(language: .julia, options: options)
        case "powershell", "pwsh", "ps":
            return makeLexer(language: .powershell, options: options)
        case "groovy", "gradle":
            return makeLexer(language: .groovy, options: options)

        case "cmake":
            return makeLexer(language: .cmake, options: options)
        case "graphql", "gql":
            return makeLexer(language: .graphql, options: options)
        case "terraform", "hcl", "tf":
            return makeLexer(language: .terraform, options: options)
        case "nginx":
            return makeLexer(language: .nginx, options: options)
        case "objective-c", "objectivec", "objc":
            return makeLexer(language: .objectivec, options: options)
        case "objective-c++", "objectivecpp", "objcpp":
            return makeLexer(language: .objectivecpp, options: options)
        case "vim", "vimscript":
            return makeLexer(language: .vim, options: options)
        case "zig":
            return makeLexer(language: .zig, options: options)
        case "nim":
            return makeLexer(language: .nim, options: options)
        case "solidity", "sol":
            return makeLexer(language: .solidity, options: options)

        case "coffeescript", "coffee":
            return makeLexer(language: .coffeescript, options: options)
        case "scss", "sass":
            return makeLexer(language: .scss, options: options)
        case "less":
            return makeLexer(language: .less, options: options)
        case "haml":
            return makeLexer(language: .haml, options: options)
        case "pug", "jade":
            return makeLexer(language: .pug, options: options)
        case "protobuf", "proto":
            return makeLexer(language: .protobuf, options: options)
        case "ocaml", "ml":
            return makeLexer(language: .ocaml, options: options)
        case "fsharp", "f#", "fs":
            return makeLexer(language: .fsharp, options: options)
        case "fortran", "f90":
            return makeLexer(language: .fortran, options: options)
        case "assembly", "asm":
            return makeLexer(language: .assembly, options: options)

        case "restructuredtext", "restructured-text", "rst", "rest":
            return makeLexer(language: .restructuredtext, options: options)
        case "latex", "tex":
            return makeLexer(language: .latex, options: options)
        case "gitignore", "git-ignore":
            return makeLexer(language: .gitignore, options: options)
        case "editorconfig", "editor-config":
            return makeLexer(language: .editorconfig, options: options)
        case "properties", "java-properties", "javaproperties":
            return makeLexer(language: .properties, options: options)
        case "csv":
            return makeLexer(language: .csv, options: options)
        case "graphviz", "dot":
            return makeLexer(language: .graphviz, options: options)
        case "plantuml", "puml":
            return makeLexer(language: .plantuml, options: options)
        case "mermaid", "mmd":
            return makeLexer(language: .mermaid, options: options)
        case "apache", "apacheconf", "httpd", "htaccess":
            return makeLexer(language: .apacheconf, options: options)

        case "tcl":
            return makeLexer(language: .tcl, options: options)
        case "awk", "gawk":
            return makeLexer(language: .awk, options: options)
        case "sed":
            return makeLexer(language: .sed, options: options)
        case "batch", "batchfile", "dosbatch", "bat", "cmd":
            return makeLexer(language: .batchfile, options: options)
        case "vbscript", "vbs":
            return makeLexer(language: .vbscript, options: options)
        case "pascal", "pas", "delphi":
            return makeLexer(language: .pascal, options: options)
        case "racket":
            return makeLexer(language: .racket, options: options)
        case "scheme", "scm":
            return makeLexer(language: .scheme, options: options)
        case "commonlisp", "common-lisp", "lisp":
            return makeLexer(language: .commonlisp, options: options)
        case "emacslisp", "emacs-lisp", "elisp":
            return makeLexer(language: .emacslisp, options: options)

        case "d", "dlang":
            return makeLexer(language: .d, options: options)
        case "crystal", "cr":
            return makeLexer(language: .crystal, options: options)
        case "elm":
            return makeLexer(language: .elm, options: options)
        case "haxe", "hx":
            return makeLexer(language: .haxe, options: options)
        case "v", "vlang":
            return makeLexer(language: .v, options: options)
        case "nix":
            return makeLexer(language: .nix, options: options)
        case "fish", "fishshell":
            return makeLexer(language: .fish, options: options)
        case "raku", "perl6", "perl-6", "p6":
            return makeLexer(language: .raku, options: options)
        case "cue", "cuelang":
            return makeLexer(language: .cue, options: options)
        case "nushell", "nu":
            return makeLexer(language: .nushell, options: options)

        case "ada":
            return makeLexer(language: .ada, options: options)
        case "cobol", "cbl":
            return makeLexer(language: .cobol, options: options)
        case "prolog":
            return makeLexer(language: .prolog, options: options)
        case "smalltalk", "st":
            return makeLexer(language: .smalltalk, options: options)
        case "eiffel":
            return makeLexer(language: .eiffel, options: options)
        case "visualbasic", "visual-basic", "vb", "vbnet", "vb.net":
            return makeLexer(language: .visualbasic, options: options)
        case "systemverilog", "system-verilog", "sv":
            return makeLexer(language: .systemverilog, options: options)
        case "vhdl":
            return makeLexer(language: .vhdl, options: options)
        case "llvm", "llvm-ir", "ir":
            return makeLexer(language: .llvm, options: options)
        case "glsl":
            return makeLexer(language: .glsl, options: options)
        default:
            return nil
        }
    }

    /// Returns a lexer for a filename extension (e.g. ".py", "js", "java").
    public static func makeLexer(filename: String, options: LexerOptions = .init()) -> Lexer? {
        let base = (filename as NSString).lastPathComponent.lowercased()
        switch base {
        case ".gitignore":
            return makeLexer(language: .gitignore, options: options)
        case ".editorconfig":
            return makeLexer(language: .editorconfig, options: options)
        case ".htaccess", "httpd.conf", "apache.conf", "apache2.conf":
            return makeLexer(language: .apacheconf, options: options)
        case "dockerfile":
            return makeLexer(language: .dockerfile, options: options)
        case "makefile":
            return makeLexer(language: .makefile, options: options)
        case "cmakelists.txt":
            return makeLexer(language: .cmake, options: options)
        case "nginx.conf":
            return makeLexer(language: .nginx, options: options)
        default:
            break
        }

        let ext = (filename as NSString).pathExtension.lowercased()
        switch ext {
        case "swift":
            return makeLexer(language: .swift, options: options)
        case "json", "jsonl", "ndjson":
            return makeLexer(language: .json, options: options)
        case "jsonld":
            return makeLexer(language: .jsonld, options: options)
        case "py":
            return makeLexer(language: .python, options: options)
        case "js", "mjs", "cjs":
            return makeLexer(language: .javascript, options: options)
        case "java":
            return makeLexer(language: .java, options: options)
        case "ts", "tsx":
            return makeLexer(language: .typescript, options: options)
        case "c", "h":
            return makeLexer(language: .c, options: options)
        case "cc", "cpp", "cxx", "hh", "hpp", "hxx":
            return makeLexer(language: .cpp, options: options)
        case "cs":
            return makeLexer(language: .csharp, options: options)
        case "go":
            return makeLexer(language: .go, options: options)
        case "rs":
            return makeLexer(language: .rust, options: options)
        case "kt", "kts":
            return makeLexer(language: .kotlin, options: options)
        case "rb", "erb", "rake":
            return makeLexer(language: .ruby, options: options)
        case "php", "phtml", "php8", "phpt":
            return makeLexer(language: .php, options: options)
        case "sh", "bash", "zsh", "ksh", "command":
            return makeLexer(language: .shell, options: options)
        case "scala", "sc", "sbt":
            return makeLexer(language: .scala, options: options)
        case "r", "rmd", "rnw":
            return makeLexer(language: .r, options: options)
        case "md", "markdown", "mdown", "mkd":
            return makeLexer(language: .markdown, options: options)
        case "yaml", "yml":
            return makeLexer(language: .yaml, options: options)
        case "toml":
            return makeLexer(language: .toml, options: options)
        case "html", "htm", "xhtml":
            return makeLexer(language: .html, options: options)
        case "xml":
            return makeLexer(language: .xml, options: options)
        case "css":
            return makeLexer(language: .css, options: options)
        case "sql":
            return makeLexer(language: .sql, options: options)
        case "diff", "patch":
            return makeLexer(language: .diff, options: options)
        case "lua":
            return makeLexer(language: .lua, options: options)
        case "pl", "pm":
            return makeLexer(language: .perl, options: options)
        case "hs":
            return makeLexer(language: .haskell, options: options)
        case "clj", "cljs", "cljc", "edn":
            return makeLexer(language: .clojure, options: options)
        case "ex", "exs":
            return makeLexer(language: .elixir, options: options)
        case "erl", "hrl":
            return makeLexer(language: .erlang, options: options)
        case "dart":
            return makeLexer(language: .dart, options: options)
        case "jl":
            return makeLexer(language: .julia, options: options)
        case "ps1", "psm1", "psd1":
            return makeLexer(language: .powershell, options: options)
        case "groovy", "gvy", "gy", "gradle":
            return makeLexer(language: .groovy, options: options)
        case "cmake":
            return makeLexer(language: .cmake, options: options)
        case "gql", "graphql":
            return makeLexer(language: .graphql, options: options)
        case "tf", "tfvars", "hcl":
            return makeLexer(language: .terraform, options: options)
        case "nginx":
            return makeLexer(language: .nginx, options: options)
        case "m", "mm":
            // Note: .m can also be MATLAB; we treat it as Objective-C here.
            return makeLexer(language: ext == "mm" ? .objectivecpp : .objectivec, options: options)
        case "vim":
            return makeLexer(language: .vim, options: options)
        case "zig":
            return makeLexer(language: .zig, options: options)
        case "nim":
            return makeLexer(language: .nim, options: options)
        case "sol":
            return makeLexer(language: .solidity, options: options)

        case "coffee", "litcoffee":
            return makeLexer(language: .coffeescript, options: options)
        case "scss", "sass":
            return makeLexer(language: .scss, options: options)
        case "less":
            return makeLexer(language: .less, options: options)
        case "haml":
            return makeLexer(language: .haml, options: options)
        case "pug", "jade":
            return makeLexer(language: .pug, options: options)
        case "proto":
            return makeLexer(language: .protobuf, options: options)
        case "ml", "mli":
            return makeLexer(language: .ocaml, options: options)
        case "fs", "fsx", "fsi":
            return makeLexer(language: .fsharp, options: options)
        case "f", "for", "ftn", "f77", "f90", "f95":
            return makeLexer(language: .fortran, options: options)
        case "s", "asm":
            return makeLexer(language: .assembly, options: options)

        case "rst", "rest":
            return makeLexer(language: .restructuredtext, options: options)
        case "tex", "latex", "ltx", "sty", "cls":
            return makeLexer(language: .latex, options: options)
        case "gitignore":
            return makeLexer(language: .gitignore, options: options)
        case "editorconfig":
            return makeLexer(language: .editorconfig, options: options)
        case "properties", "props":
            return makeLexer(language: .properties, options: options)
        case "csv":
            return makeLexer(language: .csv, options: options)
        case "dot", "gv":
            return makeLexer(language: .graphviz, options: options)
        case "puml", "plantuml", "pu":
            return makeLexer(language: .plantuml, options: options)
        case "mmd", "mermaid":
            return makeLexer(language: .mermaid, options: options)
        case "htaccess":
            return makeLexer(language: .apacheconf, options: options)

        case "tcl":
            return makeLexer(language: .tcl, options: options)
        case "awk":
            return makeLexer(language: .awk, options: options)
        case "sed":
            return makeLexer(language: .sed, options: options)
        case "bat", "cmd":
            return makeLexer(language: .batchfile, options: options)
        case "vbs":
            return makeLexer(language: .vbscript, options: options)
        case "pas", "pp", "dpr":
            return makeLexer(language: .pascal, options: options)
        case "rkt":
            return makeLexer(language: .racket, options: options)
        case "scm", "ss":
            return makeLexer(language: .scheme, options: options)
        case "lisp", "lsp":
            return makeLexer(language: .commonlisp, options: options)
        case "el", "elisp":
            return makeLexer(language: .emacslisp, options: options)

        case "d":
            return makeLexer(language: .d, options: options)
        case "cr":
            return makeLexer(language: .crystal, options: options)
        case "elm":
            return makeLexer(language: .elm, options: options)
        case "hx":
            return makeLexer(language: .haxe, options: options)
        case "v":
            return makeLexer(language: .v, options: options)
        case "nix":
            return makeLexer(language: .nix, options: options)
        case "fish":
            return makeLexer(language: .fish, options: options)
        case "raku", "p6", "pl6", "pm6":
            return makeLexer(language: .raku, options: options)
        case "cue":
            return makeLexer(language: .cue, options: options)
        case "nu":
            return makeLexer(language: .nushell, options: options)

        case "adb", "ads":
            return makeLexer(language: .ada, options: options)
        case "cob", "cbl":
            return makeLexer(language: .cobol, options: options)
        case "pro", "prolog":
            return makeLexer(language: .prolog, options: options)
        case "st":
            return makeLexer(language: .smalltalk, options: options)
        case "e":
            return makeLexer(language: .eiffel, options: options)
        case "vb":
            return makeLexer(language: .visualbasic, options: options)
        case "sv", "svh":
            return makeLexer(language: .systemverilog, options: options)
        case "vhd", "vhdl":
            return makeLexer(language: .vhdl, options: options)
        case "ll":
            return makeLexer(language: .llvm, options: options)
        case "glsl", "vert", "frag", "geom", "comp", "tesc", "tese":
            return makeLexer(language: .glsl, options: options)
        default:
            return nil
        }
    }
}
