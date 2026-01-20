import Foundation

/// Minimal Elixir lexer.
public final class ElixirLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                // Atoms
                Rule(":[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Constant"))),

                // Strings
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\b(?:-?\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule("\\b(?:defmodule|def|defp|do|end|if|else|case|when|cond|fn|receive|after|try|rescue|catch|raise|alias|import|require|use|with)\\b", action: .token(.keyword)),

                // Module aliases start uppercase
                Rule("[A-Z][A-Za-z0-9_]*(?:\\.[A-Z][A-Za-z0-9_]*)*", action: .token(.name.child("Class"))),

                Rule("[a-z_][A-Za-z0-9_]*[!?]?", action: .token(.name)),

                Rule("->|\\|>|==|!=|<=|>=|&&|\\|\\||[=+\\-*/<>!?:]", action: .token(.operator)),
                Rule("[()\\[\\]{}.,;]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
