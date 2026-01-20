import Foundation

/// Pragmatic Meson build-language lexer (smoke-test level).
public final class MesonLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            // statements / functions (common)
            "project", "executable", "library", "shared_library", "static_library",
            "dependency", "declare_dependency", "subdir", "subproject",
            "add_project_arguments", "add_project_link_arguments",
            "install_headers", "install_data", "configure_file",
            "files", "run_command", "test", "benchmark",
            "message", "warning", "error",
            "get_option", "option", "find_program", "import",
            "host_machine", "build_machine", "target_machine", "meson",
            // booleans
            "true", "false",
            // operators
            "and", "or", "not", "in",
        ], suffix: "\\b")

        let ident = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("\\\"(?:[^\\\"\\\\]|\\\\.)*\\\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string))),

                // Numbers
                .rule(Rule("[-+]?(?:\\d+)(?:\\.\\d+)?", action: .token(.number))),

                // Keywords
                .rule(Rule(keywords, action: .token(.keyword))),

                // Identifiers
                .rule(Rule(ident, action: .token(.name))),

                // Punctuation / operators
                .rule(Rule("[()\\[\\]\\{\\},:.;]", action: .token(.punctuation))),
                .rule(Rule("(==|!=|<=|>=)", action: .token(.operator))),
                .rule(Rule("[=+\\-*/%<>&|^~!?]+", action: .token(.operator))),

                .rule(Rule(".", action: .token(.text))),
            ],
        ]
    }
}
