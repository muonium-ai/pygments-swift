import Foundation

/// Pragmatic shell lexer (Bash/sh/zsh) (smoke-test level).
///
/// Highlights common shell tokens: comments, strings, variables, keywords,
/// numbers, operators and a best-effort for command words.
public final class ShellLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = RegexHelpers.words([
            "if", "then", "elif", "else", "fi",
            "for", "while", "until", "do", "done",
            "case", "esac", "in", "select",
            "function", "time", "coproc",
            "break", "continue", "return", "exit",
        ], suffix: "\\b")

        let builtins = RegexHelpers.words([
            ".", "source",
            "alias", "bg", "bind", "builtin", "caller", "cd", "command", "compgen", "complete",
            "declare", "dirs", "disown", "echo", "enable", "eval", "exec", "export", "fc",
            "fg", "getopts", "hash", "help", "history", "jobs", "kill", "let", "local",
            "logout", "popd", "printf", "pushd", "pwd", "read", "readonly", "set", "shift",
            "shopt", "test", "times", "trap", "type", "typeset", "ulimit", "umask", "unalias",
            "unset", "wait",
        ], suffix: "\\b")

        let varName = #"[A-Za-z_][A-Za-z0-9_]*"#

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Shebang
                .rule(Rule("^#![^\\n]*", action: .token(.comment.child("Preproc")))),

                // Comments
                .rule(Rule("#[^\\n]*", action: .token(.comment.child("Single")))),

                // Strings
                .rule(Rule("'", action: .token(.string), newState: .ops([.push("sq")]))),
                .rule(Rule("\"", action: .token(.string), newState: .ops([.push("dq")]))),
                .rule(Rule("`", action: .token(.string.child("Backtick")), newState: .ops([.push("bq")]))),

                // Keywords / builtins
                .rule(Rule(keywords, action: .token(.keyword))),
                .rule(Rule(builtins, action: .token(.name.child("Builtin")))),

                // Variables / parameters
                .rule(Rule("\\$\\{[^}]+\\}", action: .token(.name.child("Variable")))),
                .rule(Rule("\\$\\(" , action: .token(.punctuation), newState: .ops([.push("cmdsub")]))),
                .rule(Rule("\\$" + varName, action: .token(.name.child("Variable")))),
                .rule(Rule("\\$[0-9]+", action: .token(.name.child("Variable")))),
                .rule(Rule("\\$[?@*#$!-]", action: .token(.name.child("Variable")))),

                // Numbers
                .rule(Rule("\\b\\d+\\b", action: .token(.number))),

                // Operators / punctuation / redirections
                .rule(Rule("(\\|\\||&&|\\|\\||;;|;|&|\\(|\\)|\\{|\\}|\\[|\\]|\\\\)", action: .token(.punctuation))),
                .rule(Rule("(<<<|<<-|<<|>>|>|<|<&|>&)", action: .token(.operator))),

                // Words (commands, flags, paths). Treat as Name by default.
                .rule(Rule("-[A-Za-z0-9_\\-]+", action: .token(.name.child("Attribute")))),
                .rule(Rule("[A-Za-z0-9_./:+\\-]+", action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],

            "sq": [
                .rule(Rule("'", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule("[^']+", action: .token(.string))),
            ],

            "dq": [
                .rule(Rule("\"", action: .token(.string), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule("\\$\\{[^}]+\\}", action: .token(.name.child("Variable")))),
                .rule(Rule("\\$" + varName, action: .token(.name.child("Variable")))),
                .rule(Rule("\\$[0-9]+", action: .token(.name.child("Variable")))),
                .rule(Rule("\\$[?@*#$!-]", action: .token(.name.child("Variable")))),
                .rule(Rule(#"[^\\\\\"$]+"#, action: .token(.string))),
                .rule(Rule("\\$", action: .token(.string))),
                .rule(Rule("\\\\", action: .token(.string))),
            ],

            "bq": [
                .rule(Rule("`", action: .token(.string.child("Backtick")), newState: .ops([.pop]))),
                .rule(Rule(#"\\\\(?:.|\n)"#, action: .token(.string.child("Escape")))),
                .rule(Rule(#"[^\\\\`]+"#, action: .token(.string.child("Backtick")))),
                .rule(Rule("\\\\", action: .token(.string.child("Backtick")))),
            ],

            // Command substitution: treat similarly to root until we see a ')'.
            "cmdsub": [
                .rule(Rule("\\)", action: .token(.punctuation), newState: .ops([.pop]))),
                .include("root"),
            ],
        ]
    }
}
