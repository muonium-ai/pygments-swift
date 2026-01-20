import Foundation

/// Pragmatic Arduino lexer (C/C++-like; smoke-test level).
public final class ArduinoLexer: RegexLexer {
    public override var tokenDefs: [String: [TokenRuleDef]] {
        let keywords = [
            "setup", "loop", "pinMode", "digitalWrite", "digitalRead", "analogRead", "analogWrite",
            "delay", "delayMicroseconds", "millis", "micros", "Serial",
        ]
        let keywordPattern = "\\b(?:" + keywords.joined(separator: "|") + ")\\b"

        return [
            "root": [
                .rule(Rule("\\n", action: .token(.whitespace))),
                .rule(Rule("[\\t\\f ]+", action: .token(.whitespace))),

                // Comments
                .rule(Rule("//[^\\n]*", action: .token(.comment.child("Single")))),
                .rule(Rule("/\\*", action: .token(.comment.child("Multiline")), newState: .ops([.push("comment")]))),

                // Preprocessor-ish
                .rule(Rule("#\\s*(?:include|define|if|ifdef|ifndef|elif|else|endif|pragma)\\b", action: .token(.comment.child("Preproc")))),

                // Strings / chars
                .rule(Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string))),
                .rule(Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string.child("Char")))),

                // Numbers
                .rule(Rule("\\b0[xX][0-9A-Fa-f]+\\b", action: .token(.number.child("Hex")))),
                .rule(Rule("\\b\\d+(?:\\.\\d+)?\\b", action: .token(.number))),

                // Arduino-specific keywords
                .rule(Rule(keywordPattern, action: .token(.name.child("Builtin")))),

                // Operators / punctuation
                .rule(Rule("[-+*/%]=?|==?=?|!=?=?|<=|>=|&&|\\|\\||<<=?|>>=?|[?:~^&|]", action: .token(.operator))),
                .rule(Rule("[{}\\[\\](),.;]", action: .token(.punctuation))),

                // Identifiers
                .rule(Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name))),

                .rule(Rule(".", action: .token(.text))),
            ],
            "comment": [
                .rule(Rule("\\*/", action: .token(.comment.child("Multiline")), newState: .ops([.pop]))),
                .rule(Rule("[^*]+", action: .token(.comment.child("Multiline")))),
                .rule(Rule("\\*", action: .token(.comment.child("Multiline")))),
            ],
        ]
    }
}
