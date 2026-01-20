import Foundation

/// Minimal Protocol Buffers (.proto) lexer.
public final class ProtobufLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),
                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),

                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),

                Rule("\\b(?:syntax|package|import|option|message|enum|service|rpc|returns|extend|extensions|reserved|oneof|map|repeated|optional|required)\\b", action: .token(.keyword)),
                Rule("\\b(?:double|float|int32|int64|uint32|uint64|sint32|sint64|fixed32|fixed64|sfixed32|sfixed64|bool|string|bytes)\\b", action: .token(.keyword.child("Type"))),

                Rule("\\b(?:true|false)\\b", action: .token(.keyword)),

                Rule("-?(?:0|[1-9]\\d*)", action: .token(.number)),

                Rule("[{}();=\\[\\],<>.]", action: .token(.punctuation)),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),
                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
