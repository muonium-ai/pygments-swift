import Foundation

/// Minimal Objective-C lexer.
public final class ObjectiveCLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                // Objective-C string literals
                Rule("@\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule(
                    "(?:@interface|@implementation|@end|@protocol|@class|@property|@synthesize|@dynamic|@autoreleasepool|@try|@catch|@finally|@throw|@selector|@import)\\b",
                    action: .token(.keyword)
                ),
                Rule(
                    "\\b(?:auto|break|case|char|const|continue|default|do|double|else|enum|extern|float|for|goto|if|inline|int|long|register|restrict|return|short|signed|sizeof|static|struct|switch|typedef|union|unsigned|void|volatile|while|_Bool|_Complex)\\b",
                    action: .token(.keyword)
                ),

                Rule("@[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Decorator"))),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("->|\\+\\+|--|==|!=|<=|>=|&&|\\|\\||<<|>>|[=+\\-*/%<>!&|^~?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
