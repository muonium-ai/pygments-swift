import Foundation

/// Minimal Objective-C++ lexer.
public final class ObjectiveCppLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("//[^\\n]*", action: .token(.comment.child("Single"))),
                Rule("/\\*.*?\\*/", options: [.dotMatchesLineSeparators], action: .token(.comment.child("Multiline"))),

                Rule("@\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'(?:[^'\\\\]|\\\\.)*'", action: .token(.string)),

                Rule("\\b(?:0x[0-9A-Fa-f]+|\\d+(?:\\.\\d+)?)\\b", action: .token(.number)),

                Rule(
                    "(?:@interface|@implementation|@end|@protocol|@class|@property|@synthesize|@dynamic|@autoreleasepool|@try|@catch|@finally|@throw|@selector|@import)\\b",
                    action: .token(.keyword)
                ),
                Rule(
                    "\\b(?:alignas|alignof|and|and_eq|asm|auto|bitand|bitor|bool|break|case|catch|char|char16_t|char32_t|class|compl|concept|const|constexpr|const_cast|continue|decltype|default|delete|do|double|dynamic_cast|else|enum|explicit|export|extern|false|float|for|friend|goto|if|inline|int|long|mutable|namespace|new|noexcept|not|not_eq|nullptr|operator|or|or_eq|private|protected|public|register|reinterpret_cast|requires|return|short|signed|sizeof|static|static_assert|static_cast|struct|switch|template|this|thread_local|throw|true|try|typedef|typeid|typename|union|unsigned|using|virtual|void|volatile|wchar_t|while|xor|xor_eq)\\b",
                    action: .token(.keyword)
                ),

                Rule("@[A-Za-z_][A-Za-z0-9_]*", action: .token(.name.child("Decorator"))),

                Rule("[A-Za-z_][A-Za-z0-9_]*", action: .token(.name)),

                Rule("::|->\\*|->|\\+\\+|--|==|!=|<=|>=|&&|\\|\\||<<|>>|[=+\\-*/%<>!&|^~?:.]", action: .token(.operator)),
                Rule("[()\\[\\]{};,]", action: .token(.punctuation)),

                Rule(".", action: .token(.text)),
            ]
        ]
    }
}
