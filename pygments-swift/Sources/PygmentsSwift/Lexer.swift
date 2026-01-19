import Foundation

public struct LexerOptions: Sendable {
    public var stripnl: Bool
    public var stripall: Bool
    public var ensurenl: Bool
    public var tabsize: Int

    public init(stripnl: Bool = true, stripall: Bool = false, ensurenl: Bool = true, tabsize: Int = 0) {
        self.stripnl = stripnl
        self.stripall = stripall
        self.ensurenl = ensurenl
        self.tabsize = tabsize
    }
}

public protocol Lexer {
    func getTokens(_ text: String) -> [Token]
}

open class LexerBase: Lexer {
    public let options: LexerOptions

    public init(options: LexerOptions = .init()) {
        self.options = options
    }

    open func getTokens(_ text: String) -> [Token] {
        fatalError("Subclasses must override")
    }

    public func preprocess(_ text: String) -> String {
        var s = text
        // Normalize newlines
        s = s.replacingOccurrences(of: "\r\n", with: "\n")
        s = s.replacingOccurrences(of: "\r", with: "\n")

        if options.stripall {
            s = s.trimmingCharacters(in: .whitespacesAndNewlines)
        } else if options.stripnl {
            s = s.trimmingCharacters(in: CharacterSet(charactersIn: "\n"))
        }

        if options.tabsize > 0 {
            s = s.replacingOccurrences(of: "\t", with: String(repeating: " ", count: options.tabsize))
        }

        if options.ensurenl, !s.hasSuffix("\n") {
            s.append("\n")
        }

        return s
    }
}
