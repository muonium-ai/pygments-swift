import Foundation

public struct TokenType: Hashable, Sendable, CustomStringConvertible {
    public let path: [String]

    public init(_ path: [String] = []) {
        self.path = path
    }

    public var parent: TokenType? {
        guard !path.isEmpty else { return nil }
        return TokenType(Array(path.dropLast()))
    }

    public func child(_ name: String) -> TokenType {
        TokenType(path + [name])
    }

    public func isSubtype(of other: TokenType) -> Bool {
        if other.path.isEmpty { return true }
        if other.path.count > path.count { return false }
        return Array(path.prefix(other.path.count)) == other.path
    }

    public var description: String {
        if path.isEmpty { return "Token" }
        return "Token." + path.joined(separator: ".")
    }

    /// Parses strings like "String.Double" or "Token.Literal.Number".
    public static func fromString(_ s: String) -> TokenType {
        let trimmed = s.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return .token }

        let parts = trimmed.split(separator: ".").map(String.init)
        let withoutLeadingToken: [String]
        if parts.first == "Token" {
            withoutLeadingToken = Array(parts.dropFirst())
        } else {
            withoutLeadingToken = parts
        }

        return TokenType(withoutLeadingToken)
    }
}

// MARK: - Standard-ish tokens (subset)

public extension TokenType {
    static let token = TokenType()

    static let text = token.child("Text")
    static let whitespace = text.child("Whitespace")

    static let escape = token.child("Escape")
    static let error = token.child("Error")
    static let other = token.child("Other")

    static let keyword = token.child("Keyword")
    static let name = token.child("Name")
    static let literal = token.child("Literal")
    static let string = literal.child("String")
    static let number = literal.child("Number")

    static let punctuation = token.child("Punctuation")
    static let `operator` = token.child("Operator")
    static let comment = token.child("Comment")
    static let generic = token.child("Generic")
}
