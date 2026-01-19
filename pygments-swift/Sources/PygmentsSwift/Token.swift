import Foundation

public struct Token: Hashable, Sendable {
    /// UTF-16 offset from the start of the input.
    ///
    /// This matches `NSRange`/`NSString` indexing and is convenient for regex-based lexing.
    public let start: Int

    /// Unicode-scalar offset (codepoint index) from the start of the input.
    ///
    /// Python Pygments token indices are based on Python string indices, which align
    /// with Unicode codepoints (not UTF-16 code units). Use this for parity checks.
    public let startScalar: Int
    public let type: TokenType
    public let value: String

    public init(start: Int, startScalar: Int, type: TokenType, value: String) {
        self.start = start
        self.startScalar = startScalar
        self.type = type
        self.value = value
    }
}
