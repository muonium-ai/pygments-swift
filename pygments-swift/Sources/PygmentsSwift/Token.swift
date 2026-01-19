import Foundation

public struct Token: Hashable, Sendable {
    /// UTF-16 offset from the start of the input.
    public let start: Int
    public let type: TokenType
    public let value: String

    public init(start: Int, type: TokenType, value: String) {
        self.start = start
        self.type = type
        self.value = value
    }
}
