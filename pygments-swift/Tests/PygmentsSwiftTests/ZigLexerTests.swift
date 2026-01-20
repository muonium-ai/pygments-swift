import XCTest
@testable import PygmentsSwift

final class ZigLexerTests: XCTestCase {
    func testZigBasics() {
        let lexer = ZigLexer()
        let input = """
        // comment
        const std = @import("std");
        pub fn main() void { _ = std; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Decorator") && $0.value == "@import" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "fn" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
