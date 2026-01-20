import XCTest
@testable import PygmentsSwift

final class MakefileLexerTests: XCTestCase {
    func testMakefileBasics() {
        let lexer = MakefileLexer()
        let input = """
        # comment
        CONFIG ?= debug

        all: build test

        build:
        \techo \"building (CONFIG=$(CONFIG))\"
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Attribute") && $0.value == "CONFIG" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Function") && $0.value == "all" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("echo") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
