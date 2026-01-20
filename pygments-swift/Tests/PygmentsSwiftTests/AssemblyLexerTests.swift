import XCTest
@testable import PygmentsSwift

final class AssemblyLexerTests: XCTestCase {
    func testAssemblyBasics() {
        let lexer = AssemblyLexer()
        let input = """
        ; comment
        .global _start
        _start:
          mov $10, %eax
          ret
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(500).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Label") && $0.value.contains(":") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) && $0.value == "10" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
