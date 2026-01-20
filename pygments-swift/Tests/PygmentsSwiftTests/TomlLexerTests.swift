import XCTest
@testable import PygmentsSwift

final class TomlLexerTests: XCTestCase {
    func testTomlBasics() {
        let lexer = TomlLexer()
        let input = """
        title = "Fibonacci"

        [inputs]
        small = [0, 1, 2]

        [[cases]]
        n = 10
        expected = 55
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Attribute") && $0.value == "title" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("Fibonacci") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Label") && $0.value == "inputs" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
