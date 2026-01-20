import XCTest
@testable import PygmentsSwift

final class YangLexerTests: XCTestCase {
    func testYangBasics() {
        let lexer = YangLexer()
        let input = """
        module fibonacci {
          namespace "urn:example:fibonacci";
          prefix fib;
          container config { leaf n { type uint32; } }
        }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "module" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "container" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
