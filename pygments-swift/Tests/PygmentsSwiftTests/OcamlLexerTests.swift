import XCTest
@testable import PygmentsSwift

final class OcamlLexerTests: XCTestCase {
    func testOcamlBasics() {
        let lexer = OcamlLexer()
        let input = """
        (* comment *)
        let rec fib n = if n < 2 then n else fib (n-1) + fib (n-2)
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(450).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "let" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "rec" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
